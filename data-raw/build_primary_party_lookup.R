# Build the `primary_party_lookup` crosswalk table  (grain: year, race, candidate).
#
# KY partisan primaries are SEPARATE elections -- Republicans and Democrats run
# in distinct contests. The scraped election_data, however, carries no usable
# party for primaries (party = 'primary') and sometimes merges both parties'
# candidates under one race string (e.g. 2023 "GOVERNOR and LIEUTENANT GOVERNOR"
# holds Beshear AND Cameron). This table assigns a party to each primary
# CANDIDATE so the app can split a race into its separate partisan elections.
# election_data itself is left untouched.
#
# Signal precedence (first wins), per candidate:
#   1. manual candidate  -- data-raw/primary_party_manual.csv row WITH a candidate
#   2. manual race       -- manual.csv row with BLANK candidate (applies to whole race)
#   3. suffix            -- race name ends in '- DEMOCRATIC' / '- REPUBLICAN' (2018-2020)
#   4. general match     -- candidate matches a same-year GENERAL candidate
#                           (office+district+normalized name) whose party is REP/DEM
#   5. race-inferred     -- the race is single-party (all matched candidates agree),
#                           so unmatched candidates in it inherit that party
# Nonpartisan office levels (judicial, local, ballot) -> 'nonpartisan' (party NA).
# Partisan candidates we cannot resolve -> 'unresolved' and exported for hand-fill.
#
# Usage:
#   Rscript data-raw/build_primary_party_lookup.R          # DRY RUN: report + unresolved CSV
#   Rscript data-raw/build_primary_party_lookup.R --write  # also write the table to the DB
#
# Requires the dev SSH tunnel (Postgres on 127.0.0.1:5433) and the race_lookup table.

suppressMessages({
  library(DBI); library(RPostgres); library(dplyr); library(stringr); library(tibble); library(tidyr)
})

write_table    <- "--write" %in% commandArgs(trailingOnly = TRUE)
manual_csv     <- file.path("data-raw", "primary_party_manual.csv")
unresolved_csv <- file.path("data-raw", "primary_party_unresolved.csv")
partisan_levels <- c("federal", "state_exec", "state_leg", "county")

u  <- keyring::key_list("kypolitics_db")$username[1]
pw <- keyring::key_get("kypolitics_db", username = u)
con <- dbConnect(Postgres(), dbname = "kypolitics", host = "127.0.0.1", port = 5433, user = u, password = pw)
on.exit(dbDisconnect(con), add = TRUE)
q <- function(sql) as_tibble(dbGetQuery(con, sql))

# Normalize a candidate name to a match key: drop running mate after '/', drop
# quoted nicknames, drop punctuation, uppercase, collapse whitespace.
norm_name <- function(x) {
  x <- str_replace(x, "/.*$", "")
  x <- str_replace_all(x, '"[^"]*"', " ")
  x <- str_replace_all(x, "[.,]", " ")
  str_to_upper(str_squish(x))
}
suffix_party <- function(race) {
  up <- str_to_upper(race)
  case_when(str_detect(up, "DEMOCRAT") ~ "DEM",
            str_detect(up, "REPUBLIC") ~ "REP",
            TRUE ~ NA_character_)
}

rl <- q("SELECT race, office, office_level, district FROM race_lookup")

prim <- q("SELECT DISTINCT year, race, candidate FROM election_data WHERE election='primary'") %>%
  left_join(rl, by = "race") %>%
  mutate(nkey = norm_name(candidate))

gen_key <- q("SELECT DISTINCT year, race, candidate, party FROM election_data
              WHERE election='general' AND party IN ('REP','DEM')") %>%
  left_join(rl, by = "race") %>%
  mutate(nkey = norm_name(candidate)) %>%
  distinct(year, office, district, nkey, party)

# Per-candidate general match (only when the name maps to a single party).
cand_match <- prim %>%
  inner_join(gen_key, by = c("year", "office", "district", "nkey")) %>%
  group_by(year, race, candidate) %>%
  summarise(general_cand = if (n_distinct(party) == 1) first(party) else NA_character_, .groups = "drop")

# Race-level signal: is the race single-party among its matched candidates?
race_sig <- cand_match %>%
  filter(!is.na(general_cand)) %>%
  group_by(year, race) %>%
  summarise(race_party = if (n_distinct(general_cand) == 1) first(general_cand) else NA_character_,
            .groups = "drop")

# Manual overrides (candidate-level and race-level)
manual <- if (file.exists(manual_csv)) {
  readr::read_csv(manual_csv, show_col_types = FALSE) %>%
    mutate(year = as.numeric(year), race = as.character(race),
           candidate = ifelse(is.na(candidate), "", as.character(candidate)),
           party = str_to_upper(str_trim(as.character(party)))) %>%
    filter(!is.na(party), party != "")
} else tibble(year = numeric(), race = character(), candidate = character(), party = character())
manual_cand <- manual %>% filter(candidate != "") %>% distinct(year, race, candidate, mc = party)
manual_race <- manual %>% filter(candidate == "") %>% distinct(year, race, mr = party)

lk <- prim %>%
  distinct(year, race, candidate, office, office_level) %>%
  left_join(manual_cand, by = c("year", "race", "candidate")) %>%
  left_join(manual_race, by = c("year", "race")) %>%
  left_join(cand_match,  by = c("year", "race", "candidate")) %>%
  left_join(race_sig,    by = c("year", "race")) %>%
  mutate(
    suffix      = suffix_party(race),
    nonpartisan = !office_level %in% partisan_levels,
    party = case_when(
      !is.na(mc)           ~ mc,
      !is.na(mr)           ~ mr,
      nonpartisan          ~ NA_character_,
      !is.na(suffix)       ~ suffix,
      !is.na(general_cand) ~ general_cand,
      !is.na(race_party)   ~ race_party,
      TRUE                 ~ NA_character_
    ),
    source = case_when(
      !is.na(mc)           ~ "manual_cand",
      !is.na(mr)           ~ "manual_race",
      nonpartisan          ~ "nonpartisan",
      !is.na(suffix)       ~ "suffix",
      !is.na(general_cand) ~ "general",
      !is.na(race_party)   ~ "race_inferred",
      TRUE                 ~ "unresolved"
    )
  ) %>%
  select(year, race, candidate, office, office_level, party, source)

# ---- report ----
cat("Primary (year, race, candidate) rows:", nrow(lk), "\n\n")
cat("Coverage by year x source:\n")
print(lk %>% count(year, source) %>% pivot_wider(names_from = source, values_from = n, values_fill = 0), n = 40)
cat("\nPARTISAN candidates resolved vs needing hand-fill (per year):\n")
print(lk %>% filter(office_level %in% partisan_levels) %>%
        mutate(resolved = !is.na(party)) %>%
        count(year, resolved) %>%
        pivot_wider(names_from = resolved, values_from = n, values_fill = 0), n = 40)
cat("\nDistinct partisan primary elections (year, race, party) produced:\n")
print(lk %>% filter(!is.na(party)) %>% distinct(year, race, party) %>% count(year, name = "elections"), n = 40)

# ---- export unresolved partisan candidates for hand-fill ----
unresolved <- lk %>%
  filter(office_level %in% partisan_levels, is.na(party)) %>%
  arrange(year, office, race, candidate) %>%
  transmute(year, race, candidate, office, party = "", note = source)
readr::write_csv(unresolved, unresolved_csv)
cat("\nWrote", nrow(unresolved), "unresolved partisan candidates to", unresolved_csv, "\n")

# Seed the hand-edit worksheet once (never overwrite the user's edits). Race-level
# rows (blank candidate) for races where NO candidate resolved; candidate-level
# rows for the leftover candidates in partially-resolved (combined) races.
if (!file.exists(manual_csv)) {
  unresolved_races <- lk %>% group_by(year, race) %>%
    summarise(any_resolved = any(!is.na(party)),
              partisan = any(office_level %in% partisan_levels), .groups = "drop") %>%
    filter(partisan)
  seed_race <- unresolved_races %>% filter(!any_resolved) %>%
    transmute(year, race, candidate = "", party = "")
  seed_cand <- lk %>%
    semi_join(unresolved_races %>% filter(any_resolved), by = c("year", "race")) %>%
    filter(office_level %in% partisan_levels, is.na(party)) %>%
    transmute(year, race, candidate, party = "")
  readr::write_csv(bind_rows(seed_race, seed_cand) %>% arrange(year, race, candidate), manual_csv)
  cat("Seeded hand-edit worksheet at", manual_csv,
      "-- blank-candidate rows set the whole race; fill 'party' with DEM/REP, re-run with --write.\n")
}

# ---- write ----
if (write_table) {
  dbWriteTable(con, "primary_party_lookup", as.data.frame(lk), overwrite = TRUE, row.names = FALSE)
  dbExecute(con, 'CREATE INDEX IF NOT EXISTS idx_ppl_yrc ON primary_party_lookup ("year","race","candidate")')
  cat("\nWROTE primary_party_lookup (", nrow(lk), "rows ) to the database.\n")
} else {
  cat("\nDRY RUN -- table not written. Re-run with --write to create primary_party_lookup.\n")
}
