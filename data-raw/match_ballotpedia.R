# Match Ballotpedia 2026 candidate parties (data-raw/ballotpedia_2026_raw.csv,
# produced by scrape_ballotpedia.R) to the unresolved 2026 primary candidates and
# APPEND candidate-level resolutions to data-raw/primary_party_manual.csv.
# Then re-run: Rscript data-raw/build_primary_party_lookup.R --write
#
# Usage:
#   Rscript data-raw/match_ballotpedia.R          # report only
#   Rscript data-raw/match_ballotpedia.R --write  # append matches to manual.csv

suppressMessages({ library(dplyr); library(stringr); library(readr); library(tibble); library(tidyr) })
source("R/utils_office.R")          # classify_office()
do_write <- "--write" %in% commandArgs(trailingOnly = TRUE)

norm_name <- function(x){
  x <- str_replace(x, "/.*$", "")
  x <- str_replace_all(x, '"[^"]*"', " ")
  x <- str_replace_all(x, "[.,]", " ")
  str_to_upper(str_squish(x))
}
firstlast <- function(nk){
  toks <- str_split(nk, " ")
  vapply(toks, function(t) if (length(t) >= 2) paste(t[1], t[length(t)]) else t[[1]], character(1))
}
# Ballotpedia office string -> canonical office matching classify_office()$office.
bp_canon <- function(o){
  ol <- str_to_lower(o)
  out <- classify_office(o)$office
  out <- ifelse(str_detect(ol, "u\\.?s\\.? senate|united states senate"), "U.S. Senate", out)
  out <- ifelse(str_detect(ol, "u\\.?s\\.? house|united states house|congress"), "U.S. House", out)
  out <- ifelse(str_detect(ol, "state senate|kentucky senate"), "State Senate", out)
  out <- ifelse(str_detect(ol, "house of representatives|state house|state representative"), "State House", out)
  out
}

bp <- read_csv("data-raw/ballotpedia_2026_raw.csv", show_col_types = FALSE) %>%
  mutate(office = bp_canon(office_bp), nkey = norm_name(candidate)) %>%
  group_by(office, nkey) %>%                              # drop name+office tagged both parties
  summarise(party = if (n_distinct(party) == 1) first(party) else NA_character_, .groups = "drop") %>%
  filter(!is.na(party))
cat("Ballotpedia (office, candidate) party tags:", nrow(bp), "\n")

unr <- read_csv("data-raw/primary_party_unresolved.csv", show_col_types = FALSE) %>%
  filter(year == 2026) %>%
  select(year, race, candidate, office) %>%
  mutate(nkey = norm_name(candidate))

exact <- unr %>% inner_join(bp, by = c("office", "nkey"))
left  <- unr %>% anti_join(exact, by = c("race", "candidate"))
bp_fl <- bp %>% mutate(fl = firstlast(nkey)) %>% group_by(office, fl) %>%
  summarise(party = if (n_distinct(party) == 1) first(party) else NA, .groups = "drop") %>% filter(!is.na(party))
loose <- left %>% mutate(fl = firstlast(nkey)) %>% inner_join(bp_fl, by = c("office", "fl"))

matched <- bind_rows(exact %>% select(year, race, candidate, party),
                     loose %>% select(year, race, candidate, party)) %>% distinct()

cat("\nUnresolved 2026:", nrow(unr), "| matched:", nrow(matched),
    sprintf("(%.0f%%)\n", 100 * nrow(matched) / nrow(unr)))
cat("\nMatched by office:\n")
print(matched %>% left_join(unr %>% select(race, candidate, office), by = c("race","candidate")) %>%
        count(office, sort = TRUE), n = 30)
cat("\nStill unresolved by office:\n")
print(unr %>% anti_join(matched, by = c("race","candidate")) %>% count(office, sort = TRUE), n = 30)

if (do_write) {
  man <- read_csv("data-raw/primary_party_manual.csv", show_col_types = FALSE) %>%
    mutate(candidate = ifelse(is.na(candidate), "", as.character(candidate)))
  out <- bind_rows(man, matched %>% transmute(year, race, candidate, party)) %>%
    group_by(year, race, candidate) %>%
    arrange(desc(nchar(coalesce(party, "")))) %>% slice(1) %>% ungroup() %>%
    arrange(year, race, candidate)
  write_csv(out, "data-raw/primary_party_manual.csv")
  cat("\nWrote", nrow(matched), "resolutions into data-raw/primary_party_manual.csv\n")
} else {
  cat("\nReport only. Re-run with --write to append to manual.csv.\n")
}
