# Build the `race_lookup` crosswalk table.
#
# Maps every distinct raw `race` string in `election_data` to a canonical
# office classification, so the app (and any analysis) can group / sort / compare
# races consistently across years without re-parsing messy strings at runtime.
#
# The crosswalk keys on the raw `race` string exactly as stored in
# `election_data` (the join key). `election_data` itself is left untouched.
#
# Columns written:
#   race          raw string, exactly as in election_data (PRIMARY KEY / join key)
#   office_level  federal | state_exec | state_leg | county | judicial | local | ballot
#   office        canonical office name, consistent across years
#   district      district / circuit number where applicable, else NA
#   jurisdiction  city/place for local races, else NA
#
# Usage:
#   Rscript data-raw/build_race_lookup.R          # DRY RUN: print summary, write nothing
#   Rscript data-raw/build_race_lookup.R --write  # classify AND write race_lookup to the DB
#
# Requires the dev SSH tunnel to be open (Postgres on 127.0.0.1:5433).

suppressMessages({
  library(DBI); library(RPostgres); library(dplyr); library(stringr); library(tibble)
})

write_table <- "--write" %in% commandArgs(trailingOnly = TRUE)

# ---- connect -----------------------------------------------------------------
u  <- keyring::key_list("kypolitics_db")$username[1]
pw <- keyring::key_get("kypolitics_db", username = u)
con <- dbConnect(Postgres(), dbname = "kypolitics",
                 host = "127.0.0.1", port = 5433, user = u, password = pw)
on.exit(dbDisconnect(con), add = TRUE)

races <- dbGetQuery(con, "SELECT DISTINCT race FROM election_data")$race

# ---- classify ----------------------------------------------------------------
# Classification logic lives in R/utils_office.R so the app and this generator
# stay in lock-step. Sourced directly (no load_all) since that file is
# dependency-light (dplyr + stringr only).
source(file.path("R", "utils_office.R"))

lk <- dplyr::bind_cols(tibble(race = races), classify_office(races))

# ---- report ------------------------------------------------------------------
cat("Distinct races:", nrow(lk), "\n\n")
cat("By office_level:\n")
print(lk %>% count(office_level) %>% arrange(desc(n)), n = 50)
cat("\nTop offices:\n")
print(lk %>% count(office, office_level) %>% arrange(desc(n)), n = 60)
cat("\nSample 'Other Local' (catch-all to eyeball):\n")
print(lk %>% filter(office == "Other Local") %>% slice_head(n = 25) %>% pull(race))
cat("\nDistrict extraction spot-check:\n")
print(lk %>% filter(!is.na(district)) %>% slice_sample(n = 15) %>% select(race, office, district), n = 15)

# ---- write -------------------------------------------------------------------
if (write_table) {
  dbWriteTable(con, "race_lookup", as.data.frame(lk), overwrite = TRUE, row.names = FALSE)
  dbExecute(con, 'CREATE INDEX IF NOT EXISTS idx_race_lookup_race ON race_lookup ("race")')
  cat("\nWROTE race_lookup (", nrow(lk), "rows ) to the database.\n")
} else {
  cat("\nDRY RUN -- nothing written. Re-run with --write to create race_lookup.\n")
}
