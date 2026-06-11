# Merge a filled-in primary_party_todo.csv (hand-entered party column) into
# data-raw/primary_party_manual.csv, normalizing party labels to REP/DEM/IND.
# Then re-run: Rscript data-raw/build_primary_party_lookup.R --write
suppressMessages({ library(dplyr); library(stringr); library(readr) })

norm_party <- function(p){
  p <- str_to_upper(str_trim(as.character(p)))
  case_when(
    p %in% c("REP","REPUBLICAN","R")             ~ "REP",
    p %in% c("DEM","DEMOCRAT","DEMOCRATIC","D")   ~ "DEM",
    p %in% c("IND","INDEPENDENT","I")             ~ "IND",
    TRUE                                          ~ NA_character_  # blank / unrecognized -> leave unresolved
  )
}

todo <- read_csv("data-raw/primary_party_todo.csv", show_col_types = FALSE) %>%
  mutate(party = norm_party(party)) %>%
  filter(!is.na(party)) %>%
  transmute(year, race, candidate, party)
cat("filled rows to merge:", nrow(todo), "\n"); print(table(todo$party))

man <- read_csv("data-raw/primary_party_manual.csv", show_col_types = FALSE) %>%
  mutate(candidate = ifelse(is.na(candidate), "", as.character(candidate)))

out <- bind_rows(man, todo) %>%
  group_by(year, race, candidate) %>%
  arrange(desc(nchar(coalesce(party, "")))) %>% slice(1) %>% ungroup() %>%
  arrange(year, race, candidate)
write_csv(out, "data-raw/primary_party_manual.csv")
cat("\nmanual.csv now has", nrow(out), "rows (",
    sum(out$party != "" & !is.na(out$party)), "with a party ).\n")
