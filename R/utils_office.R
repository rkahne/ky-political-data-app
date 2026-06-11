#' Shared office-classification helpers
#'
#' Single source of truth for turning a messy race / office string into a
#' canonical office classification. Used by:
#'   - data-raw/build_race_lookup.R  (offline: builds the race_lookup crosswalk)
#'   - mod_elections.R               (groups the election picker via race_lookup)
#'   - mod_fundraising.R             (groups the office picker at runtime)
#'
#' This file is dependency-light on purpose (only dplyr + stringr) so the
#' offline generator can `source()` it without loading the whole package.
#'
#' @noRd
NULL

# Display order and friendly labels for the office-level groups (optgroup headers)
office_level_order <- c('federal', 'state_exec', 'state_leg',
                        'county', 'judicial', 'ballot', 'local')
office_level_label <- c(federal = 'Federal', state_exec = 'Statewide (Executive)',
                        state_leg = 'State Legislature', county = 'County Offices',
                        judicial = 'Judicial', ballot = 'Ballot Questions',
                        local = 'Local')

#' Classify race / office strings into a canonical office taxonomy
#'
#' Priority-ordered (first matching branch wins): more specific / local offices
#' (e.g. "County Judge/Executive", "Circuit Court Clerk") are claimed before
#' broader judicial / statewide patterns.
#'
#' @param strings Character vector of raw race or office_sought strings.
#' @return A tibble with one row per input: office_level, office (canonical),
#'   district (number where applicable, else NA), jurisdiction ("City of X" for
#'   local races, else NA).
#' @noRd
classify_office <- function(strings){
  s <- stringr::str_to_lower(strings)

  office <- dplyr::case_when(
    stringr::str_detect(s, "president")                                                                    ~ "President",
    stringr::str_detect(s, "united states senat|u\\.?s\\.? senat|\\bus senat")                             ~ "U.S. Senate",
    stringr::str_detect(s, "representative in congress|congressional district|\\bcongress\\b|u\\.?s\\.? representative|\\bus representative|u\\.?s\\.? house|\\bus house|united states house") ~ "U.S. House",
    stringr::str_detect(s, "governor")                                                                     ~ "Governor",
    stringr::str_detect(s, "attorney general")                                                             ~ "Attorney General",
    stringr::str_detect(s, "secretary of state")                                                           ~ "Secretary of State",
    stringr::str_detect(s, "auditor")                                                                      ~ "Auditor of Public Accounts",
    stringr::str_detect(s, "commissioner of agriculture")                                                  ~ "Commissioner of Agriculture",
    stringr::str_detect(s, "superintendent of public instruction")                                         ~ "Superintendent of Public Instruction",
    stringr::str_detect(s, "state treasurer|\\btreasurer\\b")                                              ~ "State Treasurer",
    stringr::str_detect(s, "state senat")                                                                  ~ "State Senate",
    stringr::str_detect(s, "state representative")                                                         ~ "State House",
    # County row offices
    stringr::str_detect(s, "circuit court clerk|circuit clerk")                                            ~ "Circuit Court Clerk",
    stringr::str_detect(s, "county clerk")                                                                 ~ "County Clerk",
    stringr::str_detect(s, "county judge")                                                                 ~ "County Judge/Executive",
    stringr::str_detect(s, "county attorney")                                                              ~ "County Attorney",
    stringr::str_detect(s, "county commissioner")                                                          ~ "County Commissioner",
    stringr::str_detect(s, "\\bsheriff\\b")                                                                ~ "Sheriff",
    stringr::str_detect(s, "\\bjailer\\b")                                                                 ~ "Jailer",
    stringr::str_detect(s, "\\bcoroner\\b")                                                                ~ "Coroner",
    stringr::str_detect(s, "property valuation|\\bpva\\b")                                                 ~ "Property Valuation Administrator",
    stringr::str_detect(s, "\\bsurveyor\\b")                                                               ~ "County Surveyor",
    stringr::str_detect(s, "\\bcons?table\\b")                                                             ~ "Constable",  # also catches 'contable' typo
    stringr::str_detect(s, "magistrate")                                                                   ~ "Magistrate",
    stringr::str_detect(s, "justice of the peace")                                                         ~ "Justice of the Peace",
    stringr::str_detect(s, "fiscal court")                                                                 ~ "Fiscal Court",
    # Judicial
    stringr::str_detect(s, "supreme court")                                                                ~ "Supreme Court Justice",
    stringr::str_detect(s, "court of appeals")                                                             ~ "Court of Appeals Judge",
    stringr::str_detect(s, "commonwealth.?s? attorney")                                                    ~ "Commonwealth's Attorney",
    stringr::str_detect(s, "family court")                                                                 ~ "Family Court Judge",
    stringr::str_detect(s, "circuit judge|circuit court judge")                                            ~ "Circuit Judge",
    stringr::str_detect(s, "district judge|district court judge")                                          ~ "District Judge",
    stringr::str_detect(s, "\\bjustice\\b|\\bjudge\\b")                                                    ~ "Judge",
    # Ballot measures
    stringr::str_detect(s, "constitutional amendment")                                                     ~ "Constitutional Amendment",
    stringr::str_detect(s, "\\?|amendment|are you in favor|ad valorem|local option|referendum|\\bquestion\\b|annexation|\\bannex\\b|cannabis") ~ "Ballot Question",
    # Local
    stringr::str_detect(s, "\\bmayor\\b")                                                                  ~ "Mayor",
    stringr::str_detect(s, "city clerk")                                                                   ~ "City Clerk",
    stringr::str_detect(s, "commission")                                                                   ~ "City Commission",
    stringr::str_detect(s, "council|legislative body")                                                     ~ "City Council",
    stringr::str_detect(s, "school board|board of education|\\bschool\\b|\\bboe\\b")                        ~ "School Board",
    stringr::str_detect(s, "soil and water|soil & water|soil conservation")                                ~ "Soil & Water Conservation",
    stringr::str_detect(s, "\\bindependent\\b")                                                            ~ "School Board",  # independent school districts
    TRUE                                                                                                   ~ "Other Local"
  )

  level_map <- c(
    "President" = "federal", "U.S. Senate" = "federal", "U.S. House" = "federal",
    "Governor" = "state_exec", "Attorney General" = "state_exec",
    "Secretary of State" = "state_exec", "Auditor of Public Accounts" = "state_exec",
    "Commissioner of Agriculture" = "state_exec",
    "Superintendent of Public Instruction" = "state_exec", "State Treasurer" = "state_exec",
    "State Senate" = "state_leg", "State House" = "state_leg",
    "Circuit Court Clerk" = "county", "County Clerk" = "county",
    "County Judge/Executive" = "county", "County Attorney" = "county",
    "County Commissioner" = "county",
    "Sheriff" = "county", "Jailer" = "county", "Coroner" = "county",
    "Property Valuation Administrator" = "county", "County Surveyor" = "county",
    "Constable" = "county", "Magistrate" = "county", "Justice of the Peace" = "county",
    "Fiscal Court" = "county",
    "Supreme Court Justice" = "judicial", "Court of Appeals Judge" = "judicial",
    "Commonwealth's Attorney" = "judicial", "Family Court Judge" = "judicial",
    "Circuit Judge" = "judicial", "District Judge" = "judicial", "Judge" = "judicial",
    "Constitutional Amendment" = "ballot", "Ballot Question" = "ballot",
    "Mayor" = "local", "City Clerk" = "local", "City Commission" = "local",
    "City Council" = "local", "School Board" = "local",
    "Soil & Water Conservation" = "local", "Other Local" = "local"
  )
  office_level <- unname(level_map[office])

  # District / circuit number (first ordinal+keyword in the string).
  district <- stringr::str_match(
    s, "(\\d+)\\s*(?:st|nd|rd|th)\\s+(?:congressional|senatorial|representative|judicial|magisterial|educational|circuit|appellate|supreme court|district|division)"
  )[, 2]
  has_district <- office %in% c(
    "U.S. House", "State Senate", "State House", "Constable", "Magistrate",
    "Supreme Court Justice", "Court of Appeals Judge", "Commonwealth's Attorney",
    "Family Court Judge", "Circuit Judge", "District Judge", "Judge"
  )
  district <- dplyr::if_else(has_district, district, NA_character_)

  # Jurisdiction for local races: best-effort "City of X" capture.
  jurisdiction <- stringr::str_match(strings, "(City of [A-Za-z .'-]+?)(?:\\s+\\(|$)")[, 2]
  jurisdiction <- dplyr::if_else(office_level == "local", stringr::str_trim(jurisdiction), NA_character_)

  dplyr::tibble(
    office_level = office_level,
    office = office,
    district = district,
    jurisdiction = jurisdiction
  )
}

#' Build a grouped, sorted named list for pickerInput `choices`
#'
#' Groups by office_level (friendly headers, fixed display order) and sorts
#' within each group by canonical office, then numeric district, then the
#' displayed value. Empty groups are dropped. Values missing an office_level
#' fall back to the "Local" group.
#'
#' @param values Character vector of the selectable strings (what the picker returns).
#' @param office_level Character vector (same length) of office_level codes.
#' @param office Character vector for secondary sort; defaults to `values`.
#' @param district Optional vector for numeric tertiary sort within an office.
#' @return Named list of character vectors, in office-level display order.
#' @noRd
group_office_choices <- function(values, office_level, office = values, district = NA){
  df <- dplyr::tibble(
      value = values,
      office_level = dplyr::if_else(is.na(office_level), 'local', office_level),
      office = office,
      district_num = suppressWarnings(as.integer(district))
    ) %>%
    dplyr::mutate(office_level = factor(office_level, levels = office_level_order)) %>%
    dplyr::arrange(office_level, office, district_num, stringr::str_to_lower(value))

  out <- lapply(office_level_order, function(lv) df$value[df$office_level == lv])
  names(out) <- unname(office_level_label[office_level_order])
  out[lengths(out) > 0]
}
