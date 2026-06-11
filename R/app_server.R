#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import purrr
#' @import tibble
#' @import stringr
#' @import forcats
#' @import reactable
#' @import plotly
#' @import scales
#' @import DBI
#' @import leaflet
#' @import sf
#' @import shinycssloaders
#' @import htmltools
#' @import pool
#' @import lubridate
#' @noRd

db <- create_db_pool()

visit <- tibble::tibble(app = 'kypoliticaldata.com',
                 time = Sys.time())

pool::dbWriteTable(db, 'site_visit_history', visit, temporary = FALSE, append = TRUE, indexes = list('app'))

onStop(function() {
  pool::poolClose(db)
})

# Pre-load static lookup tables once at startup (avoids repeated DB queries)
fips_lookup <- dplyr::tbl(db, 'fips') %>% dplyr::collect()
zip_county_lookup <- dplyr::tbl(db, 'ky_zip_county') %>% dplyr::collect() %>%
  dplyr::mutate(zip_code = as.character(zip_code))

# Load spatial data from PostGIS using RPostgres (which understands geometry natively)
read_sf_table <- function(table_name) {
  sf_con <- RPostgres::Postgres()
  db_name <- get_db_config("db_name")
  db_host <- get_db_config("db_host")
  db_port <- get_db_config("db_port")
  use_keyring <- get_db_config("db_use_keyring")

  if (isTRUE(use_keyring)) {
    db_user <- keyring::key_list("kypolitics_db")$username[1]
    db_pass <- keyring::key_get("kypolitics_db", username = db_user)
    con <- DBI::dbConnect(sf_con, dbname = db_name, host = db_host, port = db_port,
                          user = db_user, password = db_pass)
  } else {
    con <- DBI::dbConnect(sf_con, dbname = db_name)
  }
  on.exit(DBI::dbDisconnect(con))
  sf::st_read(con, table_name)
}

ky_counties <- read_sf_table("counties")
ky_precincts <- read_sf_table("precincts")
ky_precincts_22 <- read_sf_table("precincts_22") %>% mutate(VTDST = paste0('00',PRECINCT))
ky_precincts_23 <- read_sf_table("precincts_24") %>% mutate(VTDST = paste0('00',PRECINCT))
ky_precincts_24 <- read_sf_table("precincts_general_24") %>% mutate(VTDST = paste0('00',PRECINCT))
house_plan <- read_sf_table("house_plan")
house_plan_hcs <- read_sf_table("house_plan_hcs")
house_plan_dem <- read_sf_table("house_plan_dem")
house_2014 <- read_sf_table("house_2014")
cong_plan <- read_sf_table("cong_plan")
sen_plan <- read_sf_table("sen_plan")
sen_2014 <- read_sf_table("sen_2014")

app_server <- function( input, output, session ) {
  output$kpd_bar <- renderImage({
    logo <- get_db_config("logo_path")
    if (nchar(logo) > 0 && file.exists(logo)) {
      list(src = logo,
           alt = 'Kentucky Political Data',
           width = 200,
           height = 175)
    } else {
      # Dev mode: no logo available, use a placeholder
      list(src = "",
           alt = 'Kentucky Political Data',
           width = 200,
           height = 175)
    }
  }, deleteFile = FALSE)
  
  # Your application server logic 
  mod_elections_server("elections_ui_1")
  mod_legislation_server("legislation_ui_1")
  mod_redistricting_server("redistricting_ui_1")
  mod_election_22_server("election_22_ui_1")
  mod_primary_22_server("primary_22_ui_1")
  mod_fundraising_server("fundraising_ui_1")
}
