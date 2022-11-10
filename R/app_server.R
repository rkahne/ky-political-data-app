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

db <- pool::dbPool(odbc::odbc(),
                .connection_string = "Driver={PostgreSQL ANSI};",
                Database = "kypolitics")

visit <- tibble::tibble(app = 'kypoliticaldata.com',
                 time = Sys.time())

pool::dbWriteTable(db, 'site_visit_history', visit, temporary = FALSE, append = TRUE, indexes = list('app'))

onStop(function() {
  pool::poolClose(db)
})

ky_counties <- readr::read_rds('/srv/data/shapefiles/counties.rds')
ky_precincts <- readr::read_rds('/srv/data/shapefiles/precincts.rds')
ky_precincts_22 <- readr::read_rds('/srv/data/shapefiles/ky_shp_new_lou.rds')
house_plan <- readr::read_rds('/srv/data/shapefiles/gop_plan_22_shp_BILL_VERSION.rds')
house_plan_hcs <- readr::read_rds('/srv/data/shapefiles/gop_plan_22_shp_hcs.rds')
house_plan_dem <- readr::read_rds('/srv/data/shapefiles/dem_plan_22_shp.rds')
house_2014 <- readr::read_rds('/srv/data/shapefiles/house_map_2014.rds')
cong_plan <- readr::read_rds('/srv/data/shapefiles/gop_plan_cong_22.rds')
sen_plan <- readr::read_rds('/srv/data/shapefiles/gop_plan_sen_22.rds')
sen_2014 <- readr::read_rds('/srv/data/shapefiles/senate_map_2014.rds')

app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_elections_server("elections_ui_1")
  mod_legislation_server("legislation_ui_1")
  mod_redistricting_server("redistricting_ui_1")
  mod_election_22_server("election_22_ui_1")
  mod_primary_22_server("primary_22_ui_1")
  mod_fundraising_server("fundraising_ui_1")
}
