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
ky_precicnts <- readr::read_rds('/srv/data/shapefiles/precincts.rds')

app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_elections_server("elections_ui_1")
  mod_legislation_server("legislation_ui_1")
  mod_redistricting_server("redistricting_ui_1")
}
