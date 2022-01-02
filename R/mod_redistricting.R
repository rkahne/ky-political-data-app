#' redistricting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_redistricting_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName = 'redistricting', 
          fluidRow(
            selectInput(ns('yr'), 'Select Year', c(2014, 2022), 2022),
            box(width = 12, 'Please be patient while the map loads', leafletOutput(ns('house_map'), height = 800) %>% withSpinner())
          )
 
  )
}
    
#' redistricting Server Functions
#'
#' @noRd 
mod_redistricting_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    house_14 <- read_rds('/srv/data/h_14_leaf.rds')
    house_22 <- read_rds('/srv/data/h_22_leaf.rds')
    output$house_map <- renderLeaflet({
      if(input$yr == 2014){
        house_14
      }else{
        house_22
      }
    })
 
  })
}
    
## To be copied in the UI
# mod_redistricting_ui("redistricting_ui_1")
    
## To be copied in the server
# mod_redistricting_server("redistricting_ui_1")
