#' primary_22 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_primary_22_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName = 'primary_22',
          box(width = 6, selectInput(ns('sel_elec'), 
                                     'Select Election', 
                                     c('Louisville Mayor DEM', 'KY-03 US Congress DEM', 'Fayette County Attorney DEM'))),
          leafletOutput(ns('prim_22_leaf'))
  )
}
    
#' primary_22 Server Functions
#'
#' @noRd 
mod_primary_22_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    lou_mayor_dem <- tryCatch(read_rds('/srv/data/maps/mayor_leaf.rds'), error = function(e) NULL)
    us3_cong_dem <- tryCatch(read_rds('/srv/data/maps/cong_leaf.rds'), error = function(e) NULL)
    fayette_ca_dem <- tryCatch(read_rds('/srv/data/maps/fayette_ca_leaf.rds'), error = function(e) NULL)
    output$prim_22_leaf <- renderLeaflet({
      if(input$sel_elec == 'Louisville Mayor DEM') lou_mayor_dem
      else if(input$sel_elec == 'KY-03 US Congress DEM') us3_cong_dem
      else if(input$sel_elec == 'Fayette County Attorney DEM') fayette_ca_dem
    })
  })
}
    
## To be copied in the UI
# mod_primary_22_ui("primary_22_ui_1")
    
## To be copied in the server
# mod_primary_22_server("primary_22_ui_1")
