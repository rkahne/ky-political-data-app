#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
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
#' @import keyring
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      dashboardHeader(title = ''),
      dashboardSidebar(
        sidebarMenu(id = 'sidebar_menu',
                    menuItem('Elections', tabName = 'elections', icon = icon('vote-yea')),
                    menuItem('Legislation', tabName = 'legislation', icon = icon('landmark')),
                    menuItem('Redistricting', tabName = 'redistricting', icon = icon('map'), selected = TRUE))
      ),
      dashboardBody(
        tabItems(
          mod_elections_ui("elections_ui_1"),
          mod_legislation_ui("legislation_ui_1"),
          mod_redistricting_ui("redistricting_ui_1")
        ),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'kypoliticaldata'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

