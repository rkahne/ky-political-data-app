#' election_22 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_election_22_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName = 'election_22', 
          fluidRow(
            switchInput(ns('house_senate_select'), value = TRUE, onLabel = 'House', offLabel = 'Senate')
          ),
          fluidRow(
            box(width = 12, leafletOutput(ns('candidate_leaflet')) %>% withSpinner())
          ),
          fluidRow(
            box(width = 12, reactableOutput(ns('candidate_table')) %>% withSpinner())
          )
  )
}

#' election_22 Server Functions
#'
#' @noRd 
mod_election_22_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$candidate_leaflet <- renderLeaflet({
      if(input$house_senate_select == TRUE){
        can_tbl <- tbl(db, 'candidates_22') %>% 
          filter(office == 'State Representative') %>%  
          distinct() %>% 
          collect() %>% 
          mutate(candidate = if_else(is.na(lrc_website), '#6baed6', '#08519c'),
                 district = as.character(district),
                 label = if_else(is.na(middle_name), str_glue('{district}-{first_name} {last_name}'), str_glue('{district}-{first_name} {middle_name} {last_name}')))
        house_plan_hcs %>% 
          left_join(can_tbl) %>% 
          replace_na(list(candidate = 'white')) %>% 
          leaflet() %>% 
          addTiles() %>% 
          addPolygons(
            weight = 2,
            opacity = 1,
            color = "black",
            fillOpacity = 0,
            label = ~label
          ) %>% 
          addPolygons(
            fillOpacity = .7,
            color = ~candidate,
            weight = 1,
            label = ~label
          ) %>% 
          addLegend(position = 'topright',
                    colors = c('#6baed6', '#08519c'),
                    labels = c('New Candidate', 'Incumbent')) 
      }else{
        can_tbl <- tbl(db, 'candidates_22') %>% 
          filter(office == 'State Senator') %>%  
          distinct() %>% 
          collect() %>% 
          mutate(candidate = if_else(is.na(lrc_website), '#6baed6', '#08519c'),
                 district = as.character(district),
                 label = if_else(is.na(middle_name), str_glue('{district}-{first_name} {last_name}'), str_glue('{district}-{first_name} {middle_name} {last_name}')))
        sen_plan %>% 
          mutate(district = str_remove_all(district, 'District ')) %>% 
          left_join(can_tbl) %>% 
          mutate(candidate = case_when(!is.na(candidate) ~ candidate,
                                       as.numeric(district) %% 2 != 0 ~ 'white',
                                       T ~ 'black')) %>%  
          leaflet() %>% 
          addTiles() %>% 
          addPolygons(
            weight = 2,
            opacity = 1,
            color = "black",
            fillOpacity = 0,
            label = ~label
          ) %>% 
          addPolygons(
            fillOpacity = .7,
            color = ~candidate,
            weight = 1,
            label = ~label
          ) %>% 
          addLegend(position = 'topright',
                    colors = c('#6baed6', '#08519c'),
                    labels = c('New Candidate', 'Incumbent')) 
      }
    })
    
    output$candidate_table <- renderReactable({
      if(input$house_senate_select == TRUE){
        dist_sf <- st_contains(house_plan_hcs, st_point(c(input$candidate_leaflet_shape_click$lng, input$candidate_leaflet_shape_click$lat)), sparse = FALSE)
        dist <- house_plan_hcs$district[which(dist_sf == T)]
        df <- tbl(db, 'candidates_22') %>% 
          filter(office == 'State Representative',
                 district == dist)
      }else{
        dist_sf <- st_contains(sen_plan, st_point(c(input$candidate_leaflet_shape_click$lng, input$candidate_leaflet_shape_click$lat)), sparse = FALSE)
        dist <- sen_plan$district[which(dist_sf == T)] %>% str_remove_all('District ') %>% as.numeric()
        df <- tbl(db, 'candidates_22') %>% 
          filter(office == 'State Senator',
                 district == dist)
      }
      
      df %>% 
        collect() %>% 
        mutate(name = if_else(is.na(middle_name), str_glue('{first_name} {last_name}'), str_glue('{first_name} {middle_name} {last_name}'))) %>% 
        select(name, district, political_website, lrc_website, facebook, twitter, instagram) %>% 
        reactable(columns = list(
          name = colDef(name = ' '),
          district = colDef(name = ' ', cell = function(value) str_glue('District {value}')),
          political_website = colDef(name = ' ', cell = function(value, index){
            if(is.na(value)) ''
            else tags$a(href = value, target = '_blank', 'Political Website')
          }),
          lrc_website = colDef(name = ' ', cell = function(value){
            if(is.na(value)) ''
            else tags$a(href = as.character(value), target = '_blank', 'LRC Website')
          }),
          facebook = colDef(name = ' ', cell = function(value){
            if(is.na(value)) ''
            else tags$a(href = as.character(value), target = '_blank', shiny::icon('facebook', "fa-2x"))
          }),
          twitter =  colDef(name = ' ', cell = function(value){
            if(is.na(value)) ''
            else tags$a(href = as.character(value), target = '_blank', shiny::icon('twitter', "fa-2x"))
          }),
          instagram = colDef(name = ' ', cell = function(value){
            if(is.na(value)) ''
            else tags$a(href = as.character(value), target = '_blank', shiny::icon('instagram', "fa-2x"))
          })
        ))
    })
    
  })
}

## To be copied in the UI
# mod_election_22_ui("election_22_ui_1")

## To be copied in the server
# mod_election_22_server("election_22_ui_1")
