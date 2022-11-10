#' elections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_elections_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName = 'elections', 
          fluidRow(
            box(width = 2, uiOutput(ns('select_year_ui')) %>% withSpinner()),
            box(width = 2, uiOutput(ns('primary_general_ui')) %>% withSpinner()),
            box(width = 4, uiOutput(ns('select_election_ui')) %>% withSpinner()),
            box(width = 2, selectInput(ns('precinct_county'), label = 'Detail Level', c('Precinct', 'County')))
          ),
          fluidRow(
            box(width = 12, 
                actionButton(ns('draw_map'), label = 'Draw Map'), 'Please only click me once and be patient :) (You will need to zoom manually)',
                leafletOutput(ns('election_map'), height = 650) %>% withSpinner())
          ),
          fluidRow(
            box(width = 6, reactableOutput(ns('election_result')))
          ),
          fluidRow(
            box(width = 12,
                reactableOutput(ns('election_table')))
          )
  )
}
    
#' elections Server Functions
#'
#' @noRd 
mod_elections_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    map_data_init <- reactive({
      if(length(input$select_year) > 0 & 
         length(input$primary_general) > 0 &
         length(input$select_election) > 0){
        tbl(db, 'election_data') %>% 
          filter(year == !!input$select_year,
                 election == str_to_lower(!!input$primary_general),
                 race == !!input$select_election) %>% 
          collect()
      }
      }) 
    
    pol_pal_xl <- function(party, pct){
      case_when(party == 'REP' & pct > .8 ~ '#99000d',
                party == 'REP' & pct > .75 ~ '#cb181d',
                party == 'REP' & pct > .7 ~ '#ef3b2c',
                party == 'REP' & pct > .65 ~ '#fb6a4a',
                party == 'REP' & pct > .6 ~ '#fc9272',
                party == 'REP' & pct > .55 ~ '#fcbba1',
                party == 'REP' & pct > .5 ~ '#fee0d2',
                party == 'REP' ~ '#fff5f0',
                party == 'DEM' & pct > .8 ~ '#084594',
                party == 'DEM' & pct > .75 ~ '#2171b5',
                party == 'DEM' & pct > .7 ~ '#4292c6',
                party == 'DEM' & pct > .65 ~ '#6baed6',
                party == 'DEM' & pct > .6 ~ '#9ecae1',
                party == 'DEM' & pct > .55 ~ '#c6dbef',
                party == 'DEM' & pct > .5 ~ '#deebf7',
                party == 'DEM' ~ '#f7fbff',
                
                party == 'IND1' & pct >  .8  ~ '#6a51a3',
                party == 'IND1' & pct >  .65 ~ '#9e9ac8',
                party == 'IND1' & pct >  .5 ~ '#cbc9e2',
                party == 'IND1' & pct < .5  ~ '#f2f0f7',
                
                party == 'IND2' & pct <  .8  ~ '#d94701',
                party == 'IND2' & pct >  .65 ~ '#fd8d3c',
                party == 'IND2' & pct >  .5  ~ '#fdbe85',
                party == 'IND2' & pct <  .5  ~ '#feedde',
                
                party == 'IND3' & pct >  .8  ~ '#238b45',
                party == 'IND3' & pct >  .65 ~ '#74c476',
                party == 'IND3' & pct >  .5  ~ '#bae4b3',
                party == 'IND3' & pct <  .5  ~ '#edf8e9',
                
                party == 'IND4' & pct >  .8  ~ '#ce1256',
                party == 'IND4' & pct >  .65 ~ '#df65b0',
                party == 'IND4' & pct >  .5  ~ '#d7b5d8',
                party == 'IND4' & pct <  .5  ~ '#f1eef6',
                T ~ 'white')
    }
    
    create_reactable <- function(mdl, canl, coun_cd){
      if(nrow(canl) == 2){
        mdl %>% 
          left_join(canl) %>% 
          select(-party, -candidate, -votes) %>% 
          pivot_wider(names_from = num, values_from = vote) %>%
          mutate(across(starts_with("can"), 
                        ~., 
                        .names = "{.col}_pct")) %>% 
          select(-total_votes) %>% 
          reactable(
            columns = list(
              county = coun_cd,
              can1 = colDef(name = 'Votes',
                            cell = function(value){scales::comma(unlist(value)[1], accuracy = 1)},
                            format = colFormat(separators = TRUE, digits = 2)),
              can1_pct = colDef(name = 'Percent',
                                cell = function(value){scales::percent(unlist(value)[1] / unlist(value)[2], accuracy = 0.01)},
                                format = colFormat(percent = TRUE)),
              can2 = colDef(name = 'Votes',
                            cell = function(value){scales::comma(unlist(value)[1], accuracy = 1)},
                            format = colFormat(separators = TRUE, digits = 2)),
              can2_pct = colDef(name = 'Percent',
                                cell = function(value){scales::percent(unlist(value)[1] / unlist(value)[2], accuracy = 0.01)},
                                format = colFormat(percent = TRUE)),
              winner = colDef(name = 'Winner')
            ),
            columnGroups = list(
              colGroup(name = canl$candidate[which(canl$num == 'can1')], columns = c("can1", "can1_pct")),
              colGroup(name = canl$candidate[which(canl$num == 'can2')], columns = c("can2", "can2_pct"))
            )
          )
      }else if(nrow(canl) == 3){
        mdl %>% 
          left_join(canl) %>% 
          select(-party, -candidate, -votes) %>% 
          pivot_wider(names_from = num, values_from = vote) %>%
          mutate(across(starts_with("can"), 
                        ~., 
                        .names = "{.col}_pct")) %>% 
          select(-total_votes) %>% 
          reactable(
            columns = list(
              county = coun_cd,
              can1 = colDef(name = 'Votes',
                            cell = function(value){scales::comma(unlist(value)[1], accuracy = 1)},
                            format = colFormat(separators = TRUE, digits = 2)),
              can1_pct = colDef(name = 'Percent',
                                cell = function(value){scales::percent(unlist(value)[1] / unlist(value)[2], accuracy = 0.01)},
                                format = colFormat(percent = TRUE)),
              can2 = colDef(name = 'Votes',
                            cell = function(value){scales::comma(unlist(value)[1], accuracy = 1)},
                            format = colFormat(separators = TRUE, digits = 2)),
              can2_pct = colDef(name = 'Percent',
                                cell = function(value){scales::percent(unlist(value)[1] / unlist(value)[2], accuracy = 0.01)},
                                format = colFormat(percent = TRUE)),
              can3 = colDef(name = 'Votes',
                            cell = function(value){scales::comma(unlist(value)[1], accuracy = 1)},
                            format = colFormat(separators = TRUE, digits = 2)),
              can3_pct = colDef(name = 'Percent',
                                cell = function(value){scales::percent(unlist(value)[1] / unlist(value)[2], accuracy = 0.01)},
                                format = colFormat(percent = TRUE)),
              winner = colDef(name = 'Winner')
            ),
            columnGroups = list(
              colGroup(name = canl$candidate[which(canl$num == 'can1')], columns = c("can1", "can1_pct")),
              colGroup(name = canl$candidate[which(canl$num == 'can2')], columns = c("can2", "can2_pct")),
              colGroup(name = canl$candidate[which(canl$num == 'can3')], columns = c("can3", "can3_pct"))
            )
          )
      }else if(nrow(canl) == 4){
        mdl %>% 
          left_join(canl) %>% 
          select(-party, -candidate, -votes) %>% 
          pivot_wider(names_from = num, values_from = vote) %>%
          mutate(across(starts_with("can"), 
                        ~., 
                        .names = "{.col}_pct")) %>% 
          select(-total_votes) %>% 
          reactable(
            columns = list(
              county = coun_cd,
              can1 = colDef(name = 'Votes',
                            cell = function(value){scales::comma(unlist(value)[1], accuracy = 1)},
                            format = colFormat(separators = TRUE, digits = 2)),
              can1_pct = colDef(name = 'Percent',
                                cell = function(value){scales::percent(unlist(value)[1] / unlist(value)[2], accuracy = 0.01)},
                                format = colFormat(percent = TRUE)),
              can2 = colDef(name = 'Votes',
                            cell = function(value){scales::comma(unlist(value)[1], accuracy = 1)},
                            format = colFormat(separators = TRUE, digits = 2)),
              can2_pct = colDef(name = 'Percent',
                                cell = function(value){scales::percent(unlist(value)[1] / unlist(value)[2], accuracy = 0.01)},
                                format = colFormat(percent = TRUE)),
              can3 = colDef(name = 'Votes',
                            cell = function(value){scales::comma(unlist(value)[1], accuracy = 1)},
                            format = colFormat(separators = TRUE, digits = 2)),
              can3_pct = colDef(name = 'Percent',
                                cell = function(value){scales::percent(unlist(value)[1] / unlist(value)[2], accuracy = 0.01)},
                                format = colFormat(percent = TRUE)),
              can4 = colDef(name = 'Votes',
                            cell = function(value){scales::comma(unlist(value)[1], accuracy = 1)},
                            format = colFormat(separators = TRUE, digits = 2)),
              can4_pct = colDef(name = 'Percent',
                                cell = function(value){scales::percent(unlist(value)[1] / unlist(value)[2], accuracy = 0.01)},
                                format = colFormat(percent = TRUE)),
              winner = colDef(name = 'Winner')
            ),
            columnGroups = list(
              colGroup(name = canl$candidate[which(canl$num == 'can1')], columns = c("can1", "can1_pct")),
              colGroup(name = canl$candidate[which(canl$num == 'can2')], columns = c("can2", "can2_pct")),
              colGroup(name = canl$candidate[which(canl$num == 'can3')], columns = c("can3", "can3_pct")),
              colGroup(name = canl$candidate[which(canl$num == 'can4')], columns = c("can4", "can4_pct"))
            )
          )
      }else{
        mdl %>% 
          select(-vote, -total_votes, -party) %>% 
          pivot_wider(names_from = candidate, values_from = votes) %>% 
          reactable(columns = list(
            county = coun_cd,
            winner = colDef(name = 'Winner', cell = function(value) str_to_title(value))
          ))
      }
      
    }
    
    
    output$select_year_ui <- renderUI({
      yrs <- tbl(db, 'election_data') %>% select(year) %>% distinct() %>% collect()
      selectInput(ns('select_year'), label = 'Select Year', sort(yrs$year), max(yrs))
    })
    
    output$primary_general_ui <- renderUI({
      prim_gen <- tbl(db, 'election_data') %>% 
        filter(year == !!input$select_year) %>% 
        select(election) %>% 
        distinct() %>% 
        collect() %>% 
        pull(election) %>% 
        str_to_title()
      selectInput(ns('primary_general'), label = 'Election Type', prim_gen, 'General')
    })
    
    output$select_election_ui <- renderUI({
      elections <- tbl(db, 'election_data') %>% 
        filter(year == !!input$select_year,
               election == str_to_lower(!!input$primary_general)) %>% 
        select(race) %>% 
        distinct() %>% 
        collect() %>% 
        pull(race)
      pickerInput(ns('select_election'), 
                  label = 'Select Election', 
                  choices = elections,
                  multiple = FALSE,
                  options = pickerOptions('liveSearch' = TRUE))
    })
    
    output$election_result <- renderReactable({
      map_data_init() %>% 
        group_by(candidate) %>% 
        summarize(votes = sum(votes)) %>% 
        ungroup() %>% 
        arrange(desc(votes)) %>% 
        reactable(columns = list(
          candidate = colDef(name = 'Candidate',
                             cell = function(value) str_to_title(value),
                             style = list(fontSize = '20px')),
          votes = colDef(name = 'Votes',
                         format = colFormat(separators = TRUE, digits = 0),
                         style = list(fontSize = '20px'))
          )
        )
    })
    
    output$election_map <- renderLeaflet({
      leaflet(ky_counties) %>% 
        addTiles() %>% 
        addPolygons(
          weight = 2,
          opacity = 1,
          color = "black",
          dashArray = "3",
          fillOpacity = 0.1,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE))
    })
    
    output$election_table <- renderReactable({
      if(!is.null(map_data_init())){
        if(input$precinct_county == 'County' | nrow(map_data_init() %>% select(county, Precinct) %>% distinct()) > 1000){
          map_data_long <- map_data_init() %>% 
            group_by(county, candidate, party) %>% 
            summarize(votes = sum(votes)) %>% 
            ungroup() %>% 
            group_by(county) %>% 
            mutate(winner = candidate[which(votes == max(votes))],
                   total_votes = sum(votes)) %>% 
            ungroup() %>% 
            rowwise() %>% 
            mutate(vote = list(c(votes, total_votes))) %>% 
            ungroup() 
          can_list <- map_data_long %>% 
            select(candidate) %>% 
            distinct() %>% 
            mutate(num = paste0('can', 1:n()))
          
          cd <- colDef(name = 'County',
                       cell = function(value) str_to_title(value))
          
          create_reactable(map_data_long, can_list, cd)
        }else{
          map_data_long <- map_data_init() %>% 
            filter(Precinct != 'county') %>% 
            mutate(Precinct = paste(str_to_upper(county),Precinct, sep = ' - ')) %>% 
            group_by(Precinct, candidate, party) %>% 
            summarize(votes = sum(votes)) %>% 
            ungroup() %>% 
            nest(data = c(-Precinct)) %>% 
            mutate(winner = map_chr(data, function(x){
              winner <- x %>% 
                filter(votes == max(votes)) %>% 
                pull(party)
              if(length(winner) == 1){
                winner 
              }else{ 
                'Tie'
              }
            })) %>% 
            unnest(data) %>% 
            group_by(Precinct) %>% 
            mutate(total_votes = sum(votes)) %>% 
            ungroup() %>% 
            rowwise() %>% 
            mutate(vote = list(c(votes, total_votes))) %>% 
            ungroup() %>% 
            rename(county = Precinct)
          can_list <- map_data_long %>% 
            select(candidate) %>% 
            distinct() %>% 
            mutate(num = paste0('can', 1:n()))
          
          cd <- colDef(name = 'Precinct',
                 cell = function(value) str_to_title(value))
          
          create_reactable(map_data_long, can_list, cd)
        }
      }
    })
    
    observeEvent(input$draw_map, {
      if('primary' %in% pull(distinct(select(map_data_init(), party)), party)){
        tbl_join <- map_data_init() %>% 
          group_by(candidate) %>% 
          summarize(votes = sum(votes)) %>%
          mutate(pct = votes / sum(votes)) %>% 
          arrange(desc(pct)) %>% 
          mutate(cnt = 1:n()) %>% 
          mutate(party = paste0('IND', cnt))
        tbl_dta <- map_data_init() %>% 
          select(-party) %>% 
          left_join(
            tbl_join %>% select(candidate, party),
            by = 'candidate'
          )
      }else{
        tbl_dta <- map_data_init()
      }
      if(input$precinct_county == 'County'){
        map_data_long <- tbl_dta %>% 
          group_by(county, candidate, party) %>% 
          summarize(votes = sum(votes)) %>% 
          ungroup() %>% 
          collect() %>% 
          mutate(can_txt = paste0(candidate, ': ', scales::comma(votes, accuracy = 1))) %>% 
          group_by(county) %>% 
          mutate(winner = party[which(votes == max(votes))],
                 winning_share = votes[which(votes == max(votes))] / sum(votes),
                 label_init = paste(can_txt, collapse = '<br>')) %>% 
          mutate(fill_col = map2_chr(winner, winning_share, pol_pal_xl)) %>% 
          ungroup() %>% 
          rowwise() %>% 
          mutate(label = HTML(str_glue('<h4>{str_to_title(county)}</h4>{label_init}'))) %>% 
          select(-label_init, -can_txt)
        map_data <- map_data_long %>% 
          select(-party) %>% 
          pivot_wider(names_from = candidate, values_from = votes)
        
        map_data_shp <- ky_counties %>% 
          mutate(county = str_to_lower(NAME)) %>% 
          left_join(map_data, by = 'county')
        
        leafletProxy("election_map", data = map_data_shp) %>%
          clearShapes() %>% 
          addPolygons(
            fillOpacity = .7,
            color = ~fill_col,
            weight = 1,
            label = ~label,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "10px",
              direction = "auto")
          )
          
      }else{
        map_data_long_chk <- tbl_dta %>% 
          collect() 
        precinct_num <- map_data_long_chk %>% 
          select(county, Precinct) %>% 
          distinct() %>% 
          nrow()
        if(precinct_num > 1000){
          leafletProxy("election_map") %>%
            addControl(str_glue('<h4>Precinct Level only available on maps <1000 precincts</h4>'), position = 'bottomleft')
        }else{
          map_data_long <- map_data_long_chk %>% 
            nest(data = c(-Precinct, -county, -race)) %>% 
            mutate(winner = map_chr(data, function(x){
              winner <- x %>% 
                filter(votes == max(votes)) %>% 
                pull(party)
              if(length(winner) == 1){
                winner 
              }else{ 
                'Tie'
              }
            })) %>% 
            unnest(data) %>% 
            mutate(can_txt = paste0(candidate, ': ', scales::comma(votes, accuracy = 1))) %>% 
            group_by(county, Precinct) %>% 
            mutate(winning_share = votes[which(votes == max(votes))][1] / sum(votes),
                   label_init = paste(can_txt, collapse = '<br>')) %>% 
            mutate(fill_col = map2_chr(winner, winning_share, pol_pal_xl)) %>% 
            ungroup() %>% 
            rowwise() %>% 
            mutate(label = HTML(str_glue('<h4>{str_to_title(county)} - {Precinct}</h4>{label_init}'))) %>% 
            select(-label_init, -can_txt)
          map_data <- map_data_long %>% 
            select(-party) %>% 
            pivot_wider(names_from = candidate, values_from = votes) %>% 
            left_join(tbl(db, 'fips') %>% collect()) %>% 
            mutate(VTDST = paste0('00',Precinct),
                   COUNTYFP = as.character(fips) %>% str_sub(3,5))
          
          # WORK
          if(!!input$select_election < 2022){
            prec_map <- ky_precincts
          }else{
            prec_map <- ky_precincts_22
          }
          
          map_data_shp <- prec_map %>% 
            mutate(county = str_to_lower(NAME),
                   COUNTYFP = as.character(COUNTYFP)) %>% 
            inner_join(map_data, by = c('COUNTYFP', 'VTDST'))
          
          leafletProxy("election_map", data = map_data_shp) %>%
            clearShapes() %>% 
            addPolygons(
              fillOpacity = .7,
              color = ~fill_col,
              weight = 1,
              label = ~label,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "10px",
                direction = "auto")
            )
        }
        
      }
    })
  })
}
    
## To be copied in the UI
# mod_elections_ui("elections_ui_1")
    
## To be copied in the server
# mod_elections_server("elections_ui_1")
