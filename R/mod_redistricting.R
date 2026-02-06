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
          tabsetPanel(id = ns('redist_tabs'),
                      tabPanel(title = 'New Proposed Districts', value = ns('redist_full_map'),
                                   fluidRow(
                                     tabsetPanel(id = ns('redist_top_level'),
                                                 tabPanel(title = 'House', value = ns('redist_top_house'),
                                                          box(width = 12,
                                                              box(width = 12, 'Please be patient while the map loads', leafletOutput(ns('house_map'), height = 800) %>% withSpinner())
                                                          )),
                                                 tabPanel(title = 'Senate', value = ns('redist_top_sen'),
                                                          box(width = 12,
                                                              box(width = 12, 'Please be patient while the map loads', leafletOutput(ns('senate_map'), height = 800) %>% withSpinner())
                                                          )),
                                                 tabPanel(title = 'Congress', value = ns('redist_top_cong'),
                                                          box(width = 12,
                                                              box(width = 12, 'Please be patient while the map loads', leafletOutput(ns('cong_map'), height = 800) %>% withSpinner())
                                                          ))
                                     )

                                )

                      ),
                      tabPanel(title = 'Overlay Election Results', value = ns('redist_map_election'),
                                 fluidRow(
                                   box(width = 12,
                                       fluidRow(
                                         box(width = 2, selectInput(ns('select_chamber'), 'Select Chamber', c('House', 'Senate'))),
                                         box(width = 4, uiOutput(ns('select_district_ui'))),
                                         box(width = 2, selectInput(ns('select_year'), 'Select Year', c(2018, 2019, 2020, 2022))),
                                         box(width = 4, uiOutput(ns('select_race_ui')))
                                       ),
                                       fluidRow(
                                         box(width = 12, leafletOutput(ns('election_leaf')) %>% withSpinner())
                                       ),
                                       fluidRow(
                                         box(width = 12, reactableOutput(ns('election_summary')) %>% withSpinner())
                                       ),
                                       fluidRow(
                                         box(width = 12, reactableOutput(ns('election_detail')) %>% withSpinner())
                                       ))
                                 )
                               )
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

    prec <- reactive({
      req(input$select_chamber, input$select_district)
      if(input$select_chamber == 'House'){
        tbl(db, 'redistricting_gop_house_22_hcs') %>%
          filter(district == !!input$select_district) %>%
          select(county, precinct)
      }else{
        tbl(db, 'redistricting_gop_sen_22') %>%
          filter(district == !!input$select_district) %>%
          select(county, precinct)
      }
    })

    map_data_init <- reactive({
      req(input$select_year, input$select_race, input$select_chamber)
      tbl(db, 'election_data') %>%
        filter(year == !!input$select_year,
               election == 'general',
               race == !!input$select_race) %>%
        mutate(Precinct = str_sub(Precinct, 1, 4)) %>%
        group_by(Precinct, county, race, candidate, year, election, party) %>%
        summarize(votes = sum(votes)) %>%
        ungroup() %>%
        inner_join(prec(), by = c('county', 'Precinct' = 'precinct')) %>%
        collect()
    })

    output$house_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = house_plan_hcs,
                    group = '2022 HCS',
                    weight = 2,
                    opacity = 1,
                    color = "black",
                    fillOpacity = 0,
                    label = ~district) %>%
        addPolygons(data = house_plan,
                    group = '2022 Original Bill',
                    weight = 2,
                    opacity = 1,
                    color = "green",
                    fillOpacity = 0,
                    label = ~district) %>%
        addPolygons(data = house_plan_dem,
                    group = '2022 Democratic Plan',
                    weight = 2,
                    opacity = 1,
                    color = "blue",
                    fillOpacity = 0,
                    label = ~district)%>%
        addPolygons(data = house_2014,
                    group = '2014',
                    weight = 2,
                    opacity = 1,
                    color = "red",
                    fillOpacity = 0,
                    label = ~DISTRICT) %>%
        addLayersControl(
          overlayGroups = c('2022 HCS', '2022 Original Bill', '2022 Democratic Plan', '2014'),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c('2022 Original Bill', '2022 Democratic Plan', '2014'))
    })

    output$senate_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = sen_plan,
                    group = '2022',
                    weight = 2,
                    opacity = 1,
                    color = "black",
                    fillOpacity = 0,
                    label = ~district) %>%
        addPolygons(data = sen_2014,
                    group = '2014',
                    weight = 2,
                    opacity = 1,
                    color = "blue",
                    fillOpacity = 0,
                    label = ~DISTRICT) %>%
        addLayersControl(
          overlayGroups = c('2022', '2014'),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup('2014')
    })

    output$cong_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = cong_plan,
                    group = '2022',
                    weight = 2,
                    opacity = 1,
                    color = "black",
                    fillOpacity = 0,
                    label = ~district)
    })

    output$select_district_ui <- renderUI({
      req(input$select_chamber)
      if(input$select_chamber == 'House'){
        opts <- tbl(db, 'redistricting_gop_house_22_hcs') %>%
          select(district) %>%
          distinct() %>%
          collect() %>%
          mutate(row = str_remove_all(district, 'District ') %>% as.numeric()) %>%
          arrange(row) %>%
          pull(district)
      }else{
        opts <- tbl(db, 'redistricting_gop_sen_22') %>%
          select(district) %>%
          distinct() %>%
          collect() %>%
          mutate(row = str_remove_all(district, 'District ') %>% as.numeric()) %>%
          arrange(row) %>%
          pull(district)
      }

      pickerInput(ns('select_district'),
                  'Select District',
                  opts,
                  multiple = FALSE,
                  options = pickerOptions('liveSearch' = TRUE)) %>% withSpinner()
    })

    output$select_race_ui <- renderUI({
      req(input$select_year, input$select_district)
      opts <- tbl(db, 'election_data') %>%
        filter(election == 'general') %>%
        inner_join(prec(), by = c('county', 'Precinct' = 'precinct')) %>%
        filter(year == !!input$select_year) %>%
        select(race) %>%
        distinct() %>%
        collect() %>%
        pull(race)

      pickerInput(ns('select_race'),
                  label = 'Select Election',
                  choices = opts,
                  multiple = FALSE,
                  options = pickerOptions('liveSearch' = TRUE)) %>% withSpinner()
    })

    output$election_leaf <- renderLeaflet({
      req(input$select_race, input$select_year, input$select_district, map_data_init())
      map_data_long_chk <- map_data_init()

      # Vectorized winner determination (replaced nest/map_chr/unnest)
      map_data_long <- map_data_long_chk %>%
        group_by(county, Precinct, race) %>%
        mutate(
          max_votes = max(votes),
          n_winners = sum(votes == max_votes),
          winner = if_else(n_winners == 1L, party[which.max(votes)], 'Tie')
        ) %>%
        ungroup() %>%
        select(-max_votes, -n_winners) %>%
        mutate(can_txt = paste0(candidate, ': ', scales::comma(votes, accuracy = 1))) %>%
        group_by(county, Precinct) %>%
        mutate(winning_share = votes[which.max(votes)] / sum(votes),
               label_init = paste(can_txt, collapse = '<br>')) %>%
        mutate(fill_col = map2_chr(winner, winning_share, pol_pal_xl)) %>%
        ungroup() %>%
        # Vectorized label creation (replaced rowwise)
        mutate(label = lapply(
          str_glue('<h4>{str_to_title(county)} - {Precinct}</h4>{label_init}'),
          HTML
        )) %>%
        select(-label_init, -can_txt)
      map_data <- map_data_long %>%
        select(-party) %>%
        pivot_wider(names_from = candidate, values_from = votes) %>%
        # Use pre-loaded fips_lookup instead of querying DB
        left_join(fips_lookup) %>%
        mutate(VTDST = paste0('00',Precinct),
               COUNTYFP = as.character(fips) %>% str_sub(3,5))

      map_data_shp <- ky_precincts %>%
        mutate(county = str_to_lower(NAME),
               COUNTYFP = as.character(COUNTYFP)) %>%
        inner_join(map_data, by = c('COUNTYFP', 'VTDST'))

      if(input$select_chamber == 'House'){
        map_shp <- house_plan_hcs %>% filter(district == str_remove_all(!!input$select_district, 'District '))
      }else{
        map_shp <- sen_plan %>% filter(district == !!input$select_district)
      }

      leaflet() %>%
        addTiles()  %>%
        addPolygons(
          data = map_shp,
          weight = 2,
          opacity = 1,
          color = 'black',
          fillOpacity = 0,
        ) %>%
        addPolygons(
          data = map_data_shp,
          fillOpacity = .7,
          color = ~fill_col,
          weight = 1,
          label = ~label,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "10px",
            direction = "auto")
        )
    })

    output$election_summary <- renderReactable({
      req(map_data_init())
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

    output$election_detail <- renderReactable({
      req(map_data_init())
      # Vectorized winner determination (replaced nest/map_chr/unnest)
      map_data_long <- map_data_init() %>%
        filter(Precinct != 'county') %>%
        mutate(Precinct = paste(str_to_upper(county), Precinct, sep = ' - ')) %>%
        group_by(Precinct, candidate, party) %>%
        summarize(votes = sum(votes)) %>%
        ungroup() %>%
        group_by(Precinct) %>%
        mutate(
          max_votes = max(votes),
          n_winners = sum(votes == max_votes),
          winner = if_else(n_winners == 1L, party[which.max(votes)], 'Tie'),
          total_votes = sum(votes)
        ) %>%
        ungroup() %>%
        select(-max_votes, -n_winners) %>%
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
    })


  })
}

## To be copied in the UI
# mod_redistricting_ui("redistricting_ui_1")

## To be copied in the server
# mod_redistricting_server("redistricting_ui_1")
