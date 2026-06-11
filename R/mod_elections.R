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
          # Responsive map height: fixed on desktop, taller (viewport-based) on mobile
          tags$style(HTML(sprintf(
            "#%1$s {height: 650px !important;}
             @media (max-width: 768px) {
               #%1$s {height: 75vh !important; min-height: 420px;}
               .election-filters .form-group {margin-bottom: 8px;}
             }",
            ns('election_map')
          ))),
          fluidRow(
            box(width = 12, title = 'Filters', status = 'primary',
                collapsible = TRUE, collapsed = FALSE,
                div(class = 'election-filters',
                    fluidRow(
                      column(width = 2, uiOutput(ns('select_year_ui')) %>% withSpinner()),
                      column(width = 2, uiOutput(ns('primary_general_ui')) %>% withSpinner()),
                      column(width = 4, uiOutput(ns('select_election_ui')) %>% withSpinner()),
                      column(width = 2, selectInput(ns('precinct_county'), label = 'Detail Level', c('Precinct', 'County'))),
                      column(width = 2, uiOutput(ns('include_counties_ui')) %>% withSpinner())
                    )
                )
            )
          ),
          fluidRow(
            box(width = 12,
                actionButton(ns('draw_map'), label = 'Draw Map'), 'Please only click me once and be patient :) (You will need to zoom manually)
                Please note: results are based on scrape of election night results. Consult offical results for 100% correct totals.',
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

#' Build grouped, sorted choices for the election picker
#'
#' Joins the supplied race strings to the `race_lookup` crosswalk (built by
#' data-raw/build_race_lookup.R) and returns a named list suitable for
#' pickerInput `choices`: each name is a friendly office-level header and each
#' value is the raw race strings in that group, sorted by canonical office then
#' district (numeric) then name. Empty groups are dropped. Races missing from
#' the crosswalk fall back to the "Local" group so the picker never loses items.
#'
#' @param races Character vector of distinct race names (already filtered to the
#'   selected year / election type).
#' @param lookup The pre-loaded `race_lookup` data frame.
#' @return Named list of character vectors, in office-level display order.
#' @noRd
build_election_choices <- function(races, lookup){
  df <- tibble(race = unique(races)) %>%
    left_join(lookup, by = 'race') %>%
    mutate(office = if_else(is.na(office), race, office))
  # Group + sort via the shared helper (see R/utils_office.R)
  group_office_choices(values = df$race, office_level = df$office_level,
                       office = df$office, district = df$district)
}

#' elections Server Functions
#'
#' @noRd
mod_elections_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    map_data_init <- reactive({
      req(input$select_year, input$primary_general, input$select_election, input$include_counties)
      coun <- str_to_lower(input$include_counties)
      tbl(db, 'election_data') %>%
        filter(year == !!input$select_year,
               election == !!str_to_lower(input$primary_general),
               race == !!input$select_election,
               county %in% coun) %>%
        distinct() %>%
        collect()
      })

    # Continuous color gradient: hue encodes the winning party, saturation/
    # darkness encodes the winning share (margin of victory). A pale fill is a
    # near-tie; a deep fill is a blowout. Vectorized over party/pct.
    pol_pal_xl <- function(party, pct){
      ramps <- list(
        REP  = c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d'),
        DEM  = c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#084594'),
        IND1 = c('#fcfbfd','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f'),
        IND2 = c('#fff5eb','#fdd0a2','#fdae6b','#fd8d3c','#f16913','#d94801','#8c2d04'),
        IND3 = c('#f7fcf5','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#005a32'),
        IND4 = c('#f7f4f9','#d4b9da','#c994c7','#df65b0','#e7298a','#ce1256','#91003f')
      )
      # Map the competitive range (50% -> 85% of the vote) onto the full ramp so
      # differences in margin are visible; wins above 85% saturate at the darkest shade.
      scale_lo <- 0.5
      scale_hi <- 0.85
      out <- rep('#f0f0f0', length(party))
      for(p in names(ramps)){
        idx <- which(party == p)
        if(length(idx) > 0){
          ramp_fn <- grDevices::colorRamp(ramps[[p]], space = 'Lab')
          t <- (pct[idx] - scale_lo) / (scale_hi - scale_lo)
          t[is.na(t)] <- 0
          t <- pmin(pmax(t, 0), 1)
          out[idx] <- grDevices::rgb(ramp_fn(t), maxColorValue = 255)
        }
      }
      out
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
      # Newest year first, default to most recent
      selectInput(ns('select_year'), label = 'Select Year', sort(yrs$year, decreasing = TRUE), max(yrs$year))
    })

    output$primary_general_ui <- renderUI({
      req(input$select_year)
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
      req(input$select_year, input$primary_general)
      elections <- tbl(db, 'election_data') %>%
        filter(year == !!input$select_year,
               election == str_to_lower(!!input$primary_general)) %>%
        select(race) %>%
        distinct() %>%
        collect() %>%
        pull(race)
      pickerInput(ns('select_election'),
                  label = 'Select Election',
                  choices = build_election_choices(elections, race_lookup),
                  multiple = FALSE,
                  options = pickerOptions('liveSearch' = TRUE))
    })

    output$include_counties_ui <- renderUI({
      req(input$select_year, input$primary_general, input$select_election)
      counties <- tbl(db, 'election_data') %>%
        filter(year == !!input$select_year,
               election == str_to_lower(!!input$primary_general),
               race == !!input$select_election) %>%
        select(county) %>%
        distinct() %>%
        mutate(county = str_to_lower(county)) %>%
        collect() %>%
        pull(county)
      pickerInput(ns('include_counties'),
                  label = 'Select Counties for Map',
                  choices = counties,
                  multiple = TRUE,
                  selected = counties,
                  options = pickerOptions('liveSearch' = TRUE,
                                          `actions-box` = TRUE,
                                          `deselect-all-text` = "Select None",
                                          `select-all-text` = "Select All"))
    })

    output$election_result <- renderReactable({
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

    output$election_map <- renderLeaflet({
      leaflet(ky_counties) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          opacity = 0.6,
          color = "#888888",
          fillColor = "#cccccc",
          fillOpacity = 0.1,
          smoothFactor = 0.3,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#444",
            fillOpacity = 0.3,
            bringToFront = TRUE))
    })

    output$election_table <- renderReactable({
      req(map_data_init())
      if(input$precinct_county == 'County' | nrow(map_data_init() %>% select(county, Precinct) %>% distinct()) > 1000){
        map_data_long <- map_data_init() %>%
          group_by(county, candidate, party) %>%
          summarize(votes = sum(votes)) %>%
          ungroup() %>%
          group_by(county) %>%
          mutate(winner = candidate[which.max(votes)],
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
      }
    })

    observeEvent(input$draw_map, {
      req(map_data_init())
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
          mutate(winner = party[which.max(votes)],
                 winning_share = votes[which.max(votes)] / sum(votes),
                 label_init = paste(can_txt, collapse = '<br>')) %>%
          mutate(fill_col = pol_pal_xl(winner, winning_share)) %>%
          ungroup() %>%
          # Vectorized label creation (replaced rowwise)
          mutate(label = lapply(
            str_glue('<h4>{str_to_title(county)}</h4>{label_init}'),
            HTML
          )) %>%
          select(-label_init, -can_txt)
        map_data <- map_data_long %>%
          select(-party) %>%
          pivot_wider(names_from = candidate, values_from = votes)

        map_data_shp <- ky_counties %>%
          mutate(county = str_to_lower(NAME)) %>%
          left_join(map_data, by = 'county')

        # Zoom to the counties that actually have results (the selected ones)
        bb <- map_data_shp %>%
          filter(!is.na(fill_col)) %>%
          sf::st_transform(4326) %>%
          sf::st_bbox()

        m <- leafletProxy("election_map", data = map_data_shp) %>%
          clearShapes() %>%
          addPolygons(
            fillColor = ~fill_col,
            fillOpacity = 0.78,
            color = "#ffffff",
            weight = 1,
            opacity = 0.7,
            smoothFactor = 0.3,
            label = ~label,
            highlightOptions = highlightOptions(
              weight = 3,
              color = "#444444",
              fillOpacity = 0.9,
              bringToFront = TRUE),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto")
          )
        if(all(is.finite(bb))){
          m %>% fitBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
        }

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
            mutate(fill_col = pol_pal_xl(winner, winning_share)) %>%
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

          if(input$select_year < 2022){
            prec_map <- ky_precincts
          }else if(input$select_year == 2022){
            prec_map <- ky_precincts_22
          }else if(input$select_year == 2023){
            prec_map <- ky_precincts_23
          }else{
            prec_map <- ky_precincts_24
          }

          map_data_shp <- prec_map %>%
            mutate(county = str_to_lower(NAME),
                   COUNTYFP = as.character(COUNTYFP)) %>%
            inner_join(map_data, by = c('COUNTYFP', 'VTDST'))

          # inner_join keeps only matched precincts, so bbox is the area shown
          bb <- map_data_shp %>%
            sf::st_transform(4326) %>%
            sf::st_bbox()

          m <- leafletProxy("election_map", data = map_data_shp) %>%
            clearShapes() %>%
            addPolygons(
              fillColor = ~fill_col,
              fillOpacity = 0.78,
              color = "#ffffff",
              weight = 1,
              opacity = 0.7,
              smoothFactor = 0.3,
              label = ~label,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#444444",
                fillOpacity = 0.9,
                bringToFront = TRUE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto")
            )
          if(all(is.finite(bb))){
            m %>% fitBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
          }
        }

      }
    })
  })
}

## To be copied in the UI
# mod_elections_ui("elections_ui_1")

## To be copied in the server
# mod_elections_server("elections_ui_1")
