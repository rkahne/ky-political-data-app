#' legislation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_legislation_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName = 'legislation', 
          fluidRow(
            box(width = 4, uiOutput(ns('select_session_ui'))),
            uiOutput(ns('update_day_ui'))
          ),
          tabsetPanel(id = ns('leg_tabs'),
                      tabPanel(title = 'Explore Bills', value = ns('explore_bills'),
                               fluidRow(
                                 box(width = 12, 
                                     switchInput(ns('house_senate_table_select'), value = TRUE, onLabel = 'House', offLabel = 'Senate'),
                                     reactableOutput(ns('bill_overview_table')) %>% shinycssloaders::withSpinner())
                               )),
                      tabPanel(title = 'Bill Details', value = ns('bill_details'),
                               fluidRow(
                                 box(width = 12,
                                     fluidRow(
                                       box(width = 4, uiOutput(ns('select_bill_ui'))),
                                       box(width = 4, uiOutput(ns('view_bill_lrc')))
                                     ),
                                     fluidRow(
                                       box(width = 12, uiOutput(ns('bill_detail_header')))
                                     ),
                                     fluidRow(
                                       box(width = 12, tabsetPanel(id = ns('leg_bill_details'),
                                                                   tabPanel(title = 'Actions', value = ns('actions'),
                                                                            box(width = 12, reactableOutput(ns('bill_detail_actions')))),
                                                                   tabPanel(title = 'Votes', value = ns('votes'),
                                                                            fluidRow(box(width = 4, 'Note: Votes represent EITHER Vote for Passage or Veto Override, whichever is last.')),
                                                                            fluidRow(box(width = 6, title = 'House', 
                                                                                         reactableOutput(ns('bill_house_vote_summary')),
                                                                                         reactableOutput(ns('bill_house_votes'))),
                                                                                     box(width = 6, title = 'Senate', 
                                                                                         reactableOutput(ns('bill_senate_vote_summary')),
                                                                                         reactableOutput(ns('bill_senate_votes')))))
                                       )
                                       )
                                     )
                                 )
                               )
                      ),
                      tabPanel(title = 'Legislator Details', value = ns('bill_legislator_details'),
                               fluidRow(
                                 box(width = 4, uiOutput(ns('select_legislator_ui')))
                               ),
                               fluidRow(
                                 box(width = 8, uiOutput(ns('legislator_descrip'))),
                                 box(width = 4, uiOutput(ns('legislator_img')))
                               ),
                               fluidRow(
                                 box(width = 12, tabsetPanel(id = ns('leg_leg_details'),
                                                             tabPanel(title = 'Votes', value = ns('leg_votes'),
                                                                      box(width = 12, reactableOutput(ns('legislator_votes')))),
                                                             tabPanel(title = 'Sponsorship', value = ns('leg_sponsors'),
                                                                      box(width = 12, reactableOutput(ns('legislator_sponsorship'))))
                                                             )
                                     )
                               )
                      ),
                      tabPanel(title = 'Actions By Day', value = ns('actions_by_day'),
                               fluidRow(
                                 box(width = 4, uiOutput(ns('select_day_actions_ui')))
                               ),
                               fluidRow(
                                 box(width = 12, reactableOutput(ns('actions_actions_reactable')))
                               ))
          )
  )
}

#' legislation Server Functions
#'
#' @noRd 
mod_legislation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    l_md <- reactive({
      if(input$house_senate_table_select == TRUE){
        tbl(db, 'legislative_metadata') %>%
          filter(session == !!input$select_session,
                 str_sub(bill_num, 1, 1) == 'h') %>% 
          select(bill_num, title, bill_summary, last_action, passed_house, passed_senate, passed) %>% 
          collect() %>% 
          mutate(bill_q = as.numeric(str_remove_all(bill_num, '\\D'))) %>% 
          arrange(bill_q) %>% 
          select(-bill_q)
      }else if(input$house_senate_table_select == FALSE){
        tbl(db, 'legislative_metadata') %>%
          filter(session == !!input$select_session,
                 str_sub(bill_num, 1, 1) == 's') %>% 
          collect() %>% 
          select(bill_num, title, bill_summary, last_action, passed_house, passed_senate, passed) %>% 
          mutate(bill_q = as.numeric(str_remove_all(bill_num, '\\D'))) %>% 
          arrange(bill_q) %>% 
          select(-bill_q) 
      }
    })
    
    output$select_session_ui <- renderUI({
      opts <- tbl(db, 'legislative_sponsors') %>% 
        select(session) %>% 
        distinct() %>%
        arrange() %>% 
        collect() %>% 
        pull(session) %>% 
        sort(decreasing = TRUE)
      
      selectInput(ns('select_session'), 'Select Session', opts)
    })
    
    output$update_day_ui <- renderUI({
      if(input$select_session == '2023 General Assembly'){
        update_day <- collect(tbl(db, 'legislative_update_day'))$update_day
        box(width = 2, str_glue('Last Update: {month(update_day, label = TRUE, abbr = FALSE)} {day(update_day)}, {year(update_day)}'))
      }
    })
    
    output$bill_overview_table <- renderReactable({
      if(!is.null(input$select_session) & !is.null(input$house_senate_table_select)){
        if(input$house_senate_table_select == TRUE){
          tab_init <- tbl(db, 'legislative_metadata') %>%
            filter(session == !!input$select_session,
                   str_sub(bill_num, 1, 1) == 'h')
        }else{
          tab_init <- tbl(db, 'legislative_metadata') %>%
            filter(session == !!input$select_session,
                   str_sub(bill_num, 1, 1) == 's')
        }
        
        
        data_table <- tab_init %>% 
          select(bill_num, title, bill_summary, last_action, passed_house, passed_senate, passed) %>% 
          left_join(
            tbl(db, 'legislative_sponsors') %>% 
              filter(session == !!input$select_session) %>% 
              left_join(tbl(db, 'legislative_legislators') %>% 
                          select(legislator_id, name, party)) %>% 
              mutate(name_party = paste(name, party, sep = '|')) %>%
              group_by(bill_num) %>% 
              mutate(spon_list = str_flatten(name_party, collapse = ';')) %>% 
              group_by(bill_num, spon_list, party) %>% 
              summarize(spon_count = n()) %>% 
              ungroup() %>% 
              filter(!is.na(spon_count),
                     !is.na(party)) %>% 
              pivot_wider(names_from = 'party', values_from = spon_count) %>% 
              replace_na(list(`Democratic` = 0, `Republican` = 0))
          ) %>% 
          ungroup() %>% 
          select(bill_num, title, bill_summary, last_action, spon_list, Democratic, Republican, passed_house, passed_senate, passed) %>% 
          collect() %>% 
          mutate(bill_q = as.numeric(str_remove_all(bill_num, '\\D'))) %>% 
          arrange(bill_q) %>% 
          select(-bill_q)
        
        data_table %>% 
          reactable(columns = list(
            .selection = colDef(show = FALSE),
            bill_num = colDef(name = 'Bill Number', cell = str_to_upper),
            title = colDef(name = 'Bill Title', minWidth = 200),
            bill_summary = colDef(show = FALSE),
            last_action = colDef(name = 'Last Action', minWidth = 200),
            spon_list = colDef(name = 'Sponsors', cell = function(value){
              tibble(sponsor = unlist(str_split(value, pattern = ';'))) %>%
                separate(sponsor, c('name', 'party'), '\\|') %>%
                mutate(name_css = case_when(party == 'Republican' ~ paste0('<span style="color:red">',name,'</span>'),
                                            party == 'Democratic' ~ paste0('<span style="color:blue">',name,'</span>'),
                                            T ~ paste0('<span>',name,'</span>'))) %>%
                pull(name_css) %>%
                paste(., collapse = ', ') %>%
                HTML()
            }, html = TRUE, minWidth = 300),
            Democratic = colDef(name = 'Dem. Sponsors', minWidth = 80),
            Republican = colDef(name = 'GOP Sponsors', minWidth = 80),
            passed_house = colDef(name = 'Passed House', cell = function(value) if_else(value == 1, '\u2714\ufe0f', '\u274c'), minWidth = 75),
            passed_senate = colDef(name = 'Passed Senate', cell = function(value) if_else(value == 1, '\u2714\ufe0f', '\u274c'), minWidth = 75),
            passed = colDef(name = 'Passed Both', cell = function(value) if_else(value == 1, '\u2714\ufe0f', '\u274c'), minWidth = 75)
          ),
          details = function(index){
            data_table$bill_summary[index]
          },
          searchable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 20,
          onClick = 'select',
          selection = 'single',
          theme = reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #3c8dbc")
          ))
      }
    })
    
    output$select_bill_ui <- renderUI({
      opts <- l_md() %>% 
        pull(bill_num) %>% 
        unique() %>% 
        str_to_upper()
      selectInput(ns('select_bill'), 'Select Bills', opts)
    })
    
    output$view_bill_lrc <- renderUI({
      if(length(input$select_bill) > 0 &
         length(input$select_session) > 0){
        yr <- input$select_session %>% str_extract("\\d{4}") %>% str_sub(3)
        link <- str_glue('https://apps.legislature.ky.gov/record/{yr}rs/{input$select_bill}.html')
        HTML(str_glue('<h2><a href= "{link}" target="_blank">View Bill at LRC</a></h2>'))
      }
    })
    
    output$bill_detail_header <- renderUI({
      if(!is.null(input$select_bill)){
        title <- l_md() %>% 
          filter(bill_num == str_to_lower(!!input$select_bill)) %>% 
          pull(title) %>% 
          paste0('<h4>',.,'</h4>')
        
        spon <- tbl(db, 'legislative_sponsors') %>% 
          filter(session == !!input$select_session,
                 bill_num == str_to_lower(!!input$select_bill)) %>% 
          select(legislator_id, chief_sponsor) %>% 
          left_join(tbl(db, 'legislative_legislators') %>% 
                      select(legislator_id, name, party)) %>% 
          collect() %>% 
          arrange(desc(chief_sponsor), legislator_id) %>% 
          mutate(name = case_when(chief_sponsor == '1' ~ paste0(name,'*'),
                                  T ~ name)) %>% 
          rownames_to_column() %>% 
          mutate(name = ifelse(as.numeric(rowname) == max(as.numeric(rowname)), name, paste0(name, ', '))) %>%
          mutate(markup = case_when(party == 'Democratic' ~ as.character(str_glue('<span style="color:blue">{name}</span>')),
                                    party == 'Republican' ~ as.character(str_glue('<span style="color:red">{name}</span>')))) %>% 
          pull(markup) %>% 
          paste(., collapse = '') %>% 
          paste0('<p style="text-align:center">',.,'</p>')
        
        summ <- l_md() %>% 
          filter(bill_num == str_to_lower(!!input$select_bill)) %>% 
          pull(bill_summary) %>% 
          paste0('<p style="text-align:center">',.,'</p>')
        HTML(title, spon, summ)
      }
    })
    
    output$bill_detail_actions <- renderReactable({
      if(!is.na(input$select_session) & !is.na(input$select_bill)){
        tbl(db, 'legislative_actions') %>% 
          filter(session == !!input$select_session,
                 bill_num == str_to_lower(!!input$select_bill)) %>% 
          select(date, action) %>% 
          collect() %>% 
          rowid_to_column() %>% 
          arrange(desc(rowid)) %>% 
          select(-rowid) %>% 
          reactable(columns = list(
            date = colDef(name = 'Date', minWidth = 66),
            action = colDef(name = 'Action', minWidth = 300)
          ),
          filterable = T,
          defaultPageSize = 20)
      }
    })
    
    output$bill_house_vote_summary <- renderReactable({
      if(!is.na(input$select_session) & !is.na(input$select_bill)){
        tbl(db, 'legislative_votes') %>% 
          filter(session == !!input$select_session,
                 bill_num == str_to_lower(!!input$select_bill),
                 chamber == 'house') %>% 
          left_join(tbl(db, 'legislative_legislators') %>% 
                      select(legislator_id, name, party))  %>% 
          select(name, party, vote) %>% 
          arrange(name) %>% 
          collect() %>% 
          count(party, vote) %>% 
          group_by(vote) %>% 
          mutate(Total = sum(n)) %>% 
          pivot_wider(names_from = party, values_from = n) %>% 
          replace_na(list(Democratic = 0, Republican = 0)) %>% 
          mutate(ct = case_when(vote == 'YEA' ~ 1,
                                vote == 'NAY' ~ 2,
                                T ~ 3)) %>% 
          arrange(ct) %>% 
          select(-ct) %>% 
          reactable(columns = list(
            vote = colDef(name = 'Vote')
          ))
      }
    })
    
    output$bill_house_votes <- renderReactable({
      if(!is.na(input$select_session) & !is.na(input$select_bill)){
        tbl(db, 'legislative_votes') %>% 
          filter(session == !!input$select_session,
                 bill_num == str_to_lower(!!input$select_bill),
                 chamber == 'house') %>% 
          left_join(tbl(db, 'legislative_legislators') %>% 
                      select(legislator_id, name, party)) %>% 
          select(name, party, vote) %>% 
          arrange(name) %>% 
          collect() %>% 
          reactable(columns = list(
            name = colDef(name = 'Name'),
            party = colDef(name = 'Party'),
            vote = colDef(name = 'Vote', cell = function(value) case_when(value == 'YEA' ~ '\u2714\ufe0f YEA',
                                                                          value == 'NAY' ~ '\u274c NAY',
                                                                          T ~ value))
          ),
          filterable = TRUE,
          defaultPageSize = 100)
      }
    })
    
    output$bill_senate_vote_summary <- renderReactable({
      if(!is.na(input$select_session) & !is.na(input$select_bill)){
        tbl(db, 'legislative_votes') %>% 
          filter(session == !!input$select_session,
                 bill_num == str_to_lower(!!input$select_bill),
                 chamber == 'senate') %>% 
          left_join(tbl(db, 'legislative_legislators') %>% 
                      select(legislator_id, name, party))  %>% 
          select(name, party, vote) %>% 
          arrange(name) %>% 
          collect() %>% 
          count(party, vote) %>% 
          group_by(vote) %>% 
          mutate(Total = sum(n)) %>% 
          pivot_wider(names_from = party, values_from = n) %>% 
          replace_na(list(Democratic = 0, Republican = 0)) %>% 
          mutate(ct = case_when(vote == 'YEA' ~ 1,
                                vote == 'NAY' ~ 2,
                                T ~ 3)) %>% 
          arrange(ct) %>% 
          select(-ct) %>% 
          reactable(columns = list(
            vote = colDef(name = 'Vote')
          ))
      }
    })
    
    output$bill_senate_votes <- renderReactable({
      if(!is.na(input$select_session) & !is.na(input$select_bill)){
        tbl(db, 'legislative_votes') %>% 
          filter(session == !!input$select_session,
                 bill_num == str_to_lower(!!input$select_bill),
                 chamber == 'senate') %>% 
          left_join(tbl(db, 'legislative_legislators') %>% 
                      select(legislator_id, name, party)) %>% 
          select(name, party, vote) %>% 
          arrange(name) %>% 
          collect() %>% 
          reactable(columns = list(
            name = colDef(name = 'Name'),
            party = colDef(name = 'Party'),
            vote = colDef(name = 'Vote', cell = function(value)  case_when(value == 'YEA' ~ '\u2714\ufe0f YEA',
                                                                           value == 'NAY' ~ '\u274c NAY',
                                                                           T ~ value))
          ),
          filterable = TRUE,
          defaultPageSize = 100)
      }
    })
    
    observeEvent(getReactableState('bill_overview_table', 'selected'), {
      updateTabsetPanel(session = getDefaultReactiveDomain(), 'leg_tabs', 'bill_details')
      updateSelectInput(session, 'select_bill', selected = str_to_upper(l_md()$bill_num[getReactableState('bill_overview_table', 'selected')]))
    })
    
    output$select_legislator_ui <- renderUI({
      if(!is.null(input$select_session)){
        session_selected <- case_when(input$select_session == "2024 General Assembly" ~ 'ga_24',
                                      input$select_session == "2023 General Assembly" ~ 'ga_23',
                                      input$select_session == "2022 General Assembly" ~ 'ga_22',
                                      input$select_session == "2021 General Assembly" ~ 'ga_21',
                                      input$select_session == "2020 General Assembly" ~ 'ga_20',
                                      input$select_session == "2019 General Assembly" ~ 'ga_19',
                                      input$select_session == "2018 General Assembly" ~ 'ga_18',
                                      input$select_session == "2017 General Assembly" ~ 'ga_17')
        
        opts_df <- tbl(db, 'legislative_legislators') %>% 
          rename(leg_name = name) %>% 
          pivot_longer(cols = starts_with('ga')) %>% 
          collect() %>% 
          filter(name == session_selected,
                 value == 1) %>% 
          select(leg_name, legislator_id, chamber)
        
        
        opts <- opts_df$legislator_id
        names(opts) <- paste0(opts_df$leg_name,' (',opts_df$chamber,')')
        
        selectInput(ns('select_legislator'), 'Select Legislator', opts)
      }
    })
    
    output$legislator_descrip <- renderUI({
      if(!is.null(input$select_legislator)){
        leg_desc_df <- tbl(db, 'legislative_legislators') %>% 
          filter(legislator_id == !!input$select_legislator) %>% 
          collect()
        HTML(str_glue('<h3><table>
                        <tr><td><strong>Name:</strong></td><td>{leg_desc_df$full_name}</td></tr>
                        <tr><td><strong>Party:</strong></td><td>{leg_desc_df$party}</td></tr>
                        <tr><td><strong>Chamber:</strong></td><td>{leg_desc_df$chamber}</td></tr>
                        <tr><td><strong>District:</strong></td><td>{leg_desc_df$district}</td></tr>
                      </table></h3>'))
      }
    })
    
    output$legislator_img <- renderUI({
      if(!is.null(input$select_legislator)){
        leg_img_url <- tbl(db, 'legislative_legislators') %>% 
          filter(legislator_id == !!input$select_legislator) %>% 
          select(img_src) %>% 
          collect() %>% 
          pull(img_src)
        leg_url <- tbl(db, 'legislative_legislators') %>% 
          filter(legislator_id == !!input$select_legislator) %>% 
          select(lrc_link) %>% 
          collect() %>% 
          pull(lrc_link)
        if(!is.na(leg_img_url)){
          HTML(str_glue('<div><img src="https://legislature.ky.gov{leg_img_url}" /></div>
                        <div><h4><a href="{leg_url}">See Legislator on LRC</a></h4></div>'))
        }
      }
    })
    
    output$legislator_votes <- renderReactable({
      if(!is.null(input$select_legislator)){
        tbl(db, 'legislative_votes') %>% 
          filter(legislator_id == !!input$select_legislator,
                 session == !!input$select_session) %>% 
          select(bill_num, vote) %>% 
          mutate(bill_num = str_to_upper(bill_num)) %>%
          collect() %>%
          rowwise() %>% 
          mutate(bill_numeric = unlist(str_extract_all(bill_num, '\\d')) %>% paste(collapse = '') %>% as.numeric()) %>% 
          arrange(bill_numeric) %>% 
          select(-bill_numeric) %>% 
          ungroup() %>% 
          reactable(columns = list(
            .selection = colDef(show = FALSE),
            bill_num = colDef(name = 'Bill Number'),
            vote = colDef(name = 'Vote', cell = function(value) if_else(value == 'YEA', '\u2714\ufe0f YEA', '\u274c NAY'))
          ),
          filterable = T,
          defaultPageSize = 50,
          onClick = 'select',
          selection = 'single',
          theme = reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #3c8dbc")
          ))
      }
    })
    
    output$legislator_sponsorship <- renderReactable({
      if(!is.null(input$select_legislator)){
        if(input$select_session == '2022 General Assembly'){
          bn_row_name <- 'Bill Number (* Chief Sponsor)'
        }else{
          bn_row_name <- 'Bill Number'
        }
        tbl(db, 'legislative_sponsors') %>% 
          filter(legislator_id == !!input$select_legislator,
                 session == !!input$select_session)  %>%
          left_join(tbl(db, 'legislative_metadata') %>% 
                      filter(session == !!input$select_session)) %>% 
          collect() %>% 
          mutate(bill_num = case_when(chief_sponsor == '1' ~ paste0(bill_num,'*'),
                                      T ~ bill_num) %>% str_to_upper()) %>% 
          select(bill_num, title) %>% 
          reactable(columns = list(
            .selection = colDef(show = FALSE),
            bill_num = colDef(name = bn_row_name),
            title = colDef(name = 'Bill Title')
          ),
          filterable = T,
          defaultPageSize = 50,
          onClick = 'select',
          selection = 'single',
          theme = reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #3c8dbc")
          ))
          
      }
    })
    
    observeEvent(getReactableState('legislator_votes', 'selected'), {
      sel_df <- tbl(db, 'legislative_votes') %>% 
        filter(legislator_id == !!input$select_legislator,
               session == !!input$select_session) %>% 
        collect()%>%
        rowwise() %>% 
        mutate(bill_numeric = unlist(str_extract_all(bill_num, '\\d')) %>% paste(collapse = '') %>% as.numeric()) %>% 
        arrange(bill_numeric) %>% 
        select(-bill_numeric)
      updateTabsetPanel(session = getDefaultReactiveDomain(), 'leg_tabs', 'bill_details')
      updateSelectInput(session, 'select_bill', selected = str_to_upper(sel_df$bill_num[getReactableState('legislator_votes', 'selected')]))
    })
    
    observeEvent(getReactableState('legislator_sponsorship', 'selected'), {
      sel_df <-tbl(db, 'legislative_sponsors') %>% 
        filter(legislator_id == !!input$select_legislator,
               session == !!input$select_session)  %>%
        left_join(tbl(db, 'legislative_metadata') %>% 
                    filter(session == !!input$select_session)) %>% 
        select(bill_num, title) %>% 
        collect()
      updateTabsetPanel(session = getDefaultReactiveDomain(), 'leg_tabs', 'bill_details')
      updateSelectInput(session, 'select_bill', selected = str_to_upper(sel_df$bill_num[getReactableState('legislator_sponsorship', 'selected')]))
    })
    
    output$select_day_actions_ui <- renderUI({
      if(!is.null(input$select_session)){
        sels <- tbl(db, 'legislative_actions') %>% 
          filter(session == !!input$select_session) %>%
          # filter(session == '2022 General Assembly') %>% 
          select(date) %>% 
          distinct() %>% 
          collect() %>% 
          mutate(yr = year(date)) %>% 
          filter(yr == as.numeric(str_remove_all(!!input$select_session, '\\D'))) %>%
          # filter(yr == as.numeric(str_remove_all('2022 General Assembly', '\\D'))) %>% 
          pull(date) %>% 
          sort(decreasing  = T)
          
        selectInput(ns('select_day_actions'), 'Select Date', sels)
      }
    })
    
    output$actions_actions_reactable <- renderReactable({
      if(!is.null(input$select_session) & !is.null(input$select_day_actions)){
        tbl(db, 'legislative_actions') %>% 
          filter(session == !!input$select_session,
                 date == !!input$select_day_actions) %>%
          mutate(bill_num = str_to_upper(bill_num)) %>% 
          select(bill_num, action) %>% 
          collect() %>% 
          reactable(columns = list(
            .selection = colDef(show = FALSE),
            bill_num = colDef(name = 'Bill Number'),
            action = colDef(name = 'Action')
          ),
          filterable = T,
          defaultPageSize = 50,
          onClick = 'select',
          selection = 'single',
          groupBy = 'bill_num',
          theme = reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #3c8dbc")
          ))
      }
    })
    
  })
  
}

## To be copied in the UI
# mod_legislation_ui("legislation_ui_1")

## To be copied in the server
# mod_legislation_server("legislation_ui_1")
