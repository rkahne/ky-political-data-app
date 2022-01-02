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
            box(width = 4, uiOutput(ns('select_session_ui')))
          ),
          tabsetPanel(id = ns('leg_tabs'),
                      tabPanel(title = 'Explore Bills', value = ns('explore_bills'),
                               fluidRow(
                                 box(width = 12, reactableOutput(ns('bill_overview_table')) %>% shinycssloaders::withSpinner())
                               )),
                      tabPanel(title = 'Bill Details', value = ns('bill_details'),
                               fluidRow(
                                 box(width = 12,
                                     fluidRow(
                                       box(width = 4, uiOutput(ns('select_bill_ui')))
                                     ),
                                     fluidRow(
                                       box(width = 12, uiOutput(ns('bill_detail_header')))
                                     ),
                                     fluidRow(
                                       box(width = 12, tabsetPanel(id = ns('leg_bill_details'),
                                                                   tabPanel(title = 'Actions', value = ns('actions'),
                                                                            box(width = 12, reactableOutput(ns('bill_detail_actions')))),
                                                                   tabPanel(title = 'Votes', value = ns('votes'),
                                                                            box(width = 6, title = 'House', 
                                                                                reactableOutput(ns('bill_house_vote_summary')),
                                                                                reactableOutput(ns('bill_house_votes'))),
                                                                            box(width = 6, title = 'Senate', 
                                                                                reactableOutput(ns('bill_senate_vote_summary')),
                                                                                reactableOutput(ns('bill_senate_votes'))))
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
                                 box(width = 12, 
                                     uiOutput(ns('legislator_descrip')),
                                     reactableOutput(ns('legislator_votes')))
                               )
                               )
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
      tbl(db, 'legislative_metadata') %>% 
        filter(session == !!input$select_session) %>% 
        select(bill_num, title, bill_summary, last_action, passed_house, passed_senate, passed) %>% 
        collect()
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
    
    output$bill_overview_table <- renderReactable({
      if(!is.null(input$select_session)){
        tbl(db, 'legislative_metadata') %>% 
          filter(session == !!input$select_session) %>% 
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
          collect()  %>% 
          reactable(columns = list(
            bill_num = colDef(name = 'Bill Number', cell = str_to_upper),
            title = colDef(name = 'Bill Title', minWidth = 300),
            bill_summary = colDef(name = 'Bill Summary', minWidth = 500),
            last_action = colDef(name = 'Last Action'),
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
          searchable = TRUE,
          filterable = TRUE,
          resizable = TRUE,
          defaultPageSize = 20,
          onClick = 'select',
          selection = 'single')
      }
    })
    
    output$select_bill_ui <- renderUI({
      opts <- l_md() %>% 
        pull(bill_num) %>% 
        unique() %>% 
        str_to_upper()
      selectInput(ns('select_bill'), 'Select Bills', opts)
    })
    
    output$bill_detail_header <- renderUI({
      if(!is.null(input$select_bill)){
        title <- l_md() %>% 
          filter(bill_num == str_to_lower(!!input$select_bill)) %>% 
          pull(title) %>% 
          paste0('<h4>',.,'</h4>')
        summ <- l_md() %>% 
          filter(bill_num == str_to_lower(!!input$select_bill)) %>% 
          pull(bill_summary) %>% 
          paste0('<p style="text-align:center">',.,'</p>')
        HTML(title, summ)
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
            vote = colDef(name = 'Vote', cell = function(value) if_else(value == 'YEA', '\u2714\ufe0f YEA', '\u274c NAY'))
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
            vote = colDef(name = 'Vote', cell = function(value) if_else(value == 'YEA', '\u2714\ufe0f YEA', '\u274c NAY'))
          ),
          filterable = TRUE,
          defaultPageSize = 100)
      }
    })
    
    observeEvent(getReactableState('bill_overview_table', 'selected'), {
      # browser()
      updateTabsetPanel(session = getDefaultReactiveDomain(), 'leg_tabs', 'bill_details')
      updateSelectInput(session, 'select_bill', selected = str_to_upper(l_md()$bill_num[getReactableState('bill_overview_table', 'selected')]))
    })
    
    output$select_legislator_ui <- renderUI({
      if(!is.null(input$select_session)){
        session_selected <- case_when(input$select_session == "2021 General Assembly" ~ 'ga_21',
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
        HTML(str_glue('<h3>{leg_desc_df$party} - {leg_desc_df$chamber} - District {leg_desc_df$district}</h3>'))
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
            bill_num = colDef(name = 'Bill Number'),
            vote = colDef(name = 'Vote', cell = function(value) if_else(value == 'YEA', '\u2714\ufe0f YEA', '\u274c NAY'))
          ),
          filterable = T,
          defaultPageSize = 50,
          onClick = 'select',
          selection = 'single')
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
 
  })
}
    
## To be copied in the UI
# mod_legislation_ui("legislation_ui_1")
    
## To be copied in the server
# mod_legislation_server("legislation_ui_1")
