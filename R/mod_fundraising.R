#' fundraising UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fundraising_ui <- function(id){
  ns <- NS(id)
  tabItem(tabName = 'fundraising', 
          fluidRow(
            box(width = 9, 
                column(width = 3, uiOutput(ns('select_election_ui'))),
                column(width = 3,uiOutput(ns('select_office_ui'))),
                column(width = 3,uiOutput(ns('select_candidate_ui'))))
          ),
          fluidRow(
            box(width = 12,
                tabsetPanel(id = ns('fundraise_tabs'),
                            tabPanel(title = 'Summary',
                                     column(width = 3,
                                            reactableOutput(ns('summary_table'))),
                                     column(width = 9,
                                            fluidRow(box(width = 12, selectInput(ns('graph_sel'), 'Select Graph', 
                                                                                 c('Burn Rate', 'Contributor Distribution', 
                                                                                   'Kentucky Giving by County', 'Out of State Giving'))
                                                         )
                                                     ),
                                            fluidRow(
                                              uiOutput(ns('fundraise_graphic'))
                                            )
                                            )),
                            tabPanel(title = 'Contributions',
                                     reactableOutput(ns('contributions_table'))),
                            tabPanel(title = 'Expenditures',
                                     reactableOutput(ns('expenditures_table'))))
            
              
            )
            )
  )
}
    
#' fundraising Server Functions
#'
#' @noRd 
mod_fundraising_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$select_election_ui <- renderUI({
      elec_opts_tib <- tbl(db, 'fundraising_contrib') %>%
        collect() %>% 
        mutate(year = year(election_date), 
               election_select = paste(str_to_title(election_type), year)) %>%
        select(election_select, election_date) %>% 
        distinct() %>% 
        group_by(election_date) %>%
        mutate(n = 1:n()) %>% 
        filter(n == 1) %>% 
        select(-n) %>% 
        ungroup() 
      elect_opts <- elec_opts_tib$election_date
      names(elect_opts) <- elec_opts_tib$election_select
      selectInput(ns('select_election'), 'Select Election', elect_opts)
    })
    
    output$select_office_ui <- renderUI({
      office_opts <- tbl(db, 'fundraising_contrib') %>%
        collect() %>%
        filter(election_date == !!input$select_election) %>%
        # filter(election_date == '2023-05-16') %>% 
        pull(office_sought) %>% 
        unique() %>% 
        str_to_title()
      
      selectInput(ns('select_office'), 'Select Office', office_opts)
    })
    
    output$select_candidate_ui <- renderUI({
      candidate_opts_tbl <- tbl(db, 'fundraising_contrib') %>%
        collect() %>%
        filter(election_date == !!input$select_election,
               office_sought == str_to_upper(!!input$select_office)) %>%
        # filter(election_date == '2023-05-16',
        #        office_sought == str_to_upper('Governor')) %>% 
        mutate(candidate_name = paste(recipient_first_name, recipient_last_name)) %>%
        select(candidate_name, candidate) %>% 
        distinct() 
      candidate_opts <- candidate_opts_tbl$candidate
      names(candidate_opts) <- candidate_opts_tbl$candidate_name
      
      selectInput(ns('select_candidate'), 'Select Candidate', candidate_opts)
    })
    
    output$contributions_table <- renderReactable({
      tbl(db, 'fundraising_contrib') %>%
        filter(election_date == !!input$select_election,
               office_sought == str_to_upper(!!input$select_office),
               candidate == !!input$select_candidate) %>%
        # filter(election_date == '2023-05-16',
        #        office_sought == str_to_upper('Governor'),
        #        candidate == 'BESHEAR_ANDY') %>% 
        
        collect() %>%
        mutate(contributor = case_when(is.na(from_organization_name) & is.na(contributor_last_name) ~ NA_character_,
                                       is.na(from_organization_name) ~ paste(contributor_first_name, contributor_last_name),
                                       T ~from_organization_name)) %>% 
        select(contributor, amount, receipt_date, city, state, zip, occupation, employer, contribution_mode, contribution_type) %>% 
        arrange(desc(receipt_date)) %>% 
        reactable(columns = list(
          contributor = colDef(name = 'Contributor'),
          amount = colDef(name = 'Amount', format = colFormat(prefix = '$', separators = T, digits = 0)),
          receipt_date = colDef(name = 'Date of Receipt'),
          city = colDef(name = 'Contributor City'),
          state = colDef(name = 'Contributor State'),
          zip = colDef(name = 'Contributor Zip'),
          occupation = colDef(name = 'Contributor Occupation'),
          employer = colDef(name = 'Contributor Employer'),
          contribution_mode = colDef(name = 'Mode of Contribution'),
          contribution_type = colDef(name = 'Type of Contribution')
        ), filterable = TRUE, defaultPageSize = 25, searchable = TRUE)
    })
    output$expenditures_table <- renderReactable({
      tbl(db, 'fundraising_expend') %>%
        filter(election_date == !!input$select_election,
               office_sought == str_to_upper(!!input$select_office),
               candidate == !!input$select_candidate) %>%
        # filter(election_date == '2023-05-16',
        #        office_sought == str_to_upper('Governor'),
        #        candidate == 'BESHEAR_ANDY') %>%
        
        collect() %>%
        mutate(recipient = case_when(is.na(organization_name) & is.na(recipient_last_name) ~ NA_character_,
                                       is.na(organization_name) ~ paste(recipient_first_name, recipient_last_name),
                                       T ~ organization_name),
               disbursement_date = as.Date(disbursement_date)) %>% 
        select(recipient, disbursement_amount, disbursement_date, purpose, occupation, disbursement_code) %>% 
        arrange(desc(disbursement_date)) %>% 
        reactable(columns = list(
          recipient = colDef(name = 'Recipient'),
          disbursement_amount = colDef(name = 'Amount', format = colFormat(prefix = '$', separators = T, digits = 0)),
          disbursement_date = colDef(name = 'Date of Disbursement'),
          occupation = colDef(name = 'Occupation of Recipient'),
          purpose = colDef(name = 'Purpose of Disbursement'),
          disbursement_code = colDef(name = 'Disbursement Code')
        ), filterable = TRUE, defaultPageSize = 25, searchable = TRUE)
    })
    
    output$summary_table <- renderReactable({
      candidate_df <- tbl(db, 'fundraising_contrib') %>%
        filter(election_date == !!input$select_election,
               office_sought == str_to_upper(!!input$select_office),
               candidate == !!input$select_candidate) %>%
        # filter(election_date == '2023-05-16',
        #        office_sought == str_to_upper('Governor'),
        #        candidate == 'BESHEAR_ANDY') %>%
        collect()
      
      candidate_disburse_df <- tbl(db, 'fundraising_expend') %>% 
        filter(election_date == !!input$select_election,
               office_sought == str_to_upper(!!input$select_office),
               candidate == !!input$select_candidate) %>%
        # filter(election_date == '2023-05-16',
        #        office_sought == str_to_upper('Governor'),
        #        candidate == 'BESHEAR_ANDY') %>%
        collect()
      
      total_raise <- candidate_df %>% 
        collect() %>% 
        pull(amount) %>% 
        sum()
      
      total_spend <- candidate_disburse_df %>%
        collect() %>% 
        pull(disbursement_amount) %>% 
        sum()
      
      unique_contrib <- candidate_df %>% 
        mutate(contributor = case_when(is.na(from_organization_name) & is.na(contributor_last_name) ~ NA_character_,
                                       is.na(from_organization_name) ~ paste(contributor_first_name, contributor_last_name, address1),
                                       T ~from_organization_name)) %>% 
        count(contributor) %>% 
        nrow()
      
      avg_contrib <- candidate_df %>% 
        pull(amount) %>% 
        mean()
      
      ky_raise <- candidate_df %>% 
        filter(state == 'KY') %>% 
        pull(amount) %>% 
        sum() 
      
      individual_max_contribs <- candidate_df %>% 
        mutate(contributor = case_when(is.na(from_organization_name) & is.na(contributor_last_name) ~ NA_character_,
                                       is.na(from_organization_name) ~ paste(contributor_first_name, contributor_last_name, address1),
                                       T ~from_organization_name)) %>%
        filter(contribution_type == 'INDIVIDUAL') %>% 
        group_by(contributor) %>% 
        summarize(amount = sum(amount)) %>% 
        filter(amount >= 2000) %>% 
        nrow()
      
      pac_contrib <- candidate_df %>% 
        mutate(contributor = case_when(is.na(from_organization_name) & is.na(contributor_last_name) ~ NA_character_,
                                       is.na(from_organization_name) ~ paste(contributor_first_name, contributor_last_name, address1),
                                       T ~from_organization_name)) %>%
        filter(contribution_type == 'KYPAC') %>% 
        pull(amount) %>% 
        sum()
      
      tibble(
        total_raise = total_raise,
        total_spend = total_spend,
        unique_contrib = unique_contrib,
        avg_contrib = avg_contrib,
        ky_raise = ky_raise,
        individual_max_contribs = individual_max_contribs,
        pac_contrib = pac_contrib
      ) %>% 
        mutate(cash_on_hand = scales::dollar(round(total_raise - total_spend)),
               total_raise = scales::dollar(round(total_raise)),
               total_spend = scales::dollar(round(total_spend)),
               unique_contrib = scales::comma(unique_contrib),
               avg_contrib = scales::dollar(avg_contrib),
               ky_raise = scales::dollar(round(ky_raise)),
               individual_max_contribs = scales::comma(individual_max_contribs),
               pac_contrib = scales::dollar(round(pac_contrib))) %>% 
        select(`Total Amount Raised` = total_raise, `Total Amount Spent` = total_spend, 
               `Cash on Hand` = cash_on_hand, `Unique Contributors` = unique_contrib, 
               `Average Contribution` = avg_contrib, `Contributions from Kentucky` = ky_raise, 
               `Number of $2000 Contributions` = individual_max_contribs, 
               `Total PAC Money Raised` = pac_contrib) %>%
        pivot_longer(everything()) %>% 
        reactable(columns = list(
          name = colDef(name = ''),
          value = colDef(name = '')
        ))
    })
    
    output$fundraise_graphic <- renderUI({
      if(!!input$graph_sel %in% c('Burn Rate', 'Contributor Distribution')){
        plotlyOutput(ns('fundraise_graph'))
      }else if(!!input$graph_sel %in% c('Kentucky Giving by County')){
        leafletOutput(ns('fundraise_leaf'))
      }else if(!!input$graph_sel %in% c('Out of State Giving')){
        reactableOutput(ns('fundraise_oos_table'))
      }
      
    })
    
    output$fundraise_graph <- renderPlotly({
      candidate_df <- tbl(db, 'fundraising_contrib') %>%
        filter(election_date == !!input$select_election,
               office_sought == str_to_upper(!!input$select_office),
               candidate == !!input$select_candidate) %>%
        # filter(election_date == '2023-05-16',
        #        office_sought == str_to_upper('Governor'),
        #        candidate == 'BESHEAR_ANDY') %>%
        collect() 
      
      candidate_disburse_df <- tbl(db, 'fundraising_expend') %>% 
        filter(election_date == !!input$select_election,
               office_sought == str_to_upper(!!input$select_office),
               candidate == !!input$select_candidate) %>%
        # filter(election_date == '2023-05-16',
        #        office_sought == str_to_upper('Governor'),
        #        candidate == 'BESHEAR_ANDY') %>%
        collect()
      
      min_date <- min(c(min(candidate_df$receipt_date, na.rm = T), min(candidate_disburse_df$disbursement_date, na.rm = T)))
      candidate_df <- candidate_df %>% 
        replace_na(list(receipt_date = min_date))
      candidate_disburse_df <- candidate_disburse_df %>% 
        replace_na(list(disbursement_date = min_date))
      
      if(!!input$graph_sel == 'Burn Rate'){
        candidate_df %>% 
          group_by(date = receipt_date) %>% 
          summarize(contributions = sum(amount)) %>% 
          full_join(
            candidate_disburse_df %>% 
              group_by(date = disbursement_date) %>% 
              summarize(disbursement = sum(disbursement_amount))
          ) %>% 
          mutate(date = as.Date(date)) %>% 
          padr::pad() %>% 
          replace_na(list(contributions = 0, disbursement = 0)) %>% 
          mutate(cs_contrib = cumsum(contributions),
                 cs_disburse = cumsum(disbursement)) %>% 
          plot_ly(x = ~date, y = ~cs_contrib, name = 'Contributions', type = 'scatter', mode = 'lines') %>% 
          add_trace(y = ~cs_disburse, name = 'Disbursements', type = 'scatter', mode = 'lines') %>% 
          layout(xaxis = list(title = ''),
                 yaxis = list(title = 'Amount ($)', tickformat = ',d'))
      }else if(!!input$graph_sel == 'Contributor Distribution'){
        candidate_df %>% 
          mutate(contributor = case_when(is.na(from_organization_name) & is.na(contributor_last_name) ~ NA_character_,
                                         is.na(from_organization_name) ~ paste(contributor_first_name, contributor_last_name, address1),
                                         T ~from_organization_name)) %>%
          filter(contribution_type == 'INDIVIDUAL') %>%
          group_by(contributor) %>% 
          summarize(amount = sum(amount)) %>%
          ungroup() %>% 
          plot_ly(y = ~amount, type = "box", boxpoints = "all", jitter = 100) %>% 
          layout(xaxis = list(title = ''),
                 yaxis = list(title = 'Amount ($)', tickformat = ',d'))
        
      }
    })
    
    output$fundraise_leaf <- renderLeaflet({
      candidate_df <- tbl(db, 'fundraising_contrib') %>%
        filter(election_date == !!input$select_election,
               office_sought == str_to_upper(!!input$select_office),
               candidate == !!input$select_candidate) %>%
        # filter(election_date == '2023-05-16',
        #        office_sought == str_to_upper('Governor'),
        #        candidate == 'BESHEAR_ANDY') %>%
        collect()
      
      give_by_coun <- candidate_df %>% 
        group_by(zip) %>% 
        summarize(amount = sum(amount)) %>% 
        left_join(tbl(db, 'ky_zip_county') %>% 
                    collect() %>% 
                    mutate(zip_code = as.character(zip_code)), 
                  by = c('zip' = 'zip_code')) %>% 
        group_by(county) %>% 
        summarize(amount = sum(amount)) %>% 
        ungroup() %>% 
        rowwise() %>% 
        mutate(lab = htmltools::HTML(str_glue('<strong>{county}</strong></br>{scales::dollar(amount)}')))
      
      bins <- quantile(give_by_coun$amount, c(0, .2, .4, .6, .8, 1))
      pal <- colorBin('YlGnBu', domain = give_by_coun$amount, bins = bins)
      ky_counties %>% 
        mutate(NAME = str_to_upper(NAME)) %>% 
        left_join(give_by_coun %>% 
                    mutate(NAME = str_to_upper(county))) %>% 
        leaflet() %>% 
        addTiles() %>% 
        addPolygons(
          fillColor = ~pal(amount),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = ~lab,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>% 
        addLegend(pal = pal, values = ~amount, opacity = 0.7, title = NULL,
                  position = "topright")
    })
    
    output$fundraise_oos_table <- renderReactable({
      tbl(db, 'fundraising_contrib') %>%
        filter(election_date == !!input$select_election,
               office_sought == str_to_upper(!!input$select_office),
               candidate == !!input$select_candidate) %>%
        # filter(election_date == '2023-05-16',
        #        office_sought == str_to_upper('Governor'),
        #        candidate == 'BESHEAR_ANDY') %>%
        collect() %>% 
        filter(state != 'KY') %>% 
        group_by(state) %>% 
        summarize(amount = sum(amount)) %>% 
        arrange(desc(amount)) %>% 
        reactable(columns = list(
          state = colDef(name = 'State'),
          amount = colDef(name = 'Amount',format = colFormat(prefix = '$', separators = T, digits = 0))
        ), filterable = TRUE, defaultPageSize = 20)
    })
 
  })
}
    
## To be copied in the UI
# mod_fundraising_ui("fundraising_ui_1")
    
## To be copied in the server
# mod_fundraising_server("fundraising_ui_1")
