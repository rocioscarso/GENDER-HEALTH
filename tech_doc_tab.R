###Code for technical document tab

#################################################.
##  Technical Doc Page ----
#################################################.
#### Techdoc for summary of indicators.[grep(input$profile_picked,optdata$profile_domain1)]),

output$tecdoc_list <- renderUI ({
  
  if (input$profile_picked != "Show all"){
    selection  <- sort(unique(c(as.character(optdata$profile[grep(input$profile_picked)]))))
  } 
  #else {
   # selection <- sort(unique(c(as.character(optdata$indicator[grep(input$profile_picked)]))))
    
  #}
  
  
}) 


output$profile_picked_ui <- renderUI({
  
  if (input$techdoc_selection == "List of available indicators") {
    label_filter <- "Step 2. Select a profile to see indicators included on it (optional)"
    div_title <- "Filter table selecting only indicators available for a specific profile"
    
  } #else if (input$techdoc_selection == "Detailed information about single indicator") {
    #label_filter <- "Step 2. Filter indicator list selecting a single profile (optional)"
    #div_title <- "Filter indicator list"
  #}
  
  div(title= div_title, 
      selectizeInput("profile_picked", label = label_filter,
                     width = "100%",choices = profile_list_filter, 
                     selected = "Show all", multiple=FALSE))
})

###############################################.
# Reactive dataset filtered for flextable 
techdoc_indicator_data <- reactive({  
  
  if (input$profile_picked == "Show all"){ # if a single profile selected
    techdoc%>%dplyr::select (profile, indicator_name, indicator_definition)
    
  }else if (input$profile_picked == "socieconomic"){
    techdoc%>%dplyr::select (profile, indicator_name, indicator_definition)%>%filter(profile=='socieconomic')
    
  }else if (input$profile_picked == "health"){
    techdoc%>%dplyr::select (profile, indicator_name, indicator_definition)%>%filter(profile=='health')
  }
})


###############################################.

## Function to manipulate filtered data - easier for data download if manipulations done after filters
#formatted_techdoc_data <- function(){
#  if (input$profile_picked != "Show all"){
#    techdoc_indicator_data() %>%
#      mutate(prof_start=regexpr((names(profile_list[unname(profile_list) == input$profile_picked])), profile))#find start position of profile name in domain column
#             prof_name_text=substr(profile,prof_start, nchar(profile))  #generate column that starts with filtered profile
#             } 
#  else{
#    techdoc_indicator_data()}}

## Function to construct flextable displaying techdoc info
plot_techdoc <- function(){
  
  if (input$profile_picked == "Show all"){ # table for a single profile selection
    techdoc_indicator_data() %>%
      dplyr::select(profile, indicator_name, indicator_definition) %>%
      flextable() %>%
      set_header_labels(profile="Profile", indicator_name="Indicator", indicator_definition = "Indicator Definition") %>%
      theme_box() %>%
      merge_v(j = ~ profile)%>%
      fontsize(size = 14, part = "all") %>% 
      autofit() %>%
      htmltools_value()
    
  } else if (input$profile_picked == "socieconomic"){ #table all indicators (ie "show all") profiles selected - additional column for profile(s)
    techdoc_indicator_data() %>%
      dplyr::select(indicator_name, indicator_definition) %>%
      flextable() %>%
      add_header_lines("Socioeconomic profile")%>%
      set_header_labels(indicator_name="Indicator",indicator_definition="Indicator Definition") %>%
      theme_box() %>%
      bg(i=1,bg="#00aae4",part="header") %>%
      fontsize(size = 14, part = "all") %>% 
      autofit() %>%
      htmltools_value()
    
  } else if (input$profile_picked == "health"){ #table all indicators (ie "show all") profiles selected - additional column for profile(s)
    techdoc_indicator_data() %>%
      dplyr::select (indicator_name, indicator_definition) %>%
      flextable() %>%
      add_header_lines(" Health profile")%>%
      set_header_labels(indicator_name="Indicator",indicator_definition="Indicator Definition") %>%
      theme_box() %>%
      fontsize(size = 14, part = "all") %>%
      bg(i=1,bg="#00aae4",part="header") %>%
      autofit() %>%
      htmltools_value()}
}

## RenderUI for which version flextable to display on techdoc page
#render techincal info depending on whether selected to see summary of 
# available indictors or single indicator definition
output$techdoc_display <- renderUI({  
  # Preparing a brief explanation for each visualisation 
  if (input$techdoc_selection == "List of available indicators") {
    plot_techdoc()
  } else if (input$techdoc_selection == "Detailed information about single indicator")
    p("loading..")}  #shows 'loading' as there can be a small delay while switching back between views
)

## Function to format data for csv according to whether showing single profile or all profiles
techdoc_csv <- function() {
  if (input$profile_picked != "Show all"){
    techdoc_indicator_data() %>%
      rename(profile_selection=profilename, all_profiles=profile) %>%  
      select(c(profile_selection, indicator_name, indicator_number, indicator_definition, all_profiles, data_source,
               measure, trends_from, update_frequency, supporting_information, last_updated))}
  else { #table all indicators (ie "show all") profiles selected - additional column for profile(s)
    techdoc_indicator_data() %>%
      select(c(indicator_name, indicator_number, indicator_definition, profile, data_source,
               measure, trends_from, update_frequency,supporting_information, last_updated))}
}

## Download techdoc data from conditional panel (flextable)
output$download_techdoc1_csv <- downloadHandler(
  filename ="indicator_definitions.csv",
  content = function(file) {
    write.csv(techdoc_csv(),
              file, row.names=FALSE) } 
)

#################################################.  
#### Techdoc for individual indicator.

## Reactive filters for technical details document - filter for indicator dependent on profile/topic selected
output$indicator_choices <- renderUI ({
  
  if (input$profile_picked != "Show all"){
    indic_selection <- sort(unique(c(as.character(optdata$indicator[grep(input$profile_picked)]))))
  }  else {indic_selection <- indicator_list}
  
  selectizeInput("indicator_selection", 
                 label = shiny::HTML("<p>Step 2. Select an indicator for detailed technical information <br/> <span style='font-weight: 400'>(hit backspace and start typing to search for an indicator)</span></p>"),
                 width = "510px", choices = indic_selection, 
                 selected = character(0), multiple=TRUE, 
                 options = list(placeholder = "Make a selection to see information", maxItems = 1)) 
}) 
###############################################.
# Creating text and titles for info to display

#reactive selection for single indicator
indicator_selected <- reactive({ 
  filter(techdoc, techdoc$indicator_name==input$indicator_selection)
})

#Text for title of indicator selected
output$indicator <- renderValueBox({
  valueBox(p(indicator_selected()$indicator_name, style="color: white; font-size: 30px; font-weight: bold;"), 
           HTML(paste("<b>","Profile:","</b>",indicator_selected()$profile)), icon = icon ("book"),color = "blue")
})
# Text for all metadata parts of the indicator
output$indicator_definition <- renderText ({indicator_selected()$indicator_definition})
output$data_source <- renderText ({indicator_selected()$data_source})
output$measure <- renderText ({indicator_selected()$measure})
output$trends_from <- renderText ({indicator_selected()$trends_from})
output$last_updated <- renderText ({indicator_selected()$last_updated})
output$update_frequency <- renderText ({indicator_selected()$update_frequency})
output$supporting_info <- renderText ({indicator_selected()$supporting_information})


## Download techdoc data from 2nd conditional panel (detailed indicator)
#Download definitions table for selected indicator 

indicator_csv <- reactive({ format_definitions_csv(indicator_selected()) })

allindicator_csv <- reactive({ format_definitions_csv(techdoc)})

output$download_detailtechdoc_csv <- downloadHandler(
  filename ="indicator_definitions.csv",
  content = function(file) {
    write.csv(indicator_csv(),
              file, row.names=FALSE) } 
)

output$download_alltechdoc_csv <- downloadHandler(
  filename ="indicator_definitions.csv",
  content = function(file) {
    write.csv(allindicator_csv(),
              file, row.names=FALSE) } 
)