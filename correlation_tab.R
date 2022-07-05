###############################################.
## Summary - common objects ----
###############################################.
# Summary help pop-up
observeEvent(input$help_summary, {
  
  
  showModal(modalDialog(
    title = "How to use this chart",
    p(column(8, 
             
             p("Select the two indicators and the year you want to plot on the maps."),
             p("This visualisation shows a coropleth maps with the level of association betweenor the socieconomic indicator and health indicator you
      have chosen. The intensity of the colors in each region reflects whether the differences between women an men are significant or not. 
      Hover over the regions to see the ratio and the year selected."),
             p("Hover over each region in the map to see the indicator values for the year selected.")
    ),
    br()),
    size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
  ))
  
})

###############################################.
# Indicator definitions
#Subsetting by profile. Profile is fiddly as vector uses abbreviations 
# so needs to be converted to the names to match techdoc.
defs_data_vi <- reactive({
  
  techdoc %>% 
    subset(grepl(names(profile_list[unname(profile_list) == input$profile_summary]), profile))
  
})

output$defs_text_vi <- renderUI({
  
  HTML(paste(sprintf("<b><u>%s</b></u> <br> %s ", defs_data_vi()$indicator_name, 
                     defs_data_vi()$indicator_definition), collapse = "<br><br>"))
})


#Dropdown for time period based on indicator selection  
output$yearSE_ui_vi<- renderUI({
  time<- sort(unique(optdata_ratio$year[optdata_ratio$indicator == input$indic_se_vi]))
  selectInput("year_se_vi",label= shiny::HTML("<p>Step 1.1. Select year: <br/> <span style='font-weight: 400'></span></p>"),
              choices = time, selected = last(time))
})

output$yearH_ui_vi<- renderUI({
  time<- sort(unique(optdata_ratio$year[optdata_ratio$indicator == input$indic_health_vi]))
  selectInput("year_health_vi",label= shiny::HTML("<p>Step 2.1. Select year: <br/> <span style='font-weight: 400'></span></p>"),
              choices = time, selected = last(time))
})

#####################.
# Reactive controls
## Remember the selected samples
# creates reactive values to remember user selection of the profile
# so it only changes when the user changes it on purpose

#Reactive expressions for the years available for the indicators the user selected



###############################################.
# This will create a reactive user interface depending on type of visualization 
# and profile selected

output$summary_expl_vi <- renderUI({
  # Preparing a brief explanation for each visualisation 
  p("This visualisation shows a coropleth maps with the women/men's ratio calculated for the socieconomic indicator and health indicator you
      have chosen. The intensity of the colors in each comunity reflects whether the differences between women an men are significant or not. 
      Hover over the regions to see the ratio and the year selected.")
  
})

########### MAPS

output$mapvi <- renderPlotly({
  
  
  data_map_vi<- optdata_ratio%>%filter(indicator==input$indic_se_vi)%>%filter(year==input$year_se_vi)
  
  CCAA_sf_s <- sf::st_cast(esp_get_ccaa(), "MULTIPOLYGON")
  CCAA_sf_s<- left_join(CCAA_sf, data_map_vi)
  
  map_SE_vi<-ggplot(CCAA_sf_s) +
    geom_sf(aes(fill = ratio, text = paste("CCAA: <b>",areaname, "</b> level of asiciation between indicators \n", ratio)), color = "grey70") +
    scale_fill_gradientn(colors = hcl.colors(10, "YlGnBu", rev = TRUE)) +
    geom_sf(data = esp_get_can_box(), color = "grey70") +
    labs(title ="Results: Level of asociation")+
    theme_void()
  
  map_SE_vi%>%
    ggplotly(tooltip = "text") 
  
})

#############################################

