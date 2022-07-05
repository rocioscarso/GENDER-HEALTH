###############################################.
## Summary - common objects ----
###############################################.
# Summary 0
observeEvent(input$help_summary, {
    showModal(modalDialog(
      title = "How to use this chart",
      p(column(10, 
               
               p("Select the two indicators and the year you want to plot on the maps."),
               p("This visualisation shows two coropleth maps with the women/men's ratio calculated for the socieconomic indicator and health indicator you
                  have chosen. The intensity of the colors in each region reflects whether the differences between women an men are significant or not. Reds means worse 
                  for women and blues means better for women."),
               p("Hover over each region in the map to see the indicator values for the year selected.")
        ),
        br()),
      size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
    ))

})


#####################.
# titles 
#create title and subtitle variables
output$summary_title <- renderText({
  paste0(input$indic_se, " in year ", input$year_se, " (left map)", " compare to ",
         input$indic_health , " in year ", input$year_health , " (right map)")
})

output$summary_subtitle <- renderText({

    paste0("Ratios calculated as women/men ")
  
})

#####################.

#Dropdown for time period based on indicator selection  
output$yearSE_ui_summary<- renderUI({
  time<- sort(unique(optdata_ratio$year[optdata_ratio$indicator == input$indic_se]))
  selectInput("year_se",label= shiny::HTML("<p>Step 1.1. Select year: <br/> <span style='font-weight: 400'></span></p>"),
              choices = time, selected = last(time))
})

output$yearH_ui_summary<- renderUI({
  time<- sort(unique(optdata_ratio$year[optdata_ratio$indicator == input$indic_health]))
  selectInput("year_health",label= shiny::HTML("<p>Step 2.1. Select year: <br/> <span style='font-weight: 400'></span></p>"),
              choices = time, selected = last(time))
})

#####################.

output$summary_expl_text <- renderUI({
  # Preparing a brief explanation for each visualisation 
    p("This visualisation shows two coropleth maps with the women/men's ratio calculated for the socieconomic indicator and health indicator you
      have chosen. The intensity of the colors in each comunity reflects whether the differences between women an men are significant or not. 
      Hover over the regions to see the ratio and the year selected.")
   
})

####### MAPS ########.
#####################.

output$map <- renderPlotly({

  data_map<- optdata_ratio%>%filter(indicator==input$indic_se)%>%filter(year==input$year_se)
  
  CCAA_sf <- sf::st_cast(esp_get_ccaa(), "MULTIPOLYGON")
  CCAA_sf <- left_join(CCAA_sf, data_map)

  map_SE<-ggplot(CCAA_sf) +
    geom_sf(aes(fill = ratio, text = paste("CCAA: <b>",areaname, "</b> with a ratio women/men of \n", ratio, "in <b>", year)), color = "grey70") +
    scale_fill_distiller(palette ="RdBu", direction = 1) +
    geom_sf(data = esp_get_can_box(), color = "grey70") +
    labs(title ="Socieconomic Results: Relation Women-Men")+
    theme_void()
  
    map_SE%>%
      ggplotly(tooltip = "text") 
    
})

output$maphealth <- renderPlotly({
  
  data_map_health<- optdata_ratio%>%filter(indicator==input$indic_health)%>%filter(year==input$year_health)
  
  CCAA_sf <- sf::st_cast(esp_get_ccaa(), "MULTIPOLYGON")
  CCAA_sf <- left_join(CCAA_sf, data_map_health)
  
  
  mid <- mean(CCAA_sf$ratio)
  
  map_health<-ggplot(CCAA_sf) +
    geom_sf(aes(fill = ratio, text = paste("CCAA: <b>",areaname, "</b> with a ratio women/men of \n", ratio, "in <b>", year)), color = "grey70") +
    scale_fill_distiller(palette ="RdBu", direction = 1) +
    geom_sf(data = esp_get_can_box(), color = "grey70") +
    labs(title ="Health Results: Relation Women-Men")+
    theme_void()
  
  map_health%>%
    ggplotly(tooltip = "text") 
  
})




###############################################.
## Downloads ----
###############################################.
#Downloading data
summary_data <- reactive({
a<- optdata_ratio %>%
  subset(indicator == input$indic_se & year==input$year_se)
b<- optdata_ratio %>%
  subset(indicator == input$indic_health & year==input$year_health)
c<-rbind(a,b)

})

summary_csv <- reactive({ format_csv(summary_data()) })

output$download_summary <- downloadHandler(filename =  'summary_data.csv',
                                         content = function(file) {write.csv(summary_data(), file, row.names=FALSE)})


##END
