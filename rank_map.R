# Rank help main window call
observeEvent(input$rank_help, 
#call to open rank area chart help modal window

#Initial help screen - users need to pick whether they want help on area or time comparison.
rank_help_main_modal <- showModal(modalDialog(
  title = "How to use this chart",
  p(column(10, 
           
           p("Select the two indicators and the year you want to plot on the maps."),
           p("This visualisation shows a coropleth map with the level of asociation between socieconomic an health indicators you
                have chosen."),
           p("The level of asociation is calculated as a proporcion between socieconomic indicator over the mean for all ccaa and health
                indicator over the mean of all ccaa. So higher values means more socieconomic influece (red color), while lower values means mores health influence 
                (blue color)"),
           p("At the left of the map there is a tabset panel in wich you can choose to see a heatmap o a scatter plot, by default the hetmap is showed first"),
           p("The heatmap shows the results of the ratios and the level of association coloured according to the map."),
           p("The scatterplot shows the values of each ratio for each ccaa. The size of the bubbles corresponds to the population size of each ccaa."),
           p("If the data show an uphill pattern as you move from left to right, this indicates a positive relationship between socioeconomic and health indicators."),
           p("Hover over each region in the map and the heatmap or scatter plot to see the values.")
  ),
  br()),
  size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
))
) 

#{showModal(rank_help_main_modal)})


# help pop-up

observeEvent(input$help_rank, {

  showModal(modalDialog(
    title = "How to use this chart",
    p(column(10, 
             
             p("Select the two indicators and the year you want to plot on the maps."),
             p("This visualisation shows a coropleth map with the level of asociation between socieconomic an health indicators you
                have chosen."),
             p("The level of asociation is calculated as a proporcion between socieconomic indicator over the mean for all ccaa and health
                indicator over the mean of all ccaa. So higher values means more socieconomic influece (red color), while lower values means mores health influence 
                (blue color)"),
             p("At the left of the map there is a tabset panel in wich you can choose to see a heatmap o a scatter plot, by default the hetmap is showed first"),
             p("The heatmap shows the results of the ratios and the level of association coloured according to the map."),
             p("The scatterplot shows the values of each ratio for each ccaa. The size of the bubbles corresponds to the population size of each ccaa."),
             p("If the data show an uphill pattern as you move from left to right, this indicates a positive relationship between socioeconomic and health indicators."),
             p("Hover over each region in the map and the heatmap or scatter plot to see the values.")
                 ),
    br()),
    size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
  ))
  
})


   
###############################################.
## Reactive controls ----
###############################################.

#Dropdown for time period based on indicator selection  

output$yearH_ui_rank<- renderUI({
  time<- sort(unique(optdata_ratio$year[optdata_ratio$indicator == input$indic_rank_health]))
  
  div(title="Select a year for health indicator. You can click in the box, hit backspace and start to type if you want to start searching.",
  selectInput("year_health_rank",label= shiny::HTML("<p>Step 2.1. Select year: <br/> <span style='font-weight: 400'></span></p>"),
              choices = time, selected = last(time)))
})

output$yearSE_ui_rank<- renderUI({
  time<- sort(unique(optdata_ratio$year[optdata_ratio$indicator == input$indic_rank_se]))
  div(title="Select a year for socieconomic indicator. You can click in the box, hit backspace and start to type if you want to start searching.",
  selectInput("year_se_rank",label= shiny::HTML("<p>Step 1.1. Select year: <br/> <span style='font-weight: 400'></span></p>"),
              choices = time, selected = last(time)))
})

###############################################.
# Indicator definitions
#Subsetting by domain and profile. Profile is fiddly as vector uses abbreviations 
# so needs to be converted to the names to match techdoc.
defs_data_rank <- reactive({techdoc %>% subset(input$indic_rank_se == indicator_name)})

output$defs_text_rank <- renderUI({
  
  HTML(paste(sprintf("<b><u>%s</b></u> <br> %s ", defs_data_rank()$indicator_name, 
                     defs_data_rank()$indicator_definition), collapse = "<br><br>"))
})


##Rank modal dialog help 


#####################################.
# titles 
#create title and subtitle variables

output$rank_title <- renderText({
  paste0("Level of association between ", input$indic_rank_se, " in year ", input$year_se_rank, " and ",
         input$indic_rank_health , " in year ", input$year_health_rank )
})

output$rank_subtitle <- renderText({
  
  paste0("Level of association calculated as (socieconomic indicator/national mean)/(health indicator/national mean)")
  
})


#####################################.    
### Map ----
#####################################.


output$mapcorr <- renderPlotly({
  
  optdata_ratio_se <-optdata_ratio_mean_complete%>%filter(indicator==input$indic_rank_se)%>%
    filter(year==input$year_se_rank)%>%rename("mean_se"="mean_ratio", "Total_mean_se"="Total_mean_indicator")
  
  optdata_ratio_h <- optdata_ratio_mean_complete%>%filter(indicator==input$indic_rank_health)%>%
    filter(year==input$year_health_rank)%>%rename("mean_health"="mean_ratio", "Total_mean_health"="Total_mean_indicator")
  
  optdata_correlation<-merge(optdata_ratio_se, optdata_ratio_h, by = "areaname")
  
  optdata_correlation<-optdata_correlation%>%mutate(Association =round(Total_mean_se/Total_mean_health,3))
  
  optdata_asociation<-optdata_correlation%>%
    dplyr::select(areaname, codauto.x, indicator.x, year.x, ratio.x, mean_se, Total_mean_se, indicator.y, year.y, ratio.y, mean_health, Total_mean_health, Association)%>%
    rename("codauto"="codauto.x", "socieoeconomic"="indicator.x", "year_se"="year.x", "ratio_se"="ratio.x", "health"="indicator.y", "year_health"="year.y", "ratio_health"="ratio.y")
  
  CCAA_sf_rank  <- sf::st_cast(esp_get_ccaa(), "MULTIPOLYGON")
 
  
  CCAA_sf_rank <- left_join(CCAA_sf_rank, optdata_asociation)
  

  
  map_corr<-ggplot(CCAA_sf_rank) +
    geom_sf(aes(fill = Association, text = paste("CCAA: <b>",areaname, "</b> with a level of association of \n", Association)), color = "grey50") +
    scale_fill_distiller(palette ="RdBu", direction = -1) +
    geom_sf(data = esp_get_can_box(), color = "grey50") +
    labs(title ="Level of association")+
    theme_void()
  
  map_corr%>%
    ggplotly(tooltip = "text") 
  
})

#####################################.    
### Heatmap ----
#####################################.
output$heatmap1 <- renderPlotly({
  
  optdata_ratio_se <-optdata_ratio_mean_complete%>%filter(indicator==input$indic_rank_se)%>%
    filter(year==input$year_se_rank)%>%rename("mean_se"="mean_ratio", "Total_mean_se"="Total_mean_indicator")
  
  optdata_ratio_h <- optdata_ratio_mean_complete%>%filter(indicator==input$indic_rank_health)%>%
    filter(year==input$year_health_rank)%>%rename("mean_health"="mean_ratio", "Total_mean_health"="Total_mean_indicator")
  
  optdata_correlation<-merge(optdata_ratio_se, optdata_ratio_h, by = "areaname")
  
  optdata_correlation<-optdata_correlation%>%mutate(Association =round(Total_mean_se/Total_mean_health,3))
  
  optdata_asociation<-optdata_correlation%>%
    dplyr::select(areaname, codauto.x, indicator.x, year.x, ratio.x, mean_se, Total_mean_se, indicator.y, year.y, ratio.y, mean_health, Total_mean_health, Association)%>%
    rename("codauto"="codauto.x", "socieoeconomic"="indicator.x", "year_se"="year.x", "ratio_se"="ratio.x", "health"="indicator.y", "year_health"="year.y", "ratio_health"="ratio.y")
  
  
  mat <- optdata_asociation
  rownames(mat) <- mat[,1]
  mat <- mat %>% dplyr::select(-areaname)
  mat1 <- mat %>% dplyr::select(ratio_se, ratio_health, Association)
  mat1 <- as.matrix(mat1)
  col <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 6, name = "RdBu")))
  heatmap<- heatmaply(mat1, 
                 dendrogram = "none",
                 xlab = "", ylab = "", 
                 main = "",
                 margins = c(60,100,40,20),
                 grid_color = "white",
                 grid_width = 0.00003,
                 titleX = FALSE,
                 hide_colorbar = TRUE,
                 colors = col,
                 branches_lwd = 0.1,
                 label_names = c("CCAA", "Feature", "Value"),
                 fontsize_row = 10, fontsize_col = 10,
                 labCol = colnames(mat1),
                 labRow = rownames(mat1),
                 heatmap_layers = theme(axis.line=element_blank())
  )
  
})

#####################################.    
### Scatter plot----
#####################################.
output$bubblechart <- renderPlotly({ 
  
  optdata_ratio_se <-optdata_ratio_mean_complete%>%filter(indicator==input$indic_rank_se)%>%
    filter(year==input$year_se_rank)%>%rename("mean_se"="mean_ratio", "Total_mean_se"="Total_mean_indicator")
  
  optdata_ratio_h <- optdata_ratio_mean_complete%>%filter(indicator==input$indic_rank_health)%>%
    filter(year==input$year_health_rank)%>%rename("mean_health"="mean_ratio", "Total_mean_health"="Total_mean_indicator")
  
  optdata_correlation<-merge(optdata_ratio_se, optdata_ratio_h, by = "areaname")
  
  optdata_correlation<-optdata_correlation%>%mutate(Association =round(Total_mean_se/Total_mean_health,3))
  
  optdata_asociation<-optdata_correlation%>%
    dplyr::select(areaname, codauto.x, indicator.x, year.x, ratio.x, mean_se, Total_mean_se, indicator.y, year.y, ratio.y, mean_health, Total_mean_health, population.x, Association)%>%
    rename("codauto"="codauto.x", "socieoeconomic"="indicator.x", "year_se"="year.x", "ratio_se"="ratio.x", "health"="indicator.y", "year_health"="year.y", "ratio_health"="ratio.y", "population"="population.x")
 
  #optdata_asociation1<-dplyr::select(areaname. population, ratio_se, ratio_health)
  CCAA= optdata_asociation$areaname
  #area= optdata_asociation$areaname
  bubble<- ggplotly(
    ggplot(optdata_asociation, aes(x=ratio_se, y=ratio_health, group=1,  color= CCAA, size = population,
                                   text = paste("CCAA: <b>",areaname, "</b> with a level of association of \n", Association))) +
      geom_point(alpha=0.6, shape = 21, ) +
      scale_size(range = c(3, 12), name=" Population (M)") +
      geom_smooth(method = 'lm', se=FALSE, color = "black", size=0.4)  +
      labs(title ="Scatter plot", x= "Socioeconomic", y="Health")+
      theme_void())
})
  #####################################.
  ## Downloads ----
  #####################################.
  #Downloading data
  
  
  rank_data  <- reactive({
    
    a<-optdata_ratio_mean_complete%>%filter(indicator==input$indic_rank_se)%>%
      filter(year==input$year_se_rank)
    
    a<-optdata_ratio_he <- optdata_ratio_mean_complete%>%filter(indicator==input$indic_rank_health)%>%
      filter(year==input$year_health_rank)
    
    c<-rbind(a, b)
  
  })
  
  rank_csv <- reactive({format_csv(rank_data())
    
  })
  
  output$download_rank<- downloadHandler(filename =  'rank_data.csv',
                                          content = function(file) {write.csv(rank_csv(), file, row.names=FALSE) })
 
  
  

