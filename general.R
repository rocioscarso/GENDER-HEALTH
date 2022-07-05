############################.
##Packages ----
############################.

#get boundaries

library(heatmaply)
library(lubridate)
library(sf)
library(shiny)    # for shiny apps
library(shinyBS) #modals
library(shinythemes) # layouts for shiny
library(dplyr) # data manipulation
library(ggplot2) #data visualization
library(rgdal) #leer ficheros .shp
library(rgeos) 
library(sf) #simple features. Binds to GDAL for reading and writing data
library(rmapshaper) #to perform topologically-aware polygon simplification
library(maptools) #funcion elide. Mover Canarias
library(maps) #to draw maps
library (DT) # for data tables
library(raster)
library(readxl) #for xls files reading
library(tidyverse)
library(tidyr)
library(mapSpain)
library(leaflet) #javascript maps
library(RColorBrewer)
library(plotly) #interactive graphs
library(viridis)
library(hrbrthemes)
library(shinyWidgets) # for extra widgets
library(tibble) # rownames to column in techdoc
library(shinyjs)
library(shinydashboard) #for valuebox on techdoc tab
library(sp)
library(shinycssloaders) #for loading icons, see line below
# it uses github version devtools::install_github("andrewsali/shinycssloaders")
# This is to avoid issues with loading symbols behind charts and perhaps with bouncing of app
library(flextable) #for tech document table
library(webshot) #to download plotly charts
library(rintrojs) # for help intros
# As well as webshot phantomjs is needed l to download Plotly charts
# https://github.com/rstudio/shinyapps-package-dependencies/pull/180
if (is.null(suppressMessages(webshot:::find_phantom()))) {
  webshot::install_phantomjs()
}

###############################################.
## Functions ----
###############################################.  
# Selects the variables of interest and renames them so csv downloads are 
# more user friendly
format_csv <- function(reactive_dataset, extra_vars = NULL ) {
  
  techdoc <- techdoc %>%dplyr::select(indicator_name, data_source)
  
  left_join(reactive_dataset, techdoc, by = c("indicator" = "indicator_name")) %>%
    select_at(c("indicator", "areaname", "year", extra_vars, "sex","trend_axis","profile", "codauto", "measure", "type_definition"))
}

format_definitions_csv <- function(reactive_dataset) {
  reactive_dataset %>% 
    dplyr::select(c(profile,indicator_name, indicator_number, indicator_definition, data_source, measure, sex,
             trends_from, update_frequency,supporting_information, last_updated)) 
}

#Download button for charts, just changing the icon
savechart_button <- function(outputId, label = "Save chart", class=NULL, disabled=FALSE){
  
  if (disabled == TRUE){
    
    # Message to display when disabled button is clicked
    disabled_msg = list(p("A software update has disabled the save chart functionality. We are working on a replacement."),
                        p("In the meantime, you can:"),
                        tags$ul(
                          tags$li("Download the data with the Download data button and build new charts in tools like Excel"),
                          tags$li("Take a screenshot of the chart area using ",
                                  tags$a(href="https://support.microsoft.com/en-us/windows/open-snipping-tool-and-take-a-screenshot-a35ac9ff-4a58-24c9-3253-f12bac9f9d44",
                                         "Snipping Tool"),
                                  " or ",
                                  tags$a(href="https://blogs.windows.com/windowsexperience/2019/04/08/windows-10-tip-snip-sketch/",
                                         "Snip and Sketch."),
                                  "At least one of these tools is usually installed on recent versions of Windows."
                          )))
    
    # Create button without link
    disabled_button = tags$p(id = outputId, class = paste("btn btn-default shiny-download-link", class, "down_disabled"),
                             icon("image"), label)
    
    # Define popup message box
    disabled_popup = bsModal(paste0(outputId, "-disabled-modal"), "Save Chart Disabled", outputId, disabled_msg, size="small")
    
    # need to explicitly return both ui elements otherwise only the last will be returned
    return(tagList(disabled_button, disabled_popup))
    
    
  } else {
    tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
           href = "", target = "_blank", download = NA, icon("image"), label)
  }
  
  
}

#Function to wrap titles, so they show completely when saving plot in ggplot
title_wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

#Function to create plot when no data available
plot_nodata <- function(height_plot = 450) {
  text_na <- list(x = 5, y = 5, text = "No data available" , size = 20,
                  xref = "x", yref = "y",  showarrow = FALSE)
  
  plot_ly(height = height_plot) %>%
    layout(annotations = text_na,
           #empty layout
           yaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           xaxis = list(showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange=TRUE),
           font = list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')) %>% 
    config( displayModeBar = FALSE) # taking out plotly logo and collaborate button
} 

#Function to create plot when no data available for ggplot visuals
plot_nodata_gg <- function() {
  ggplot()+
    xlab("No data available")+
    scale_x_discrete(position = "top")+
    theme(panel.background = element_blank(),
          axis.title.x=element_text(size=20, colour ='#555555'))
}


# UI for heatmap and snapshot plots
sum_ui <- function(title, plot_name) {
  tagList(
    h5(title, style="color: black; text-align: center; font-weight: bold;"),
    div(align = "center", withSpinner(plotlyOutput(plot_name, height = "auto")))
  ) }

# Indicator definition boxes for indicator definition tab
ind_def_box <- function(label, text_output) {
  div(class="definitionbox",
      p(paste(label), style="font-weight:bold; font-size: 16px; color: #2FA4E7;"),
      h5(style = "color: black", textOutput(text_output)))
}

#Creating big boxes for main tabs in the landing page (see ui for formatting css)
lp_main_box <- function(title_box, image_name, button_name, description) {
  div(class="landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(class = "landing-page-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      actionButton(button_name, NULL, class="landing-page-button")
  )
}


#Creating small boxes for further information in the landing page (see ui for formatting css)
lp_about_box <- function(title_box, image_name, button_name, description) {
  
  div(class="landing-page-box-about",
      div(title_box, class = "landing-page-box-title"),
      div(class = "landing-page-about-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 80%; background-position: center; background-repeat: no-repeat; ")),
      (actionButton(button_name, NULL,
                    class="landing-page-button",
                    icon = icon("arrow-circle-right", "icon-lp"),title=description)))
}

###############################################.
## Data ----
###############################################.    
#optdata <- readRDS("d:/Documents and Settings/05635062Q/Mis documentos/R/GenderHealth/data/optdata.rds") #main dataset
optdata <- read_excel("C:\\Users\\rocio\\OneDrive\\Documentos\\GHEALTH\\data\\optdata.xlsx") #main dataset C:\Users\rocio\OneDrive\Documentos\GHEALTH
techdoc <- read_excel("C:\\Users\\rocio\\OneDrive\\Documentos\\GHEALTH\\data\\techdoc.xlsx")
geo_lookup <-read_excel("C:\\Users\\rocio\\OneDrive\\Documentos\\GHEALTH\\data\\geo_lookup.xlsx")
#techdoc <- readRDS("d:/Documents and Settings/05635062Q/Mis documentos/R/GenderHealth/data/techdoc.rds") #technical documents data including definitions

#geo_lookup <- readRDS("d:/Documents and Settings/05635062Q/Mis documentos/R/GenderHealth/data/geo_lookup.rds") #geography lookup

#get boundaries
#spain <- getData("GADM", country="ESP", level=1)
###############################################.
## Ratio Female/male ----
###############################################. 

# adding new column to the main data set

optdata_mean <- optdata%>%
  dplyr::select('year', 'sex',	'measure',	'indicator', 'areaname', 'codauto', 'population', 'type_definition', 'profile','trend_axis')%>%
  group_by(year, indicator, areaname, sex, codauto, population, type_definition, profile,trend_axis )%>%
  summarise(Total_mean=mean(measure)) 

# Obtaining the ratio Women/Men
optdata_ratio <-spread(optdata_mean, sex, Total_mean)%>%mutate(ratio=round(Women/Men,2))

# Obtaining the national mean ratio  for echa indicator and each year (this is to obtain the level of association)
optdata_ratio_mean <- optdata_ratio %>%
  dplyr::select('year', 'Women', 'Men',	'indicator', 'areaname', 'codauto', 'ratio', 'population')%>%
  group_by(year,indicator)%>%summarise(mean_ratio = mean(ratio,3))


# Joining ratio with national mean ratio
optdata_ratio_complete <- left_join(optdata_ratio, optdata_ratio_mean)

# Adding a column with ratio/mean_ratio

optdata_ratio_mean_complete <-optdata_ratio_complete%>%mutate(Total_mean_indicator= round(ratio/mean_ratio, 3))  



optdata_ratio$year<-as.factor(optdata_ratio$year)
optdata_ratio$indicator<-as.factor(optdata_ratio$indicator)
techdoc$last_updated<-as.Date(techdoc$last_updated)

# Joining data with map data (by codauto column)

CCAA_sf <- sf::st_cast(esp_get_ccaa(), "MULTIPOLYGON")
CCAA_sf <- left_join(CCAA_sf, optdata_ratio)

###############################################.
## Adding a new column in geo dataset necessary for use the map ----
###############################################.


geo_lookup <- geo_lookup%>%
  mutate(
    HASC_1 = case_when(
      code == "01"  ~ 'ES.AN',
      code == "02"  ~ 'ES.AR', 
      code == "03"  ~ 'ES.AS',
      code == "04"  ~ 'ES.PM',
      code == "05"  ~ 'ES.CN',
      code == "06"  ~ 'ES.CB',
      code == "07"  ~ 'ES.CL',
      code == "08"  ~ 'ES.CM',
      code == "09"  ~ 'ES.CT',
      code == "10"  ~ 'ES.VC',
      code == "11"  ~ 'ES.EX',
      code == "12"  ~ 'ES.GA',
      code == "13"  ~ 'ES.MD',
      code == "14"  ~ 'ES.MU',
      code == "15"  ~ 'ES.NA',
      code == "16"  ~ 'ES.PV',
      code == "17"  ~ 'ES.LO',
      code == "20"  ~ 'ES.ML',
      #ceuta and Melilla are considered one in the map data 
      
      TRUE ~  "OTHER"
    )
  )


## Names ----
###############################################.   
#Geographies names
area_list <- sort(geo_lookup$areaname)
comparator_list <- sort(geo_lookup$areaname[geo_lookup$areatype %in% 
                                              c("CCAA")]) 
#code_list <- unique(optdata$code)

 
adp_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="CCAA"]) 

#year of indicators
min_year <- min(optdata$year)
max_year <- max(optdata$year)



#Indicator names
indicator_list <- sort(unique(optdata$indicator))
indicator_se_list <-sort(optdata$indicator[optdata$profile %in% 
                           c("socieconomic")])
indicator_health_list<-sort(optdata$indicator[optdata$profile %in% 
                                                c("health")])


code_list <- sort(unique(optdata$codauto))

#Profile names
topic_list_filter <- as.factor(c("Show all",unique(sort(c(
  substr(optdata$profile, 5, nchar(as.vector(optdata$profile))))))))
   

profile_list <- sort(unique(optdata$profile))

profile_list_filter <-c(setNames("Show all", "Show all"), sort(profile_list))




###############################################.
## Palettes ----
###############################################.   
#Palette for SIMD.
#pal_simd_bar <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031')
#pal_simd_trend <- c('#abd9e9', '#74add1', '#4575b4', '#313695', '#022031', '#FF0000')

#Palette for map
#pal_map <- c('#2c7bb6','#abd9e9', '#ffffbf','#fdae61','#d7191c')

###############################################.
## Plot parameters ----
###############################################.

#Common parameters for plots
xaxis_plots <- list(title = FALSE, tickfont = list(size=14), titlefont = list(size=14), 
                    showline = TRUE, tickangle = 270, fixedrange=TRUE)

yaxis_plots <- list(title = FALSE, rangemode="tozero", fixedrange=TRUE, size = 4, 
                    tickfont = list(size=14), titlefont = list(size=14)) 

font_plots <- list(family = '"Helvetica Neue", Helvetica, Arial, sans-serif')

# Identify which geographies have data for each indicator
# indic <- unique(optdata$indicator[!is.na(optdata$measure)])
# indic_geog <- tapply(optdata$areatype[!is.na(optdata$measure)], optdata$indicator[!is.na(optdata$measure)], unique)

##END

