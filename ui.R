#Code to create GH Shiny profile platform
# This script includes the user-interface definition of the app.
# Rocio Rodriguez Scarso 

################################################################################
#Code to create Genre-health's Shiny platform
# This script includes the user-interface definition of the app.
################################################################################
##################################################################################

# This script includes the user-interface definition of the app.
source(file.path("C:\\Users\\rocio\\OneDrive\\Documentos\\GHEALTH\\general.R"))# Summary tab
###############################################.
## Header ---- 
###############################################.
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  introjsUI(),   # Required to enable introjs scripts
  navbarPage(id = "intabset", #needed for landing page
             title = div(tags$a(img(src="gender_health.png", height=50), href= "https://www.cnic.es/"),
                         style = "position: relative; top: -5px;"), # Navigation bar
             windowTitle = "Gender Health", #title for browser tab
             theme = shinytheme("cerulean"), #Theme of the app (blue navbar)
             collapsible = TRUE, #tab panels collapse into menu in small screens
             header = tags$head(includeCSS("www/styles.css"), # CSS styles
                                HTML("<html lang='en'>"),
                                tags$link(rel="shortcut icon", href="favicon_scotpho.ico"), #Icon for browser tab
                                #Including Google analytics
                                #includeScript("google-analytics.js"),
                                HTML("<base target='_blank'>"),
                                tags$style(
                                  HTML("input[type='search']:disabled {visibility:hidden}")
                                )),
             
             
             ###############################################.
             ## MAPS ----
             ###############################################
             tabPanel("Maps", icon = icon("map"), value = "maps",
                      
                      fluidRow(column(12,
                                      
                                             h2("GENDER-HEALTH. Gender inequality in Spain: Impact on women's health")),
                                      
                      ),
                      shiny::hr(),
                      introBox(
                        
                        wellPanel(fluidRow( #Filter options
                          column(3, 
                                 div(title="Select a socieconomic indicator. You can click in the box, hit backspace and start to type if you want to start searching.",
                                     selectInput("indic_se", shiny::HTML("<p> Step 1. Select a socieconomic indicator <span style='font-weight: 400'> <br/> 
                                                        (hit backspace and start typing to search for an indicator)</span></p>"), 
                                                 choices=indicator_se_list, selected = 1)), 
                                    br(),
                                    # Select year
                                        
                                                     uiOutput("yearSE_ui_summary")
                                                  
                                 
                          ),#column bracket
                          
                          column(3, 
                                 div(title="Select a health indicator. You can click in the box, hit backspace and start to type if you want to start searching.",
                                     selectInput("indic_health", shiny::HTML("<p> Step 2.Select a health indicator <span style='font-weight: 400'> <br/> 
                                                        (hit backspace and start typing to search for an indicator)</span></p>"), 
                                                 choices=indicator_health_list, selected = 1)),
                                 br(),
                                 # Select year
                                 
                                                  uiOutput("yearH_ui_summary")
                          ),#column bracket
                          # Select year
                          
                          
                          column(2, offset = 3, #buttons
                                 actionButton("help_summary",label="Help", icon= icon('question-circle'), class ="down"),
                                 downloadButton('download_summary', 'Download data', class = "down"),
                                 uiOutput("save_chart_ui")
                                 )),
                          
                         
                        ), #well panel bracket
                        data.step = 4, 
                        data.intro =(p(h5("Throughout the tool use the dropdown menus to change which indicators are displayed in the maps."),
                                       br(),
                                       h5("While using dropdown menus mouse click within a dropdown menu and press backspace on your keyboard ('<-') then start typing a word to quickly find the options you are looking for"),
                                       img(src='introjs_how_to_select.png')))
                        
                      ), #introbox bracket
                      mainPanel(width = 12,
                                shiny::hr(),
                                bsModal("mod_defs_summary", "Definitions", "defs_summary",
                                        htmlOutput('defs_text_summary')),
                                fluidRow(column(6,
                                                h4(textOutput("summary_title"), style="color: black; text-align: left"),
                                                h5(textOutput("summary_subtitle"), style="color: black; text-align: left")
                                                
                                ),
                                column(3,
                                       br(),
                                       br(),
                                       p(img(src='signif_better.png', height=18, style="padding-right: 2px; vertical-align:middle"),"Better for women", 
                                         br(),
                                         img(src='non_signif.png', height=18, style="padding-right: 2px; vertical-align:middle"), "Not significant differences", 
                                         br(),
                                         img(src='signif_worse.png', height=18, style="padding-right: 2px; vertical-align:middle"),"Worse for women")
                                       )# Column bracket
                                ),#fluidrow bracket
                                shiny::hr(), #header row
                                column(width = 6, #rank bar
                                       withSpinner(plotlyOutput("map", height = "600px"))),
                                column(width = 6, #map
                                       withSpinner(plotlyOutput("maphealth", height = "600px"))),
                                # Depending what users selects different visualizations
                                
                                
                                
                      ) #main panel bracket
             ), #Tab panel bracket
           
           
             
             ###############################################.
             ## Correlation and map ---- 
             ###############################################.
             tabPanel("Association", icon = icon("chart-area"), value = "rank",
                      fluidRow(column(12,
                                      
                                      h2("GENDER-HEALTH. Gender inequality in Spain: Impact on women's health")),
                               
                      ),
                      shiny::hr(),
                      wellPanel(#Filter options
                        column(width = 3,
                               div(title="Select a socieconomic indicator. You can click in the box, hit backspace and start to type if you want to start searching.",
                                   selectInput("indic_rank_se", shiny::HTML("<p>Step 1. Select a socieconomic indicator <span style='font-weight: 400'> <br/> 
                                                        (hit backspace and start typing to search for an indicator)</span></p>"), 
                                               choices=indicator_se_list, selected = 1)), 
                               br(),
                               # Select year
                                                uiOutput("yearSE_ui_rank")
                               
                               
                        ),#column bracket
                        
                        column(3,
                               div(title="Select a health indicator. You can click in the box, hit backspace and start to type if you want to start searching.",
                                   selectInput("indic_rank_health", shiny::HTML("<p> Step 2.Select a health indicator <span style='font-weight: 400'> <br/> 
                                                        (hit backspace and start typing to search for an indicator)</span></p>"), 
                                               choices=indicator_health_list, selected = 1)),
                               br(),
                               # Select year
                                              uiOutput("yearH_ui_rank")
                        ),#column bracket
                        # Select year
                 
                        column(width = 2, offset =3,
                               introBox(
                                 actionButton("rank_help",label="Help", icon= icon('question-circle'), class ="down"),
                                 downloadButton('download_rank', 'Download data', class = "down"),
                                 data.step = 5,
                                 data.intro =(p(h5("Throughout the tool look out for options in each window that provide"),
                                                tags$li("indicator definitions or help to interpret a visualisation,",style="color: #007ba7"),
                                                tags$li("data download options for individual charts,",style="color: #007ba7"),
                                                tags$li("image downloads for individual charts.",style="color: #007ba7")))))
                      ), #well pannel bracket
                      mainPanel(width = 12, #Main panel
                                bsModal("mod_defs_rank", "Definitions", "defs_rank", htmlOutput('defs_text_rank')),
                                uiOutput("rank_summary"), #description 
                                shiny::hr(), #header row
                                fluidRow(column(6,
                                                h4(textOutput("rank_title"), style="color: black; text-align: left"),
                                                h5(textOutput("rank_subtitle"), style="color: black; text-align: left")
                                                
                                ), # column bracket
                                column(3,
                                       br(),
                                       br(),
                                       p(img(src='signif_better.png', height=18, style="padding-right: 2px; vertical-align:middle"),"Better health indicator",
                                         br(),
                                         img(src='non_signif.png', height=18, style="padding-right: 2px; vertical-align:middle"), "Not significant differences", 
                                         br(),
                                         img(src='signif_worse.png', height=18, style="padding-right: 2px; vertical-align:middle"),"Worse health indicator"))
                                ), # fluidrow bracket
                                shiny::hr(), #header row
                              
                                #shiny::hr(), #header row
                                column(width = 6,
                                       tabsetPanel(type = "tabs",
                                            tabPanel("Heatmap", withSpinner(plotlyOutput("heatmap1", height = "600px"))),
                                            tabPanel("Scatter Plot", withSpinner(plotlyOutput("bubblechart", height = "600px"))))),
                                column(width = 6, #map
                                       withSpinner(plotlyOutput("mapcorr", height = "600px"))),
                                
                                
                                
                      ) #main panel bracket
             ), #Tab panel bracket
             
             ###############################################.
             ## Data -
             ###############################################.
             tabPanel("Data", icon = icon("table"), value = "table",
                      fluidRow(column(12,
                                      
                                      h2("GENDER-HEALTH. Gender inequality in Spain: Impact on women's health")),
                               
                      ),
                      shiny::hr(),
                      #Sidepanel for filtering data
                      mainPanel(
                        width = 12, style="margin-left:0.5%; margin-right:0.5%",
                        #Row 1 for intro  
                        fluidRow(
                          p("Download the data used in the tool", 
                            style = "font-weight: bold; color: black;"),
                          p("Use the filters in the columns to select the data you want to download. ",
                            "To delete choices use cross in charts filter or clear button from keyboard."),
                          br()
                        ),
                        #Row 2 for selections
                        fluidRow(
                          column(3,
                                 p("Select what data you want", style = "font-weight: bold; color: black;"),  
                                 div("All available indicators will be displayed if none specified filter is apply."),
                                 
                          ),
                          #column(3,
                           #      br(),
                            #     selectizeInput("show_vars", "Columns to show:",
                             #                   names(optdata), selected = names(optdata), multiple = TRUE)
                          #),
                          br(),
                          column(3, offset = 1,
                                 
                                 downloadButton("download_table_csv", 'Download data', class = "down")             
                          ) #column bracket
                        ), #filters fluid row bracket
                        #Row 3- Table
                        fluidRow(  
                          column(12, div(DTOutput("table_filtered"), 
                                         style = "font-size: 98%; width: 98%"))
                        )
                      ) # main panel bracket
             ), #Tab panel bracket  
             
             
             ###############################################.             
             ##############NavBar Menu----
             ###############################################.
             #Starting navbarMenu to have tab with dropdown list
             navbarMenu("Info", icon = icon("info-circle"),
                        ###############################################.
                        ## About ----
                        ###############################################.
                        tabPanel("About", value = "about",
                                 sidebarPanel(width=1),
                                 mainPanel(width=8,
                                           h4("About", style = "color:black;"),
                                           p("GENDER-HEALTH tool allows users to explore health and socieconomic profiles in Spain "),
                                           p("The profiles are intended to increase understanding of local gender health issues related with socieconomic factors 
                                           and to prompt further investigation, rather than to be used as a performance management tool. 
                                           The information needs to be interpreted within a local framework; an indicator may be higher or lower in one area compared to another, 
                                             but local knowledge is needed to understand and interpret differences."),
                                           br(),
                                           p("The tool has been developed by a group of researchers from Universidad Politecnica de Madrid, National Center 
                                           for Cardiavascular Research, and Hospital Universitario 12 de Octubre."),
                                           p("We aim to provide a clear picture of the health of the spanish population and the socieconomic factors 
                                           that affect it. We contribute to improved collection and use of routine data on health, 
                                           risk factors, behaviours and wider health determinants. We take a lead in determining 
                                           Scotland's future public health information needs, develop innovations in public health 
                                information and provide a focus for new routine public health information development 
                                where gaps exist."),
                                           br(),
                                           p("If you have any trouble accessing any information on this site or have
                                any further questions or feedback relating to the data or the tool, then please contact us at: ",
                                             tags$b(tags$a(href="mailto:rocio.rscarso@alumnos.upm", "rocio.rscarso@alumnos.upm", class="externallink")),
                                             "and we will be happy to help.")),
                                 br()
                        ),#Tab panel
                        ###############################################.
                        ## Indicator definitions ----
                        ###############################################.
                        tabPanel("Indicator definitions", value = "definitions",
                                 #Sidepanel for filtering data
                                 fluidRow(style = "width:60%; margin-left: 2%; min-width: 350px",
                                          h4("Indicator definitions and technical information", style = "color:black;"),
                                          h5(style = "color:black", 
                                             "ScotPHO Profiles are made up of a collection of indicators related to socieconomic and health themes. Profiles are further divided into topic areas to group similar indicators together. 
                                 This page allows users to see available indicatorsand find detailed technical information about the indicators."),
                                          br(),
                                          div(title="Choose if you want to see a list of all available indicators or all the details for a specific indicator",
                                              radioGroupButtons("techdoc_selection", status = "primary",
                                                                choices = c("List of available indicators", "Detailed information about single indicator"), 
                                                                label= "Step 1. Select what you want to see:" )),
                                          br(),
                                          conditionalPanel(condition = 'input.techdoc_selection == "Detailed information about single indicator"',
                                                           uiOutput("indicator_choices"),
                                                           br()
                                          ),
                                          uiOutput("profile_picked_ui"),
                                          br(),
                                          #conditional panel for profile summary
                                          conditionalPanel(condition = 'input.techdoc_selection == "List of available indicators"',
                                                           uiOutput("tecdoc_list"),
                                                           downloadButton("download_techdoc1_csv",'Download indicator summary (.csv)', class = "down")),
                                          
                                          #conditional panel for single indicator
                                          conditionalPanel(condition = 'input.techdoc_selection == "Detailed information about single indicator"',
                                                           
                                                            downloadButton("download_detailtechdoc_csv",'Download selected definition', class = "down"),
                                                            downloadButton("download_alltechdoc_csv",'Download all indicator definitions', class = "down")
                                          )),
                                 wellPanel(width = 11,
                                           # display flextable   
                                           conditionalPanel(condition = 'input.techdoc_selection == "List of available indicators"',
                                                            br(),
                                                            br(),
                                                            uiOutput("techdoc_display")),
                                           
                                           #techdoc single indicator
                                           conditionalPanel(condition = 'input.techdoc_selection == "Detailed information about single indicator" & input.indicator_selection != null',
                                                            useShinydashboard(),
                                                            valueBoxOutput("indicator", width=12),
                                                            column(5,
                                                                   ind_def_box("Definition", "indicator_definition"),
                                                                   ind_def_box("Data source", "data_source"),
                                                                   ind_def_box("Measure", "measure")
                                                                   ),
                                                            column(5,
                                                                   ind_def_box("Trends from", "trends_from"),
                                                                   ind_def_box("Date last updated", "last_updated"),
                                                                   ind_def_box("Frequency of update", "update_frequency"),
                                                                   ind_def_box("Links to supporting information", "supporting_info"))
                                           )
                                 ) # well panel
                        ) #tab panel
                       
                        
             )# NavbarMenu bracket
  ), #Bracket  navbarPage             
  
  
  
  div(style = "margin-bottom: 30px;"), # this adds breathing space between content and footer
  ###############################################.             
  ##############Footer----    
  ###############################################.
  #Copyright warning
  tags$footer(column(8, "Universidad Politecnica de Madrid (UPM)"), 
              
              column(3, tags$a(href="mailto:rocio.rscarso@alumnos.ump.es", tags$b("Contact us!"), 
                               class="externallink", style = "color: white; text-decoration: none")), 
              #column(3, tags$a(href="https://www.scotpho.org.uk/about-us/scotpho-website-policies-and-statements/privacy-and-cookies", tags$b("Privacy & cookies"), 
              #class="externallink", style = "color: white; text-decoration: none")) 
              style = "
   position:fixed;
   text-align:center;
   left: 0;
   bottom:0;
   width:100%;
   z-index:1000;  
   height:30px; /* Height of the footer */
   color: white;
   padding: 10px;
   font-weight: bold;
   background-color: #1995dc"
  ) 
  ################################################.
) #bracket tagList
###END