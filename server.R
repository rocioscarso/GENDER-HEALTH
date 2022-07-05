#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

library(shiny)

#Code to create GENDER HEALTH Shiny profile platform
#In this script include all the server side functions: plots, reactive objects, etc.

###############################################.


## Define a server for the Shiny app
function(input, output, session) {
  
  ###############################################.
  ## Sourcing tab code  ----
  ###############################################.
  # Sourcing file with server code
  source(file.path("general.R")) #global
  source(file.path("summary_tab.R"),  local = TRUE)$value # Summary tab
  source(file.path("rank_map.R"),  local = TRUE)$value # Correlation Tab
  source(file.path("data_tab.R"),  local = TRUE)$value # Data tab
  source(file.path("tech_doc_tab.R"),  local = TRUE)$value # Technical document tab
 
  ################################################################.
  #    Modal ----
  ################################################################.
  ## Latest indicator updates modal window ----
  updates_modal <- modalDialog(
    fluidRow(
      column(12,
             # text_intro("We are continuously updating and developing our tool"),                 
             p(div("We are continuously updating and developing our tool", 
                   style = "color:0E3E5D; font-size:20px; width: 90%; text-align: left; ")),
             br(),
             br(),
             p(h5("Recent indicator updates include:", 
                  style = "width: 90%; text-align: left; font-weight: bold; "))),
    ),
    br(),
    p(h5("To find out when an indicator is due to be updated please refer to our ", 
         tags$a(href="https://docs.google.com/spreadsheets/d/e/2PACX-1/pubhtml", "updates schedule.", class="externallink"))),
    br(),
    p(h5("For any further questions or other developments you would like to 
              suggest for our current tool, please contact us at", 
         tags$a(href="mailto:rocio.rscarso@alumnos.upm.es", "rocio.rscarso@alumnos.upm.es", class="externallink"), 
         style = "width: 700px")),
    br(),
    size = "l", align= "center",
    easyClose = TRUE, fade=TRUE, footer = modalButton("Close (Esc)")
  )
    observeEvent(input$btn_indicator_updates, { showModal(updates_modal) }) # Link action button click to modal launch 
}
