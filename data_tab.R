# Code for data tab


###############################################.
## Table ----
###############################################.
# MENU "TABLE" 

# Allow the user to select the number of samples and variables to show

#datasubset2 <- reactive({
#  optdata[sample(nrow(optdata), input$sampleSize),]
#}) 


output$table_filtered <- renderDT(
              optdata,
              style = 'bootstrap',
              filter = "top",
              options = list(columnDefs = list(list(
              targets = c(1, 4, 10, 12), searchable = FALSE
              )),
              pageLength = 5),
              colnames = c("Indicator ID"=2, "Year"=3, "Sex"=4, "Measure"=5, "Period"=6, 
                           "Indicator"=7, "Scale"=8, "Profile"=9,
                           "CCAA"=10,"Area"=11, "Code"=12, "Population"=13)
             
              
              )

###############################################.
## Downloads ----
###############################################.
# Downloading data in csv format
filter_table <- reactive({
  optdata
  
})



table_csv <- reactive({ format_csv(filter_table()) })

#The filters the user applies in the data table will determine what data they download - indicator tab table
output$download_table_csv <- downloadHandler(
  filename ="data_extract.csv",
  content = function(file) {
    write.csv(table_csv(),
              file, row.names=FALSE) } 
)

##END