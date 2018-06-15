#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rgdal)
library(leaflet)
library(reshape)
library(reshape2)
library(dplyr)
library(DT)
library(readr)
library(xlsx)
library(shinythemes)


# getwd()
# fifteen <- read.csv(file = "/srv/shiny-server/apps/mass/fifteen.csv",sep = ",")
# sixteen <- read.csv(file = "/srv/shiny-server/apps/mass/sixteen.csv",sep = ",")
# seventeen <- read.csv(file = "/srv/shiny-server/apps/mass/seventeen.csv",sep = ",")
# eighteen <- read.csv(file = "/srv/shiny-server/apps/mass/eighteen.csv",sep = ",")
fifteen <- read.csv(file = "C:/Users/gonza/Desktop/apps/mapunique/fifteen.csv",sep = ",")
sixteen <- read.csv(file = "C:/Users/gonza/Desktop/apps/mapunique/sixteen.csv",sep = ",")
seventeen <- read.csv(file = "C:/Users/gonza/Desktop/apps/mapunique/seventeen.csv",sep = ",")
eighteen <- read.csv(file = "C:/Users/gonza/Desktop/apps/mapunique/eighteen.csv",sep = ",")



# Define UI for application that creates a map
ui <- fluidPage(
  shinythemes::themeSelector(),
  titlePanel(title = "Mass Shooting Tracker"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
      wellPanel(
      # Numeric input for number of rows to show
      numericInput(inputId = "n_rows",
                   label = "Number of Rows of Data to See?",
                   value = 10),
      actionButton(inputId = "button", 
                   label = "Get Data"),
      checkboxInput(inputId = "show_map",label = "Create Map",value = FALSE)
      ),
      
      # Action button to show
      wellPanel(
      
      
      numericInput(inputId = "number_shot",
                   label = "Number of Individual's Killed?",
                   value = 5),
      
      # Action button to show
      actionButton(inputId = "buttons", 
                   label = "Get Data"),
      
      numericInput(inputId = "number_wounded",
                   label = "Number of Individual's Wounded:",
                   value = 5),
      
      # Action button to show
      actionButton(inputId = "return", 
                   label = "Get Data")),
      
      ##Well Panel Two #####
      wellPanel(
      textInput(inputId = "state",
                   label = "Enter State:",value = "CA"
                      ),
      
      # Action button to show
      actionButton(inputId = "return_state", 
                   label = "Get State Data")),
      
      
      # Select filetype
      wellPanel(
      radioButtons(inputId = "filetype",
                   label = "Select filetype:",
                   choices = c("csv", "tsv", "xlsx"),
                   selected = "csv"),
      downloadButton("download_data", "Download data")),
      
      ##Checkbox input to show data
      wellPanel(
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE))
    )),
  
  mainPanel(
    tabsetPanel(
    tabPanel(leafletOutput("shooting",width = "auto", height = "625px"),title = "Mass Shootings"),
    tabPanel(DT::dataTableOutput(outputId = "datatable"),title = "Data"),
    tabPanel(DT::dataTableOutput(outputId = "datatables"),title = "Number Killed"),
    tabPanel(DT::dataTableOutput(outputId = "datareturn"),title = "Number Wounded"),
    tabPanel(DT::dataTableOutput(outputId = "datastate"),title = "State Data:"),
    tabPanel(plotOutput(outputId = "bar"),title = "Bar Graph")
    )
  
)))
  
# mainPanel(
#   leafletOutput("shooting",width = "auto", height = "625px")
#                 ,title = "Mass Shootings")

# Define server logic required to create the map
server <- function(input, output) {
  

  # filteredData <- reactive({
  #   eighteen[eighteen$killed == input$number_shot, ] 
  # })
  
  # df2 <- eventReactive(input$buttons, {
  #   eighteen %>% filter(eighteen$killed == input$number_shot)
  # })
  
  fifteen_popup <- paste0("<br><strong>Date: </strong>", 
                          fifteen$date,
                          "<br><strong>Number Killed: </strong>", 
                          fifteen$killed, 
                          "<br><strong>Wounded: </strong>",
                          fifteen$wounded,
                          "<br><strong>City: </strong>",
                          fifteen$city,
                          "<br><strong>Perpetrator: </strong>",
                          fifteen$name_semicolon_delimited
  )
  
  sixteen_popup <- paste0("<br><strong>Date: </strong>", 
                          sixteen$date,
                          "<br><strong>Number Killed: </strong>", 
                          sixteen$killed, 
                          "<br><strong>Wounded: </strong>",
                          sixteen$wounded,
                          "<br><strong>City: </strong>",
                          sixteen$city,
                          "<br><strong>Perpetrator: </strong>",
                          sixteen$name_semicolon_delimited
  )
  
  seventeen_popup <- paste0("<br><strong>Date: </strong>", 
                            seventeen$date,
                            "<br><strong>Number Killed: </strong>", 
                            seventeen$killed, 
                            "<br><strong>Wounded: </strong>",
                            seventeen$wounded,
                            "<br><strong>City: </strong>",
                            seventeen$city,
                            "<br><strong>Perpetrator: </strong>",
                            seventeen$name_semicolon_delimited
  )
  
  eighteen_popup <- paste0("<br><strong>Date: </strong>", 
                           eighteen$date,
                           "<br><strong>Number Killed: </strong>", 
                           eighteen$killed, 
                           "<br><strong>Wounded: </strong>",
                           eighteen$wounded,
                           "<br><strong>City: </strong>",
                           eighteen$city,
                           "<br><strong>Perpetrator: </strong>",
                           eighteen$name_semicolon_delimited
  )
  
  
  
  # Create reactive data frame

  

  
  shootingmap<- leaflet() %>%
    addTiles(group = "OSM (default)") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
    #addCircleMarkers()
    addCircleMarkers(data = fifteen,fillColor = "green",lng = fifteen$lon,lat = fifteen$lat, popup = fifteen_popup, group = "2015") %>%
    addCircleMarkers(data = sixteen,fillColor = "blue",lng = sixteen$lon,lat = sixteen$lat, popup = sixteen_popup, group = "2016") %>%
    addCircleMarkers(data = sixteen,fillColor = "red",lng = seventeen$lon,lat = seventeen$lat, popup = seventeen_popup, group = "2017") %>%
    addCircleMarkers(data = eighteen,fillColor = "yellow",lng = eighteen$lon,lat = eighteen$lat, popup = eighteen_popup, group = "2018" ) %>%
    addLayersControl(
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("2015","2016","2017","2018 (default)"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    addMiniMap(toggleDisplay =  TRUE)%>%
    addMeasure()
  
  output$shooting <- renderLeaflet(if(input$show_map){shootingmap})
  
  # Take a reactive dependency on input$button, but not on any other inputs
  df <- eventReactive(input$button, {
    head(eighteen, input$n_rows)
  })

 
  
  output$datatable <- DT::renderDataTable({
   
    DT::datatable(data = df(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)})
  
  ##Combine all the data together into one file ########
  df2 <- eventReactive(input$buttons, {
    newest <- rbind(eighteen,seventeen,sixteen,fifteen)
    
    newest %>% filter(newest$killed == input$number_shot)
  })
  # 

  
  output$datatables <- DT::renderDataTable({
    DT::datatable(data = df2(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  ##Combine data to filter for wounded indviduals #######
  
  df3 <- eventReactive(input$return, {
    newest <- rbind(eighteen,seventeen,sixteen,fifteen)
    
    newest %>% filter(newest$wounded == input$number_wounded)
  })
  # 
  
  
  output$datareturn <- DT::renderDataTable({
    DT::datatable(data = df3(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  
  df5 <- eventReactive(input$return_state, {
    newest <- rbind(eighteen,seventeen,sixteen,fifteen)
    
    newest %>% filter(newest$state == input$state)
  })
  # 
  
  
  output$datastate <- DT::renderDataTable(if(input$show_map){
    DT::datatable(data = df5(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  ##Bar Graph Code #######
  
  
  
  # output$bar <- barplot({
  #   
  #   df4 <- rbind(eighteen,seventeen,sixteen,fifteen)
  #     
  #   
  #   barplot(df4,col="blue",las=2)
  # 
  #   })
  
  

  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("MassShootingData.", input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(df4(), file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(df4(), file) 
      }

    }
  )
}




# Run the application 
shinyApp(ui = ui, server = server)

