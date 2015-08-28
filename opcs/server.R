##Reading in Library is necessary for functions to work properly
library(shiny)
library(dplyr)
library(datasets)
library(googleVis)
library(plotGoogleMaps)

# provide data "as of date"
date <- "20150827"

# Read in data file
file <- str_c("data/datafile_", date, ".csv")
datafile <- read.csv(file, stringsAsFactors = FALSE)

shinyServer(function(input, output, session) {
               
        myOptions <- reactive({
                list(
                        page = "enable",
                        pageSize = 20,
                        width=1500
                )
        })
        
        state_data <- reactive({
                # create placeholder selected_states variable to assign in if statements
                selected_states <- c()
                
                # if statements to handle "All states" input to state dropdown menu
                if("All states" %in% input$state){
                        selected_states <- unique(datafile$Proj.ST.Abbr)
                }
                
                if(!("All states" %in% input$state)){
                        selected_states <- input$state
                }
                
                filter(datafile, Proj.ST.Abbr %in% selected_states)        
        })
              
        # counties is a reactive variable that identifies the counties assosciated with the user's selection from the state input menu
        counties <- reactive({
                state_data <- state_data()
                state_data <- arrange(state_data, Proj.ST.Abbr, Proj.County.Name)
                unique(state_data$Proj.County.Name)                
        })
              
        # observe is a reactive function that repopulates the county select input menu using the reactive variable county
        observe({
                counties_all <- c("All counties", counties())
                updateSelectInput(session, "counties", choices = counties_all)
        })
        
        # filter data to only those states/counties selected
        data_table1 <- reactive({
                # assign previously created reactive variables to regular variables
                state_data <- state_data()
                counties <- counties()
                
                # create if statements to handle "All counties" option in dropdown menu
                if("All counties" %in% input$counties){
                        data_table1 <- filter(state_data, Proj.County.Name %in% counties)                        
                }
                
                if(!("All counties" %in% input$counties)){
                        data_table1 <- filter(state_data, Proj.County.Name %in% input$counties)                        
                }
                
                data_table1[ , c(1:3, 8:10, 18, 24, 26:28, 34:38)]           
        })
        
        # subset data to show/not show jobs or PI columns and show/not show Construction-only projects based on checkbox input
        data_table2 <- reactive({
                data_table1 <- data_table1()
                
                if(input$JobsPIFlag == FALSE){
                        data_table2 <- select(data_table1, -Jobs.Created, -Jobs.Saved, -Priv.Investment)
                }
                
                if(input$JobsPIFlag == TRUE){
                      data_table2 <- filter(data_table1, Cons.Non == "C" | Cons.Non == "B")
                }      
                
                data_table2
        })
        
        # filter data based on year input
        data_table <- reactive({
                data_table2 <- data_table2()
                range <- seq(input$years[1], input$years[2])
                data_table <- filter(data_table2, FY %in% range)
                data_table
        })
                
        # create output table
        output$table <- renderDataTable({
                data_table_output <- data_table()
                data_table_output
                }
                , options = list(pageLength = 10)
        )
        
        
        # create output map
        output$map <- renderGvis({
                data_table <- data_table()
                map_data <- select(data_table, FY, Proj.ST.Abbr, lat_lon)
                names(map_data)[2] <- "tip"
                
                map_plot <- gvisMap(map_data, "lat_lon" , "tip", 
                                    options=list(showTip=TRUE, showLine=TRUE, enableScrollWheel=TRUE, mapType='normal', useMapTypeControl=TRUE, 
                                        width = 800, height = 600))
        })
        
#         create output map
#         output$map <- renderUI({
#                 data_map <- data_table()
#                 coordinates(data_map) = ~ lon + lat      
#                 proj4string(data_map) = CRS("+proj=longlat +datum=WGS84")
#                 data_map_spdf <- SpatialPointsDataFrame(data_map, data = data.frame( ID = row.names(datafile) ) )  
#                 map <- plotGoogleMaps(data_map_spdf, filename='map.html')
#                 tags$iframe(
#                         srcdoc = paste(readLines('map.html'), collapse = '\n'),
#                         width = "100%",
#                         height = "600px"
#                 )
#         })

# create output map
#         output$map <- renderUI({
#                 data_map <- data_table()
#                 coordinates(data_map) = ~ lon + lat      
#                 proj4string(data_map) = CRS("+proj=longlat +datum=WGS84")
#                 data_map_spdf <- SpatialPointsDataFrame(data_map, data = data.frame( ID = row.names(datafile) ) )  
#                 map <- plotGoogleMaps(data_map_spdf, filename='map.html')
#                 tags$iframe(
#                         srcdoc = paste(gsub("http://maps.google.com/", 
#                                             "https://maps.google.com/", 
#                                             readLines('myMap1.html'), 
#                                             fixed=TRUE), collapse = '\n'),
#                         width = "100%",
#                         height = "600px"
#                 )
#         })

                
        # create download file
        output$downloadData <- downloadHandler(
                filename = function() {
                        str_c("datafile_", date, ".csv") 
                },
                content = function(file) {
                        write.csv(data_table(), file)
                }
        )
}
)