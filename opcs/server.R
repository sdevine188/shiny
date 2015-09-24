##Reading in Library is necessary for functions to work properly
library(shiny)
library(dplyr)
library(datasets)
library(leaflet)
library(stringr)
library(DT)

# provide data "as of date"
date <- "20150827"

# Read in data file
file <- str_c("data/datafile_", date, ".csv")
datafile <- read.csv(file, stringsAsFactors = FALSE)

# create program colors
program_options <- factor(c("Public Works", "Planning", "Econ Adjst", "Tech Asst", "Disaster Supp",
                            "GCCMIF", "Research", "CTAA", "Trade Adjst"))

year_options <- factor(seq(1995, 2016))

program_pal <- colorFactor("Set1", domain = program_options)

year_pal <- colorFactor("Set1", domain = year_options)

# fund_pal <- colorNumeric(
#         palette = "Blues",
#         domain = datafile$EDA.
# )

# shiny server
shinyServer(function(input, output, session) {
        
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
        
#         output$counties = renderUI({
#                 state_data = state_data()
#                 state_data <- arrange(state_data, Proj.ST.Abbr, Proj.County.Name)
#                 counties <- c("All counties", unique(state_data$Proj.County.Name))
#                 selectInput('counties', 'Select counties', choices = counties, multiple = TRUE, selected = counties[1])
#         })
        
        # counties is a reactive variable that identifies the counties assosciated with the user's selection from the state input menu
        counties <- reactive({
                state_data <- state_data()
                state_data <- arrange(state_data, Proj.ST.Abbr, Proj.County.Name)
                unique(state_data$Proj.County.Name)                
        })
        
        # observe is a reactive function that repopulates the county select input menu using the reactive variable county
        observe({
                counties_all <- c("All counties", counties())
                updateSelectInput(session, "counties", choices = counties_all, selected = counties_all[1])
        })
        
        # filter data to only those states/counties selected
        data_table1 <- reactive({
                # assign previously created reactive variables to regular variables
                state_data <- state_data()
                counties <- counties()
#                 counties <- unique(state_data$Proj.County.Name)
                       
                # create if statements to handle "All counties" option in dropdown menu
                if("All counties" %in% input$counties){
                        data_table1 <- filter(state_data, Proj.County.Name %in% counties)                        
                }
                
                if(!("All counties" %in% input$counties)){
                        data_table1 <- filter(state_data, Proj.County.Name %in% input$counties)                        
                }
                
                #                 data_table1[ , c(1:3, 8:10, 18, 24, 26:28, 34:38)]  
                data_table1
                
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
#                 range <- seq(1990, 2015)
                data_table <- filter(data_table2, FY %in% range)
                data_table
        })
        
        # create output table
#         output$table <- renderDataTable({
#                 data_table_output <- data_table()
#                 data_table_output
#         }
#         , options = list(pageLength = 10, searching = FALSE)
#         )
        output$table <- DT::renderDataTable({
                data_table_output2 <- data.frame()
                data_table_output <- data_table()
                
                # create placeholder dataframe to use when user-selected data has zero rows 
                # to avoid breaking datatable w/ filter
                x <- data.frame("No projects found based on search")
                names(x)[1] <- ""

                # assign either the placeholder data or the user-selected data (if rows > 1) to be fed into datatable
                if(nrow(data_table_output) < 1){
                        data_table_output2 <- x
                        return(datatable(data_table_output2, filter = "none", rownames = FALSE))
                }
                if(nrow(data_table_output) >= 1){
                        data_table_output2 <- data_table_output
                        return(datatable(data_table_output2, filter = "top"))
                }
        })

        
        # create output for map
        output$map <- renderLeaflet({
                data_table3 <- data_table()

                # select legend palette
                if(input$marker_type == "By program type"){
                        selected_pal <- program_pal
                }
                if(input$marker_type == "By fiscal year awarded"){
                        selected_pal <- year_pal
                }
                #                 if(input$marker_type == "By EDA funding level"){
                #                         selected_pal <- fund_pal
                #                 }
                
                # select legend title
                if(input$marker_type == "By program type"){
                        selected_title <- "EDA Program"
                }
                if(input$marker_type == "By fiscal year awarded"){
                        selected_title <- "Fiscal Year Awarded"
                }
                #                 if(input$marker_type == "By EDA funding level"){
                #                         selected_title <- fund_pal
                #                 }
                
                # select legend values
                if(input$marker_type == "By program type"){
                        selected_values <- data_table3$EDA.Program
                }
                if(input$marker_type == "By fiscal year awarded"){
                        selected_values <- factor(data_table3$FY)
                }
                #                 if(input$marker_type == "By EDA funding level"){
                #                         selected_values <- data_table3$EDA.
                #                 }
                
                # select circle size
                if(input$circle_size == "Small circles"){
                        selected_size <- 1
                }
                if(input$circle_size == "Large circles"){
                        selected_size <- 6
                }
                
                # build map
                data_table3_current <- data_table3[input$table_rows_current, ]
                leaflet(data_table3_current) %>%
                        addTiles() %>% 
                        addCircleMarkers(data = data_table3_current, lng = ~lon, lat = ~lat, popup = ~EDA.Program,
                              color = ~selected_pal(selected_values), opacity = 1, radius = selected_size) %>%
                        addLegend("bottomright", pal = selected_pal, values = selected_values,
                                  title = selected_title, opacity = 1)
        })
        
        # create download file
        output$downloadData <- downloadHandler(
                filename = function() {
                        str_c("datafile_", date, ".csv") 
                },
                content = function(file) {
                        write.csv(data_table3_current, file)
                }
        )
}

)
