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

# create default columns to display
default_columns <- c("Project.No.", "FY", "EDA_prog", "EDA.", "Appl.Short.Name", 
                     "Project.Short.Descrip", "Project.Location", "Proj.ST.Abbr")

# create program colors
program_options <- factor(c("Public Works", "Planning", "Econ Adjst", "Tech Asst", "Trade Adjst", "Disaster Supp",
                            "GCCMIF", "Research", "CTAA"))

year_options <- factor(seq(1995, 2016))

program_pal <- colorFactor("Set1", domain = program_options)

year_pal <- colorFactor("Set1", domain = year_options)

# create columns names in proper display order
# will need to specify columns to display once final dataset is arranged, since lat/lon etc aren't needed
non_default_columns <- names(datafile[ , !(names(datafile)%in% default_columns)])
column_display <- c(default_columns, non_default_columns)

# shiny server
shinyServer(function(input, output, session) {
        
        observe({
                reset_columns <- input$reset_columns
                updateSelectInput(session, "column_input",
                                  choices = column_display,
                                  selected = default_columns)
        })
        
        output$state <- renderUI({
                datafile2 <- arrange(datafile, Proj.ST.Abbr)
                state_choices <- c("All states", unique(datafile2$Proj.ST.Abbr))
                selectInput("state", "Select states:", choices = state_choices, multiple = TRUE, selected = state_choices[1])
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
                updateSelectInput(session, "counties", choices = counties_all, selected = counties_all[1])
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
        data_table3 <- reactive({
                data_table2 <- data_table2()
                range <- seq(input$years[1], input$years[2])
                data_table3 <- filter(data_table2, FY %in% range)
                data_table3
        })
        
        # run advanced query
        data_table4 <- eventReactive(input$submit_query, {
                data_table3 <- data_table3()
                data_table4 <- data.frame()
                
                # create if statements to handle "All programs" option in dropdown menu
                if("All programs" %in% input$program_input){
                        data_table4 <- data_table3                       
                }
                if(!("All programs" %in% input$program_input)){
                        data_table4 <- filter(data_table3, EDA_prog %in% input$program_input)                        
                }
                data_table4
        })
        
        # create reactive data_table with latest selected data
        data_table <- reactive({
                data_table <- data.frame()
                
                if(input$submit_query == 0){
                        data_table <- data_table3()
                        return(data_table)
                }
                if(input$submit_query > 0){
                        data_table <- data_table4()
                        return(data_table)
                }
        })
        
        
        output$table <- DT::renderDataTable({
                data_table_output <- data_table()
#                 data_table_output <- data_table3()
                data_table_output2 <- data.frame()
                
                
                # set columns to be displayed based on column dropdown menu
                if(is.null(input$column_input)){
                        data_table_output <- data_table_output[ , default_columns]
                } else {
                        data_table_output <- data_table_output[ , input$column_input]
                }
                
                # create placeholder dataframe to use when user-selected data has zero rows 
                # to avoid breaking datatable w/ filter
                no_projects2 <- data.frame("No projects found based on search")
                names(no_projects2)[1] <- ""
                
                # assign either the placeholder data or the user-selected data (if rows > 1) to be fed into datatable
                if(nrow(data_table_output) < 1){
                        data_table_output2 <- no_projects2
                        return(datatable(data_table_output2, filter = "none", rownames = FALSE))
                }
                if(nrow(data_table_output) >= 1){
                        data_table_output2 <- data_table_output
                        return(datatable(data_table_output2, filter = "top"))
                }
                server = TRUE
        })
        
        # create output for map
        output$map <- renderLeaflet({
                leaflet(datafile) %>% addTiles() %>%
                        fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
        })
        
        # create reactive variable for filtered data
        data_table5_filtered <- reactive({
                data_table5 <- data_table()
#                 data_table3 <- data_table3()
                if(nrow(data_table5) < 1){
                        no_projects <- data.frame("no projects")
                        return(no_projects)
                }
                if(nrow(data_table5) >= 1){
                        data_table5[input$table_rows_all, ]
                }
        })
        
        # clear markers every time data table updates
        observeEvent(input$table_rows_all, {
                leafletProxy("map") %>%
                        clearMarkers() %>%
                        clearControls()
        })
        
        # create reactive fund_pal seperately to avoid timeout/race conditions
        fund_pal <- reactive({
                data_table5_filtered <- data_table5_filtered()
                
                # only run if at least one row of data is selected
                if(data_table5_filtered[1,1] != "no projects"){
                
                        # create funds palette
                        colorNumeric(
                                palette = "Blues",
                                domain = data_table5_filtered$EDA.
                        )
                }
        })
        
        # create reactive selected_pal
        selected_pal <- reactive({
                fund_pal <- fund_pal()
                
                # select legend palette
                selected_pal <- program_pal
                if(input$marker_type == "By program type"){
                        selected_pal <- program_pal
                }
                if(input$marker_type == "By fiscal year awarded"){
                        selected_pal <- year_pal
                }
                if(input$marker_type == "By EDA funding level"){
                        selected_pal <- fund_pal
                }
                selected_pal
        })
        
        # create reactive selected_title
        selected_title <- reactive({
                # select legend title
                selected_title <- "EDA Program"
                if(input$marker_type == "By program type"){
                        selected_title <- "EDA Program"
                }
                if(input$marker_type == "By fiscal year awarded"){
                        selected_title <- "Fiscal Year Awarded"
                }
                if(input$marker_type == "By EDA funding level"){
                        selected_title <- "EDA Funding Level"
                }
                selected_title
        })
        
        # create reactive selected_values
        selected_values <- reactive({
                # select legend values
                data_table5_filtered <- data_table5_filtered()
                
                # only run if at least one row of data is selected
                if(data_table5_filtered[1,1] != "no projects"){
                
                        selected_values <- data_table5_filtered$EDA_prog
                        if(input$marker_type == "By program type"){
                                selected_values <- data_table5_filtered$EDA_prog
                        }
                        if(input$marker_type == "By fiscal year awarded"){
                                selected_values <- factor(data_table5_filtered$FY)
                        }
                        if(input$marker_type == "By EDA funding level"){
                                selected_values <- data_table5_filtered$EDA.
                        }
                        selected_values
                }
        })
        
        # create reactive selected_size
        selected_size <- reactive({
                # select circle size
                selected_size <- 7
                if(input$circle_size == "Small circles"){
                        selected_size <- 1
                }
                if(input$circle_size == "Large circles"){
                        selected_size <- 7
                }
                selected_size
        })
        
        # create label format for eda funds $
        selected_format <- reactive({
                # select circle size
                selected_format <- ""
                if(input$marker_type == "By EDA funding level"){
                        selected_format <- "$"
                }
                selected_format
        })
        
        # replace markers and legend whenever marker_type option is changed
        observeEvent(input$marker_type, {
                data_table5_filtered <- data_table5_filtered()
                
                # only run if at least one row of data is selected
                if(data_table5_filtered[1,1] != "no projects"){
                
                        default_popup <- str_c(data_table5_filtered$Appl.Short.Name, data_table5_filtered$address,
                                         str_c("FY", data_table5_filtered$FY, sep = " "),
                                       data_table5_filtered$EDA_prog, str_c("$", data_table5_filtered$EDA.), 
                                       sep = "<br/>")
                        
                        selected_pal <- selected_pal()
                        selected_title <- selected_title()
                        selected_values <- selected_values()
                        selected_size <- selected_size()
                        selected_format <- selected_format()
                        
                        leafletProxy("map", data = data_table5_filtered) %>%
                                clearMarkers() %>%
                                addCircleMarkers(data = data_table5_filtered, lng = ~lon, lat = ~lat, popup = default_popup,
                                                 color = ~selected_pal(selected_values), opacity = 1, radius = selected_size,
                                                 fillColor = ~selected_pal(selected_values), fillOpacity = .2) %>%
                                clearControls() %>%
                                addLegend("bottomright", pal = selected_pal, values = selected_values,
                                          title = selected_title, opacity = 1, labFormat = labelFormat(prefix = selected_format))
                }
        })
        
        # replace markers whenever cicle_size option is changed
        observeEvent(input$circle_size, {
                data_table5_filtered <- data_table5_filtered()
                
                # only run if at least one row of data is selected
                if(data_table5_filtered[1,1] != "no projects"){
                
                        default_popup <- str_c(data_table5_filtered$Appl.Short.Name, data_table5_filtered$address,
                                               str_c("FY", data_table5_filtered$FY, sep = " "),
                                               data_table5_filtered$EDA_prog, str_c("$", data_table5_filtered$EDA.), 
                                               sep = "<br/>")
                        
                        selected_pal <- selected_pal()
                        selected_title <- selected_title()
                        selected_values <- selected_values()
                        selected_size <- selected_size()
                        
                        leafletProxy("map", data = data_table5_filtered) %>%
                                clearMarkers() %>%
                                addCircleMarkers(data = data_table5_filtered, lng = ~lon, lat = ~lat, 
                                                 popup = default_popup,
                                                 color = ~selected_pal(selected_values), opacity = 1, radius = selected_size,
                                                 fillColor = ~selected_pal(selected_values), fillOpacity = .2)
                }
        })
        
        # replace map whenever navbar changes
        observeEvent(input$navbar, {
                data_table5_filtered <- data_table5_filtered()
                
                # only run if at least one row of data is selected
                if(data_table5_filtered[1,1] != "no projects"){
                
                        default_popup <- str_c(data_table5_filtered$Appl.Short.Name, data_table5_filtered$address,
                                               str_c("FY", data_table5_filtered$FY, sep = " "),
                                               data_table5_filtered$EDA_prog, str_c("$", data_table5_filtered$EDA.), 
                                               sep = "<br/>")
                        
                        selected_pal <- selected_pal()
                        selected_title <- selected_title()
                        selected_values <- selected_values()
                        selected_size <- selected_size()
                        selected_format <- selected_format()
                        
                        leafletProxy("map", data = data_table5_filtered) %>%
                                clearMarkers() %>%
                                addCircleMarkers(data = data_table5_filtered, lng = ~lon, lat = ~lat, 
                                                 popup = default_popup,
                                                 color  = ~selected_pal(selected_values), opacity = 1, radius = selected_size,
                                                 fillColor = ~selected_pal(selected_values), fillOpacity = .2) %>%
                                clearControls() %>%
                                addLegend("bottomright", pal = selected_pal, values = selected_values,
                                          title = selected_title, opacity = 1, labFormat = labelFormat(prefix = selected_format))
                }
        }) 
        
        output$rows_all <- renderText({
                input$submit_query
        })
        
        # create download file
        download_file <- reactive({
                data_table5_filtered <- data_table5_filtered()
                if(input$download_columns == TRUE){
                        return(data_table5_filtered[ , input$column_input])
                }
                if(input$download_columns == FALSE){
                        return(data_table5_filtered)
                }
        })
        
        # download file
        output$downloadData <- downloadHandler(
                filename = function() {
                        str_c("datafile_", date, ".csv") 
                },
                content = function(file) {
                        write.csv(download_file(), file)
                }
        )
}

)