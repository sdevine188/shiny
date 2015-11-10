library(shiny)
library(dplyr)
library(datasets)
library(leaflet)
library(stringr)
library(DT)

# provide data "as of date"
# date <- "20150827"
date <- "20151106"

# Read in data file
# file <- str_c("data/datafile_", date, ".csv")
 # file <- str_c("data/small_datafile_", date, ".csv")
 # file <- str_c("data/master_data_", date, ".csv")
 # datafile <- read.csv(file, stringsAsFactors = TRUE)
datafile_small <- readRDS("data/datafile_smallRDS.rds")
# datafile <- readRDS("data/datafile_smallRDS.rds") 
# datafile <- readRDS("data/datafile_mediumRDS.rds")

# if smallRDS is loaded first, then radio button to load full data, it takes 45 sec to load full data, and map refreshes

# datafile <- readRDS("data/datafile_fy2012_fy2016.rds")
datafile <- readRDS("data/datafile_fy2014_fy2016.rds") # 8.5 sec to load intiially
datafile_full <- readRDS("data/md.rds") # 45 sec to load second

 # faster to load into console R, but slower to load in shiny for some reason??
 # datafile <- read.csv(file, stringsAsFactors = TRUE) # 1 min 34/42 sec to load full 25k records in shiny
 # datafile <- readRDS("data/md.csv") # 1 min 24 sec to load full 25 k
 # datafile <- readRDS("data/md.rds") # 1 min 22 sec to load full 25k

# speed tests loading into shiny
# datafile <- read.csv("data/datafile_medium.csv", stringsAsFactors = FALSE)
# datafile <- readRDS("data/datafile_mediumRDS.csv") # this is way slower than read.csv
# datafile_full <- readRDS("data/datafile_mediumRDS.rds") # this is also probably slower than read.csv

# datafile <- read.csv("data/datafile_large.csv", stringsAsFactors = FALSE) # 20 seconds to load 10k records
# datafile <- readRDS("data/datafile_largeRDS.rds") # 48 seconds to load 10 k records

# create default columns to display
default_columns <- c("Project.No.", "FY", "Appr.Desc", "Best.EDA..", "Appl.Short.Name", 
                     "Project.Short.Descrip", "Appl.City.Name", "Proj.ST.Abbr")

# create program colors
# program_options <- factor(c("Public Works", "Planning", "Econ Adjst", "Tech Asst", "Trade Adjst", "Disaster Supp",
#                             "GCCMIF", "Research", "CTAA"))
program_options <- unique(datafile$Appr.Desc)

year_options <- factor(seq(1995, 2016))

program_pal <- colorFactor("Set1", domain = program_options)

year_pal <- colorFactor("Set1", domain = year_options)

# create columns names in proper display order
# will need to specify columns to display once final dataset is arranged, since lat/lon etc aren't needed
non_default_columns <- names(datafile[ , !(names(datafile)%in% default_columns)])
column_display <- c(default_columns, non_default_columns)

# read in initatives data
initiatives <- read.csv("data/initiatives.csv", stringsAsFactors = FALSE)
initiatives_display <- unique(initiatives$code_description)

# create is.even and is.odd functions
is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0

# display trace logs
# options(shiny.trace=TRUE)

# shiny server
shinyServer(function(input, output, session){
        
        # create as_of_date display
        output$as_of_date <- renderText({
                str_c("Data as of: ", date)
        })
        
        # create query_term output variable
        query_term <- reactive({
                input$enter_query_term
                var1 <- ""
                term1 <- ""
                isolate({
                        if(!(is.null(input$text_var1_input)) && !(input$text_term1_input == "")){
                                var1 <- input$text_var1_input
                                term1 <- input$text_term1_input
                                term_final <- NULL
                                if(grepl("\\|", term1)){
                                        term1_split <- str_split(term1, "\\|")
                                        term_count <- length(term1_split[[1]])
                                        for(i in 1:term_count){
                                                term_piece <- term1_split[[1]][i]
                                                term_piece <- str_c("'", term_piece, "' | ")
                                                term_final <- append(term_final, term_piece)
                                        }
                                } else {
                                        term_final <- str_c("'", term1, "'")
                                }
                                term_final <- str_c(term_final, collapse = "")
                                if(str_sub(term_final, start = -2, end = -2) == "|"){
                                        term_final <- str_sub(term_final, start = 1, end = nchar(term_final)-3)
                                }
                                str_c(var1, " contains (", term_final, ")")
                        }
                })
        })
        
        # create reactiveValues variable, which can be updated from observers
        query_term_placeholder <- reactiveValues(
                value = ""
        )
        
        # create output version of query_term_placeholder
        output$query_term_output <- reactive({
                query_term_output <- query_term_placeholder$value
                query_term_output
        })
        
        # create observers to update query_term_placeholder
        observe({
                query_term <- query_term()
                isolate({
                        if(query_term_placeholder$value != ""){
                                query_term_placeholder$value <- str_c(query_term_placeholder$value, " & ", query_term)
                        }
                        if(query_term_placeholder$value == ""){
                                query_term_placeholder$value <- query_term
                        }
                })
        })
        
        # reset text query button
        observe({
                reset_text_query <- input$reset_text_query
                updateSelectInput(session, "text_var1_input",
                                  choices = column_display)
                updateTextInput(session, "text_term1_input", value = "")
                query_term_placeholder$value <- ""
        })
        
        # reset initiatives button
        observe({
                reset_initiatives <- input$reset_initiatives
                updateSelectInput(session, "initiatives_input",
                                  choices = initiatives_display)
        })
        
        # reset columns button
        observe({
                reset_columns <- input$reset_columns
                updateSelectInput(session, "column_input",
                                  choices = column_display,
                                  selected = default_columns)
        })
        
        # reset program button
        observe({
                reset_programs <- input$reset_programs
                updateSelectInput(session, "program_input",
                                  choices = c("All programs", as.character(program_options)),
                                  selected = "All programs")
        })
        
        # reset all button
        observe({
                reset_all <- input$reset_all
                updateSelectInput(session, "program_input",
                                  choices = c("All programs", as.character(program_options)),
                                  selected = "All programs")
                updateSelectInput(session, "column_input",
                                  choices = column_display,
                                  selected = default_columns)
                updateSelectInput(session, "initiatives_input",
                                  choices = initiatives_display)
                updateSelectInput(session, "text_var1_input",
                                  choices = column_display)
                updateTextInput(session, "text_term1_input", value = "")
                query_term_placeholder$value <- ""
        })
        
        # create reactive variable to update whenever any advanced query options are reset
        reset_any <- reactive({
                reset_columns <- input$reset_columns
                reset_programs <- input$reset_programs
                reset_initiatives <- input$reset_initiatives
                reset_text_query <- input$reset_text_query
                reset_all <- input$reset_all
                str_c(reset_columns, reset_programs, reset_initiatives, reset_text_query, reset_all)
        })
        
        # build data_table3 to start assembly line for data output
        data_table2 <- reactive({
#                 range <- seq(input$years[1], input$years[2])
#                 data_table3 <- filter(datafile_full, FY %in% range)
#                 data_table3
#                 datafile
                
                
                if(input$datafile_radio == "FY 2014 to FY 2016"){
                        return(datafile)
                }
                if(input$datafile_radio == "FY 1995 to FY 2016"){
                        return(datafile_full)
                }
        })
        
        # subset data to include/not include jobs or PI columns and show/not show Construction-only projects based on checkbox input
        data_table3 <- reactive({
                data_table2 <- data_table2()
                
                if(input$JobsPIFlag == FALSE){
                        data_table3 <- select(data_table2, -Jobs.Created, -Jobs.Saved, -Private.Investment)
                }
                if(input$JobsPIFlag == TRUE){
                        data_table3 <- filter(data_table2, Cons.Non == "C" | Cons.Non == "B")
                }      
                data_table3
        })
        
        # tried to build a submit button for radio button
#         observe({
#                 if(input$radio_submit == 0){
#                         data_table_placeholder$df <- datafile
#                 }
#                 if(input$radio_submit != 0){
#                         if(input$datafile_radio == "FY 2012 to FY 2016"){
#                                 data_table_placeholder$df <- datafile
#                         }
#                         if(input$datafile_radio == "FY 1995 to FY 2016"){
#                                 data_table_placeholder$df <- datafile_full
#                         }
#                 }  
#         })
        
#         data_table3 <- reactive({
#                 data_table_placeholder$df
#         })
        
        # filter data based on advanced query inputs
        data_table4 <- reactive({
                data_table3 <- data_table3()
                # data_table3 <- data_table_placeholder$df
                data_table4 <- data.frame()
                submit_query <- input$submit_query
                reset_any <- reset_any()
                
                isolate({
                        # create if statements to handle "All programs" option in dropdown menu
                        if("All programs" %in% input$program_input){
                                data_table4 <- data_table3
                        }
                        if(!("All programs" %in% input$program_input)){
                                data_table4 <- filter(data_table3, Appr.Desc %in% input$program_input)
                        }
                        
                        # create if statements to handle initiatives dropdown menu
                        if(is.null(input$initiatives_input)){
                                data_table4 <- data_table4
                        }
                        if(!(is.null(input$initiatives_input))){
                                selected_initiatives_df <- filter(initiatives, code_description %in% 
                                                                          input$initiatives_input)
                                selected_initiative_codes <- selected_initiatives_df$code
                                selected_codes_index <- grep(str_c(selected_initiative_codes, collapse = "|"), 
                                                             data_table4$Initiatives, ignore.case = TRUE)
                                data_table4 <- data_table4[selected_codes_index, ]
                        }
                        data_table4
                        
                        # create if statements to handle query term
#                         if(query_term_placeholer$value == ""){
#                                 data_table4 <- data_table4
#                         }
#                         if(query_term_placeholder$value != ""){
#                                 term_pieces <- str_split(query_term_placeholder$value, "contains (")
#                         }
                        
                     
                })
        })
        
        # create variable data_table with latest selected data from advanced query, if necessary
        data_table <- reactive({
                data_table <- data.frame()
                data_table3 <- data_table3()
                # data_table3 <- data_table_placeholder$df
                data_table4 <- data_table4()
                
                # consider isolating this chunk?
                if(input$submit_query == 0){
                        data_table <- data_table3
                        return(data_table)
                }
                if(input$submit_query > 0){
                        data_table <- data_table4
                        return(data_table)
                }
        })
        
        # create table output
        output$table <- DT::renderDataTable({
                data_table_output <- data_table()
                data_table_output2 <- data.frame()
                
                
                # set columns to be displayed based on column dropdown menu
                if(is.null(input$column_input)){
                        data_table_output <- data_table_output[ , default_columns]
                } else {
                        data_table_output <- data_table_output[ , input$column_input]
                }
                
                # create placeholder dataframe to use when user-selected data has zero rows 
                # to avoid breaking datatable w/ filter
                no_projects2 <- data.frame("No matching records found")
                names(no_projects2)[1] <- ""
                
                # assign either the placeholder data or the user-selected data (if rows > 1) to be fed into datatable
                if(nrow(data_table_output) < 1){
                        data_table_output2 <- no_projects2
                        return(datatable(data_table_output2, filter = "none", rownames = FALSE, options = 
                                                 list(pageLength = 5)))
                }
                if(nrow(data_table_output) >= 1){
                        data_table_output2 <- data_table_output
                        # data_table_output2$FY <- as.factor(as.character(data_table_output2$FY))
                        data_table_output2$Project.No. <- as.factor(data_table_output2$Project.No.)
                        return(datatable(data_table_output2, filter = "top", options = list(pageLength = 5)))
                }
                server = TRUE
        })
        
        output$map <- renderLeaflet({
                leaflet(datafile_small) %>% addTiles() %>%
                        fitBounds(-160.0583, 20.65798, -60.954694, 60.60825)
        })
        
        # create reactive variable for filtered data
        data_table5_filtered <- reactive({
                data_table5 <- data_table()
                table_rows_all <- input$table_rows_all
                
                isolate({
                        search_input <- str_c(input$table_search, input$table_search_columns, collapse = "")
                })
                
                if(nrow(data_table5) < 1){
                        no_projects <- data.frame("no projects")
                        return(no_projects)
                }
                if(nrow(data_table5) >= 1 && search_input == ""){
                        return(data_table5)
                }
                if(nrow(data_table5) >= 1 && search_input != ""){
                        return(data_table5[input$table_rows_all, ])
                }
        })
        
        # update map when hitting refresh_map button
        observeEvent(input$refresh_map, {
                data_table5_filtered <- data_table5_filtered()
                
                data_table5_filtered <- select(data_table5_filtered, Appl.Short.Name, address, FY, Appr.Desc, Best.EDA.., lat, lon)
                data_table5_filtered <- na.omit(data_table5_filtered)
                
                # only run if at least one row of data is selected
                if(data_table5_filtered[1,1] != "no projects"){
                
                default_popup <- str_c(data_table5_filtered$Appl.Short.Name, data_table5_filtered$address,
                                       str_c("FY", data_table5_filtered$FY, sep = " "),
                                       data_table5_filtered$Appr.Desc, str_c("$", data_table5_filtered$Best.EDA..), 
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
        
        # create reactive fund_pal seperately to avoid timeout/race conditions
        fund_pal <- reactive({
                data_table5_filtered <- data_table5_filtered()
                
                # only run if at least one row of data is selected
                if(data_table5_filtered[1,1] != "no projects"){
                        
                        # create funds palette
                        colorNumeric(
                                palette = "Blues",
                                domain = data_table5_filtered$Best.EDA..
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
                        
                        selected_values <- data_table5_filtered$Appr.Desc
                        if(input$marker_type == "By program type"){
                                selected_values <- data_table5_filtered$Appr.Desc
                        }
                        if(input$marker_type == "By fiscal year awarded"){
                                selected_values <- factor(data_table5_filtered$FY)
                        }
                        if(input$marker_type == "By EDA funding level"){
                                selected_values <- data_table5_filtered$Best.EDA..
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
        
        output$rows_all <- renderText({
                input$navbar == "View map"  
#                 term_pieces <- NULL
#                 term_value <- NULL
#                 term_var <- NULL
#                 
#                 if(query_term_placeholder$value != ""){
#                         term_pieces <- str_split(query_term_placeholder$value, "contains \\(")
#                         term_pieces <- unlist(term_pieces)
#                         term_pieces <- str_sub(term_pieces, start = 1, end = -2)
#                         term_pieces <- str_split(term_pieces, "\\) & ")
#                         term_pieces <- unlist(term_pieces)
#                         for(i in 1:length(term_pieces)){
#                                 if(is.even(i)){
#                                         term_value <- append(term_value, term_pieces[i])
#                                 }
#                                 if(is.odd(i)){
#                                         term_var <- append(term_var, term_pieces[i])
#                                 }
#                         }
#                 }
# 
#                 # need to get rid of single quotes
#                 for(i in 1:length(term_value)){
#                         if(!(grepl(" | ", term_value[i]))){
#                                 term_value[i] <- str_sub(term_value[i], start = 2, end = -2)
#                         }
#                         if(grepl(" | ", term_value[i])){
#                                 new_value <- str_sub(term_value[i], start = 2, end = -2)
#                                 new_value <- str_replace_all(new_value, "' \\| '", " | ")
#                                 term_value[i] <- new_value
#                         }
#                 }
#                 term_value
#                 # term_var
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