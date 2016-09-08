list.of.packages <- c("shiny", "dplyr", "datasets", "leaflet", "stringr", "rjson", "readr", "lubridate", "rgdal", "devtools", 
                      "shinythemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0){
        install.packages(new.packages)
}
rstudio_DT_not_installed <- !("DT" %in% installed.packages()[,"Package"])
if(rstudio_DT_not_installed){
        devtools::install_github(c("rstudio/DT"))
}
rpivotTable_not_installed <- !("rpivotTable" %in% installed.packages()[,"Package"])
if(rpivotTable_not_installed){
        devtools::install_github(c("ramnathv/htmlwidgets", "smartinsightsfromdata/rpivotTable"))
}

library(shiny)
library(dplyr)
library(datasets)
library(leaflet)
library(stringr)
library(DT)
library(rjson)
library(readr)
library(lubridate)
library(rgdal)
library(rpivotTable)

# Read in data
# find current shiny data filename
shiny_data_filename <- list.files(str_c(getwd(), "/data"))[str_detect(list.files(str_c(getwd(), "/data")), "shiny_app_data_20")]
shiny_data_filename <- str_c("data/", shiny_data_filename)
# read main data file
datafile <- data.frame(read_csv(shiny_data_filename))

# read small data to start map without delay
datafile_small <- read_csv("data/shiny_app_data_small_20160209.csv")

# provide data "as of date"
# date <- "20160209"
as_of_date <- str_sub(shiny_data_filename, str_locate(shiny_data_filename, "20")[1], str_locate(shiny_data_filename, ".csv")[1] - 1)
as_of_date <- str_replace(ymd(as_of_date), " UTC", "")

# convert Control.No. to a character, so it can be searched with table filter, instead of numeric slider
# can place this code in clean_shiny_data script later
datafile$Control.No. <- as.character(datafile$Control.No.)

# create map color palettes
program_options <- unique(datafile$Program)
appropriation_options <- unique(datafile$Appropriation)
year_options <- factor(seq(1995, 2016))

program_pal <- colorFactor("Set1", domain = program_options)
appropriation_pal <- colorFactor("Set1", domain = appropriation_options)
year_pal <- colorFactor("Set1", domain = year_options)

# create default columns to display
default_columns <- c("Control.No.", "Status", "FY", "Program", "EDA.Funding", "Appl.Short.Name", 
                     "Project.Short.Descrip", "Initiatives", "Proj.State.Abbr", "Region.Name")

# create columns names in proper display order
# will need to specify columns to display once final dataset is arranged, since lat/lon etc aren't needed
non_default_columns <- names(datafile[ , !(names(datafile)%in% default_columns)])
column_display <- sort(c(default_columns, non_default_columns))

# read in initatives data
initiatives <- read.csv("data/initiatives.csv", stringsAsFactors = FALSE)
initiatives_display <- unique(initiatives$code_description)

# create is.even and is.odd functions
is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0

# display trace logs
# options(shiny.trace=TRUE)

# create state list for select state input
# note there are six records of 25k master data with "" for appl state, 
# these will only show when all states is selected bc we drop them so there is no "" showing in select state menu
state_arr <- arrange(datafile, Appl.State.Abbr)
state_list <- unique(state_arr$Appl.State.Abbr)
state_list <- state_list

# load geographic boundary data
state_boundaries <- readOGR("data/cb_2015_us_state_20m/cb_2015_us_state_20m.shp",
                  layer = "cb_2015_us_state_20m", verbose = FALSE)
cd_boundaries <- readOGR("data/cb_2014_us_cd114_20m/cb_2014_us_cd114_20m.shp",
                         layer = "cb_2014_us_cd114_20m", verbose = FALSE)
county_boundaries <- readOGR("data/cb_2015_us_county_20m/cb_2015_us_county_20m.shp",
                            layer = "cb_2015_us_county_20m", verbose = FALSE)

# log user accessing Grants Viewer
user <- Sys.info()[6]
date <- str_split(Sys.time(), " ")[[1]][1]
time <- str_split(Sys.time(), " ")[[1]][2]
time_zone <- Sys.timezone(location = FALSE)
time <- str_c(time, time_zone, sep = " ")
log_entry <- data.frame(user, date, time, row.names = NULL)
log <- read_csv("G:/Shared/EDA ALL INFORMATION/Performance and Budget Metrics/EDA Grants Viewer/log/log.csv")
log <- rbind(log, log_entry)
write_csv(log, "G:/Shared/EDA ALL INFORMATION/Performance and Budget Metrics/EDA Grants Viewer/log/log.csv")

# shiny server
shinyServer(function(input, output, session){

        # create as_of_date display
        output$as_of_date <- renderText({
                str_c("Data as of: ", as_of_date)
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
        
        # create variable for selected_states
        state_data <- reactive({
                # create placeholder selected_states variable to assign in if statements
                selected_states <- c()                
                
                if("All states" %in% input$state){
                        selected_states <- unique(datafile$Proj.State.Abbr)
                }
                if(!("All states" %in% input$state)){
                        selected_states <- input$state
                }
                
                return(filter(datafile, Proj.State.Abbr %in% selected_states))
        })
        
        # populate the select state input menu
        observe({
                states_all <- c("All states", state_list)
                updateSelectInput(session, "state", choices = states_all, selected = states_all[1])
        })
        
        # filter data based on year input
        data_table2 <- reactive({
                data_table1 <- state_data()
                range <- seq(input$years[1], input$years[2])
                data_table2 <- filter(data_table1, FY %in% range)
                data_table2
        })
        
        # filter data based on advanced query inputs
        data_table4 <- reactive({
                data_table3 <- data_table2()
                data_table4 <- data.frame()
                submit_query <- input$submit_query
                reset_any <- reset_any()
                
                isolate({
                        # create if statements to handle "All programs" option in dropdown menu
                        if("All programs" %in% input$program_input){
                                data_table4 <- data_table3
                        }
                        if(!("All programs" %in% input$program_input)){
                                data_table4 <- filter(data_table3, Program %in% input$program_input)
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
                        
                        # create if statements to handle query term
                        if(query_term_placeholder$value == ""){
                                data_table4 <- data_table4
                        }
                        if(query_term_placeholder$value != ""){
                                data_table_placeholder <- data.frame(data_table4)        
                                term_value <- NULL
                                term_var <- NULL
                                term_pieces <- NULL
                                if(query_term_placeholder$value != ""){
                                        term_pieces <- str_split(query_term_placeholder$value, "contains \\(")
                                        term_pieces <- unlist(term_pieces)
                                        term_pieces <- str_sub(term_pieces, start = 1, end = -2)
                                        term_pieces <- str_split(term_pieces, "\\) & ")
                                        term_pieces <- unlist(term_pieces)
                                        for(i in 1:length(term_pieces)){
                                                if(is.even(i)){
                                                        term_value <- append(term_value, term_pieces[i])
                                                }
                                                if(is.odd(i)){
                                                        term_var <- append(term_var, term_pieces[i])
                                                }
                                        }
                                }
                                
                                # need to get rid of single quotes
                                for(i in 1:length(term_value)){
                                        if(!(grepl(" | ", term_value[i]))){
                                                term_value[i] <- str_sub(term_value[i], start = 2, end = -2)
                                        }
                                        if(grepl(" | ", term_value[i])){
                                                new_value <- str_sub(term_value[i], start = 2, end = -2)
                                                new_value <- str_replace_all(new_value, " ' \\| ' ", "|")
                                                term_value[i] <- new_value
                                        }
                                }
                                
                                # filter datatable based on query term
                                for(i in 1:length(term_value)){
                                        data_table_placeholder <- filter(data_table_placeholder, 
                                                                         grepl(term_value[i], data_table_placeholder[ , term_var[i]], 
                                                                               ignore.case = TRUE))
                                }
                                data_table4 <- data_table_placeholder
                        }
                        data_table4
                })
        })
        
        # create variable storing the current column filter keywords
        column_filter_keywords <- reactive({
                keyword_series <- c()
                for(i in 1:length(input$table_state$columns)){
                        # use 2 as starting index since 2 is the index for the first column, not sure what table_state$columns[[1]] refers to??
                        keyword_series <- c(keyword_series, input$table_state$columns[[i]]$search$search)
                }
                keyword_series
        })
        
        # create variable storing any column filters loaded by a saved query
        column_keywords_list <- reactive({
                column_filter_keywords_loaded <- NULL
                
                # refresh variable when saved_query_radio updates
                input$saved_query_radio
                uploaded_query <- uploaded_query()
                if(input$saved_query_radio == "Apply uploaded query"  && !(is.null(uploaded_query))){
                        column_filter_keywords_loaded <- uploaded_query$column_filter_keywords
                }

                # create function to convert keywords into the list needed by DT function
                create_column_keywords_list <- function(keyword_series) {
                        column_keywords_list <- list()
                        if(length(keyword_series) > 0) {
                                for(i in 1:length(keyword_series)) {
                                        if(keyword_series[i] == "") {
                                                column_keywords_list[[i]] <- NULL
                                        } else {
                                                column_keywords_list[[i]] <- list(search = keyword_series[i])
                                        }
                                        
                                }
                                column_keywords_list
                        } else {
                                column_keywords_list[[1]] <- NULL
                        }
                }
                
                column_keywords_list <- create_column_keywords_list(column_filter_keywords_loaded)
                
                column_keywords_list
        })

        # create variable storing the current global search keywords
        global_search_keywords <- reactive({
                global_input <- input$table_state$search[[1]]
                global_input
        })
        
        # create variable storing any global search keywords loaded by a saved query
        global_keywords_list <- reactive({
                global_search_keywords_loaded <- ""
                
                # refresh variable when saved_query_radio updates
                input$saved_query_radio
                uploaded_query <- uploaded_query()
                if(input$saved_query_radio == "Apply uploaded query"  && !(is.null(uploaded_query))){
                        global_search_keywords_loaded <- uploaded_query$global_search_keywords
                }
                
                # create function to convert keywords into the list needed by DT function
                create_global_keywords_list <- function(global_input) {
                        global_keywords_list <- list(regex = TRUE, caseInsensitive = TRUE, search = global_input)
                        global_keywords_list
                }
                
                global_keywords_list <- create_global_keywords_list(global_search_keywords_loaded)
                global_keywords_list
        })
        
        # create variable storing the full query term submitted so it can be downloaded
        saved_query <- reactive({
             program_input <- input$program_input
             initiatives_input <- input$initiatives_input
             query_term <- query_term_placeholder$value
             column_input <- input$column_input
             download_columns <- input$download_columns
             state <- input$state
             years <- input$years
             column_filter_keywords <- column_filter_keywords()
             global_search_keywords <- global_search_keywords()
             saved_query_list <- list(program_input, initiatives_input, query_term, column_input, download_columns, 
                                      state, years, column_filter_keywords, global_search_keywords)
             names(saved_query_list) <- c("program_input", "initiatives_input", "query_term", "column_input", "download_columns",
                                          "state", "years", "column_filter_keywords", "global_search_keywords")
             saved_query_list_json <- toJSON(saved_query_list)
             saved_query_list_json
        })
        
        # process an uploaded saved query
        uploaded_query <- reactive({
                uploaded_file <- input$file_input
                if(!(is.null(uploaded_file))){
                        fromJSON(file = uploaded_file$datapath)
                }
        })
        
        # apply an uploaded query
        observe({
                input$saved_query_radio
                isolate({
                        uploaded_query <- uploaded_query()
                        if(input$saved_query_radio == "Apply uploaded query"  && !(is.null(uploaded_query))){
                                updateSelectInput(session, "program_input",
                                                  choices = c("All programs", as.character(program_options)),
                                                  selected = uploaded_query$program_input)
                                updateSelectInput(session, "column_input",
                                                  choices = column_display,
                                                  selected = uploaded_query$column_input)
                                updateSelectInput(session, "initiatives_input",
                                                  choices = initiatives_display, selected = uploaded_query$initiatives_input)
                                query_term_placeholder$value <- uploaded_query$query_term
                                updateCheckboxInput(session, "download_columns", value = uploaded_query$download_columns)
                                # counties_all <- c("All counties", as.character(counties()))
                                # updateSelectInput(session, "counties", choices = counties_all, selected = uploaded_query$counties)
                                states_all <- c("All states", state_list)
                                updateSelectInput(session, "state", choices = states_all, selected = uploaded_query$state)
                                updateSliderInput(session, "years", value = c(uploaded_query$years[1], uploaded_query$years[2]))
                        }
                })
        })
        
        
        # create variable data_table with latest selected data from advanced query, if necessary
        data_table <- reactive({
                data_table <- data.frame()
                # data_table3 <- data_table3()
                data_table3 <- data_table2()
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
                
                # assign either the placeholder data or the user-selected data (if rows < 1) to be fed into datatable
                if(nrow(data_table_output) < 1){
                        data_table_output2 <- no_projects2
                        
                        return(DT::datatable(data_table_output2, filter = "none", rownames = FALSE, options = list(pageLength = 5)))
                }
                
                # build datatable if rows > 1
                if(nrow(data_table_output) >= 1){
                        data_table_output2 <- data_table_output
                        data_table_output2$Status <- factor(data_table_output2$Status)
                        data_table_output2$Program <- factor(data_table_output2$Program)
                        data_table_output2$Region.Name <- factor(data_table_output2$Region.Name)
                        
                        # add column filter and global search keywords uploaded from saved query 
                        isolate({
                                column_keywords_list <- column_keywords_list()
                        })
                        isolate({
                                global_keywords_list <- global_keywords_list()
                        })
                        
                        # build datatable
                        DT::datatable(data_table_output2, filter = "top", options = list(pageLength = 5, stateSave = TRUE,
                                                                                         searchCols = column_keywords_list,
                                                                                         search = global_keywords_list),
                                      callback = DT::JS("$(window).unload(function() { table.state.clear(); })"))
                }
        },
                server = TRUE
        )
        
        output$map <- renderLeaflet({
                leaflet(datafile_small) %>% addTiles() %>%
                        fitBounds(-160.0583, 20.65798, -60.954694, 60.60825)
        })
        
        output$map_boundaries <- renderLeaflet({
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
                data_table5_filtered <- select(data_table5_filtered, Control.No., Appl.FIPS.ST, Appl.FIPS.Cnty, Appl.Cong.Dist, 
                                               Appl.Short.Name, app_address, FY, Program, Appropriation, 
                                               EDA.Funding, app_lat, app_lon)
                data_table5_filtered <- filter(data_table5_filtered, !is.na(app_lat), !is.na(app_lon))
                
                # update map_marker, if selected, when refresh_map button is hit
                if(input$map_radio == "Map with application icons"){
                        
                        # only run if at least one row of data is selected
                        if(data_table5_filtered[1,1] != "no projects"){
                                
                                popup_control <- sapply(data_table5_filtered$Control.No., function(x) ifelse(is.na(x), "NA", x))
                                popup_control <- sapply(popup_control, function(x) str_c("Control #: ", x))
                                popup_app_name <- sapply(data_table5_filtered$Appl.Short.Name, function(x) ifelse(is.na(x), "NA", x))
                                popup_app_name <- sapply(popup_app_name, function(x) str_c("Applicant name: ", x))
                                popup_app_address <- sapply(data_table5_filtered$app_address, function(x) ifelse(is.na(x), "NA", x))
                                popup_app_address <- sapply(popup_app_address, function(x) str_c("Applicant Address: ", x))
                                popup_fy <- sapply(data_table5_filtered$FY, function(x) ifelse(is.na(x), "NA", x))
                                popup_fy <- sapply(popup_fy, function(x) str_c("Fiscal year: FY ", x))
                                popup_program <- sapply(data_table5_filtered$Program, function(x) ifelse(is.na(x), "NA", x))
                                popup_program <- sapply(popup_program, function(x) str_c("Program: ", x))
                                popup_appropriation <- sapply(data_table5_filtered$Appropriation, function(x) ifelse(is.na(x), "NA", x))
                                popup_appropriation <- sapply(popup_appropriation, function(x) str_c("Appropriation: ", x))
                                popup_funds <- sapply(data_table5_filtered$EDA.Funding, function(x) ifelse(is.na(x), "NA", x))
                                popup_funds <- sapply(popup_funds, function(x) str_c("EDA funds: $", prettyNum(x, big.mark = ",",
                                                        scientific = FALSE)))
                                
                                default_popup <- str_c(popup_control, popup_app_name, popup_app_address, popup_fy, popup_program,
                                                       popup_appropriation, popup_funds, sep = "<br/>")
                                
                                selected_pal <- selected_pal()
                                selected_title <- selected_title()
                                selected_size <- selected_size()
                                selected_format <- selected_format()
                                selected_values <- if(data_table5_filtered[1,1] != "no projects"){
                
                                        selected_values <- data_table5_filtered$Program
                                        if(input$marker_type == "By program"){
                                                selected_values <- data_table5_filtered$Program
                                        }
                                        if(input$marker_type == "By appropriation"){
                                                selected_values <- data_table5_filtered$Appropriation
                                        }
                                        if(input$marker_type == "By fiscal year awarded"){
                                                # selected_values <- factor(data_table5_filtered$FY)
                                                selected_values <- data_table5_filtered$FY
                                        }
                                        if(input$marker_type == "By EDA funding level"){
                                                selected_values <- data_table5_filtered$EDA.Funding
                                        }
                                        selected_values
                                }
                               
                                selected_values_placeholder$value <- selected_values
                                
                                leafletProxy("map", data = data_table5_filtered) %>%
                                        clearMarkers() %>%
                                        addCircleMarkers(data = data_table5_filtered, lng = ~app_lon, lat = ~app_lat, popup = default_popup,
                                                         color = ~selected_pal(selected_values), opacity = 1, radius = selected_size,
                                                         fillColor = ~selected_pal(selected_values), fillOpacity = 0) %>%
                                        clearControls() %>%
                                        addLegend("bottomright", pal = selected_pal, values = selected_values,
                                                  title = selected_title, opacity = 1, labFormat = labelFormat(prefix = selected_format))
                        }
                }
                
                # update map_boundaries, if selected, when refresh_map button is hit
                if(input$map_radio == "Map with geographic boundaries"){
                        
                        if(data_table5_filtered[1,1] != "no projects"){
                                
                                if(input$map_geography == "State"){
                                
                                        # update map shape file with funding data for selected records
                                        datafile_map <- data_table5_filtered %>% filter(!is.na(Appl.FIPS.ST), EDA.Funding < 50000000) %>% 
                                                select(Appl.FIPS.ST, EDA.Funding)
                                        state_list <- data.frame("state_fips" = state_boundaries$STATEFP)
                                        state_list$state_fips <- state_list$state_fips
                                        state_list <- filter(state_list, state_fips %in% unique(datafile_map$Appl.FIPS.ST))
                                        state_funding <- datafile_map %>% group_by(Appl.FIPS.ST) %>% summarize(funding = sum(EDA.Funding), count = n())
                                        map_data <- left_join(state_list, state_funding, by = c("state_fips" = "Appl.FIPS.ST"))
                                        map_data$state_fips <- factor(str_pad(map_data$state_fips, width = 2, side = "left", pad = "0"))
                                        map_boundaries <- subset(state_boundaries, state_boundaries$STATEFP %in% unique(map_data$state_fips))
                                        map_boundaries$funding <- map_data$funding
                                        map_boundaries$count <- map_data$count
                                        
                                        default_popup <- str_c(str_c("State: ", map_boundaries$NAME),
                                                               str_c("EDA funds requested: $", prettyNum(map_boundaries$funding, big.mark = ",", scientific = FALSE),  sep = ""),
                                                               str_c("Count of applications: ", prettyNum(map_boundaries$count, big.mark = ",", scientific = FALSE)),
                                                               sep = "<br/>")
                                }
                                
                                if(input$map_geography == "Congressional District"){
                                        datafile_map <- data_table5_filtered %>% filter(!is.na(Appl.FIPS.ST), !is.na(Appl.Cong.Dist), EDA.Funding < 50000000) %>% 
                                                select(Appl.FIPS.ST, Appl.Cong.Dist, EDA.Funding)
                                        datafile_map$state_cd_fips <- str_c(str_pad(datafile_map$Appl.FIPS.ST, width = 2, side = "left", pad = "0"),
                                                                            str_pad(datafile_map$Appl.Cong.Dist, width = 2, side = "left", pad = "0"))
                                        cd_list <- data.frame("state_fips" = cd_boundaries$STATEFP, "cd_fips" = cd_boundaries$CD114FP)
                                        for(i in 1:nrow(cd_list)){
                                                if(cd_list$cd_fips[i] == "00"){
                                                        cd_list$cd_fips[i] <- "01"
                                                }
                                        }
                                        cd_list$cd_fips <- str_pad(cd_list$cd_fips, width = 2, side = "left", pad = "0")
                                        cd_list$state_cd_fips <- str_pad(str_c(cd_list$state_fips, cd_list$cd_fips), width = 4, 
                                                                         side = "left", pad = "0")
                                        cd_boundaries$state_cd_fips <- cd_list$state_cd_fips
                                        cd_list <- filter(cd_list, as.character(state_cd_fips) %in% unique(datafile_map$state_cd_fips))
                                        cd_funding <- datafile_map %>% group_by(state_cd_fips) %>% summarize(funding = sum(EDA.Funding), count = n())
                                        map_data <- left_join(cd_list, cd_funding, by = c("state_cd_fips" = "state_cd_fips"))
                                        map_data$state_cd_fips <- factor(map_data$state_cd_fips)
                                        map_boundaries <- subset(cd_boundaries, cd_boundaries$state_cd_fips %in% unique(map_data$state_cd_fips))
                                        map_boundaries$funding <- map_data$funding
                                        map_boundaries$count <- map_data$count
                                        
                                        default_popup <- str_c(str_c("Congressional District: ", map_boundaries$CD114FP),
                                                               str_c("EDA funds requested: $", prettyNum(map_boundaries$funding, big.mark = ",", scientific = FALSE),  sep = ""),
                                                               str_c("Count of applications: ", prettyNum(map_boundaries$count, big.mark = ",", scientific = FALSE)),
                                                               sep = "<br/>")
                                }
                                
                                if(input$map_geography == "County"){
                                        
                                        # update map shape file with funding data for selected records
                                        datafile_map <- data_table5_filtered %>% 
                                                filter(!is.na(Appl.FIPS.ST), !is.na(Appl.FIPS.Cnty), EDA.Funding < 50000000) %>% 
                                                select(Appl.FIPS.ST, Appl.FIPS.Cnty, EDA.Funding)
                                        datafile_map$state_county_fips <- str_c(str_pad(datafile_map$Appl.FIPS.ST, width = 2, side = "left", pad = "0"),
                                                                                str_pad(datafile_map$Appl.FIPS.Cnty, width = 3, side = "left", pad = "0"))
                                        county_list <- data.frame("state_fips" = county_boundaries$STATEFP, "county_fips" = county_boundaries$COUNTYFP)
                                        county_list$state_county_fips <- str_pad(str_c(county_list$state_fips, county_list$county_fips), width = 5, 
                                                                                 side = "left", pad = "0")
                                        county_boundaries$state_county_fips <- county_list$state_county_fips
                                        county_list <- filter(county_list, as.character(state_county_fips) %in% unique(datafile_map$state_county_fips))
                                        county_funding <- datafile_map %>% group_by(state_county_fips) %>% summarize(funding = sum(EDA.Funding), count = n())
                                        map_data <- left_join(county_list, county_funding, by = c("state_county_fips" = "state_county_fips"))
                                        map_data$state_county_fips <- factor(map_data$state_county_fips)
                                        map_boundaries <- subset(county_boundaries, county_boundaries$state_county_fips %in% unique(map_data$state_county_fips))
                                        map_boundaries$funding <- map_data$funding
                                        map_boundaries$count <- map_data$count
                                        
                                        default_popup <- str_c(str_c("County: ", map_boundaries$NAME),
                                                               str_c("EDA funds requested: $", prettyNum(map_boundaries$funding, big.mark = ",", scientific = FALSE),  sep = ""),
                                                               str_c("Count of applications: ", prettyNum(map_boundaries$count, big.mark = ",", scientific = FALSE)),
                                                               sep = "<br/>")
                                }
                                        
                                funding <- map_data$funding
                                funding_values <- map_data$funding
                                funding_values_placeholder$value <- funding_values
                                
                                map_boundaries_fund_pal <- colorNumeric(
                                        palette = "Blues",
                                        domain = map_boundaries$funding
                                )
                                
                                leafletProxy("map_boundaries", data = map_boundaries) %>% addTiles() %>%
                                        clearShapes() %>%
                                        addPolygons(
                                                stroke = TRUE, color = "black", weight = "1", fillOpacity = 0.75, smoothFactor = 0.5,
                                                fillColor = ~ map_boundaries_fund_pal(map_boundaries$funding),
                                                popup = default_popup) %>%
                                        clearControls() %>%
                                        addLegend("bottomright", pal = map_boundaries_fund_pal, values = funding_values,
                                                  title = "Requested EDA Funding", opacity = 1, labFormat = labelFormat(prefix = "$"))
                        }
                }
        })
        
        # create reactiveValues variable, which can be updated from observers
        # this is placeholder for selected_values() with icon map 
        # this prevents error, since observeEvent for display legend needs it, but its not defined until map refresh button is hit
        selected_values_placeholder <- reactiveValues(
                value = datafile$Program
        )
        
        # create reactiveValue for geographic boundary map funding legend
        funding_values_placeholder <- reactiveValues(
                value = datafile$EDA.Funding
        )
        
        # checkbox to display legend or not on award map
        observeEvent(input$display_legend_applications, {
                selected_pal <- selected_pal()
                selected_title <- selected_title()
                selected_values <- selected_values_placeholder$value
                selected_size <- selected_size()
                selected_format <- selected_format()
                
                if(input$display_legend_applications == TRUE && input$refresh_map > 0){
                        leafletProxy("map") %>%
                        addLegend("bottomright", pal = selected_pal, values = selected_values,
                        title = selected_title, opacity = 1, labFormat = labelFormat(prefix = selected_format))
                }
                if(input$display_legend_applications == FALSE && input$refresh_map > 0){
                        leafletProxy("map") %>%
                                clearControls()
                }
        })
        
        # toggle legend checkbox for geographic boundaries map
        observeEvent(input$display_legend_geography, {
                map_boundaries_fund_pal <- colorNumeric(
                        palette = "Blues",
                        domain = funding_values_placeholder$value
                )

                if(input$display_legend_geography == TRUE && input$refresh_map > 0){
                        leafletProxy("map_boundaries") %>%
                                addLegend("bottomright", pal = map_boundaries_fund_pal,
                                          values = funding_values_placeholder$value,
                                          title = "Requested EDA Funding", opacity = 1,
                                          labFormat = labelFormat(prefix = "$"))
                }
                if(input$display_legend_geography == FALSE && input$refresh_map > 0){
                        leafletProxy("map_boundaries") %>%
                                clearControls()
                }
        })
        
        # create reactive fund_pal seperately to avoid timeout/race conditions
        # this palette colors map icons by funding level
        fund_pal <- reactive({
                data_table5_filtered <- data_table5_filtered()

                # only run if at least one row of data is selected
                if(data_table5_filtered[1,1] != "no projects"){

                        # create funds palette
                        colorNumeric(
                                palette = "Blues",
                                domain = data_table5_filtered$EDA.Funding
                        )
                }
        })
        
        # create reactive selected_pal
        # this variable holds the palette for the user-selected feature by which to color map icons
        selected_pal <- reactive({
                fund_pal <- fund_pal()

                # select legend palette
                selected_pal <- program_pal
                if(input$marker_type == "By appropriation"){
                        selected_pal <- appropriation_pal
                }
                if(input$marker_type == "By program"){
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
                if(input$marker_type == "By appropriation"){
                        selected_title <- "EDA Appropriation"
                }
                if(input$marker_type == "By program"){
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
        
        # create pivot table
        output$pivot_table <- renderRpivotTable({
                data_table5_filtered <- data_table5_filtered()
                data_table5_filtered <- select(data_table5_filtered, FY, Status, Program, Initiatives, 
                                       EDA.Funding, Total.Proj.Cost, Region.Name, Appl.State.Abbr, Appl.County.Name,
                                       Appl.City.Name, Proj.State.Abbr, Proj.County.Name, Proj.City.Name,  
                                       Appl.Short.Name)
                rpivotTable(data_table5_filtered)
        })
        
        output$rows_all <- renderPrint({
                # uploaded_query <- uploaded_query()
                # uploaded_query$global_search_keywords
                # query_term_placeholder$value
                # date1
        })
        
        # create download file
        download_file <- reactive({
                data_table5_filtered <- data_table5_filtered()
                
                if(input$download_columns == TRUE){
                        return(data_table5_filtered)
                }
                if(input$download_columns == FALSE){
                        return(data_table5_filtered[ , input$column_input])
                }
        })
        
        # download data
        output$downloadData <- downloadHandler(
                filename = function() {
                        str_c("datafile_", date, ".csv") 
                },
                content = function(file) {
                        write.csv(download_file(), file, row.names = FALSE)
                }
        )
        
        # create title for saved query download
        download_query_title <- reactiveValues(
                value = str_c("query_", date)
        )
        
        # create observer to update title for saved query download
        observe({
                download_query_title$value <- input$download_query_title
        })
        
        # download saved query
        output$download_query <- downloadHandler(
                # download_query_title <- input$download_query_title,
                filename = function() {
                        str_c(download_query_title$value) 
                },
                content = function(file) {
                        write(saved_query(), file)
                }
        )
}

)