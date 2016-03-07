library(shiny)
library(dplyr)
library(datasets)
library(leaflet)
library(stringr)
library(DT)
library(rjson)
library(readr)

# provide data "as of date"
date <- "20160209"

# Read in data file
# read small data to start map without delay
# datafile_small <- read.csv("data/small_data_utf8.csv", stringsAsFactors = FALSE)
# datafile_small <- read_csv("data/small_data_utf8.csv")
# datafile_small <- read.csv("data/shiny_data_fake.csv", stringsAsFactors = FALSE)
datafile_small <- read_csv("data/shiny_app_data_small_20160209.csv")
datafile <- read_csv("data/shiny_app_data_20160209.csv")
# datafile <- read.csv("data/shiny_data_20151123.csv", stringsAsFactors = FALSE) 
# datafile <- read_csv("data/shiny_data_20151123.csv") 
# datafile <- read.csv("data/shiny_data_fake.csv", stringsAsFactors = FALSE)

# convert Control. to a character, so it can be searched with table filter, instead of numeric slider
# can place this code in clean_shiny_data script later
datafile$Control. <- as.character(datafile$Control.)

# create default columns to display
default_columns <- c("Control.", "Status", "FY", "Appr.Desc", "Best.EDA..", "Appl.Short.Name", 
                     "Project.Short.Descrip", "Appl.City.Name", "Proj.ST.Abbr")

# create map color palettes
program_options <- unique(datafile$Appr.Desc)
appropriation_options <- unique(datafile$Appropriation)
year_options <- factor(seq(1995, 2016))

program_pal <- colorFactor("Set1", domain = program_options)
appropriation_pal <- colorFactor("Set1", domain = appropriation_options)
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

# create state list for select state input
# note there are six records of 25k master data with "" for appl state, 
# these will only show when all states is selected bc we drop them so there is no "" showing in select state menu
state_arr <- arrange(datafile, Appl.State.Abbr)
state_list <- unique(state_arr$Appl.State.Abbr)
state_list <- state_list[-1]

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
        
        # create variable for selected_states
        state_data <- reactive({
                # create placeholder selected_states variable to assign in if statements
                selected_states <- c()                
                
                if("All states" %in% input$state && input$project_applicant_radio == "Project state/county"){
                        selected_states <- unique(datafile$Proj.ST.Abbr)
                }
                if("All states" %in% input$state && input$project_applicant_radio == "Applicant state/county"){
                        selected_states <- unique(datafile$Appl.State.Abbr)
                }
                if("All states" %in% input$state && input$project_applicant_radio == "Either project or applicant state/county"){
                        selected_states_proj <- unique(datafile$Proj.ST.Abbr)
                        selected_states_app <- unique(datafile$Appl.State.Abbr)
                        selected_states_combined <- c(selected_states_proj, selected_states_app)
                        selected_states <- unique(selected_states_combined)
                }
                if(!("All states" %in% input$state)){
                        selected_states <- input$state
                }

                if(input$project_applicant_radio == "Project state/county"){
                        return(filter(datafile, Proj.ST.Abbr %in% selected_states))        
                }
                if(input$project_applicant_radio == "Applicant state/county"){
                        return(filter(datafile, Appl.State.Abbr %in% selected_states))        
                }
                if(input$project_applicant_radio == "Either project or applicant state/county"){
                        return(filter(datafile, Proj.ST.Abbr %in% selected_states | Appl.State.Abbr %in% selected_states))        
                }
        })
        
        # counties is a reactive variable that identifies the counties assosciated with the user's selection from the state input menu
        counties <- reactive({
                state_data <- state_data()

                if(input$project_applicant_radio == "Project state/county"){
                        state_counties <- arrange(state_data, Proj.County.Name)
                        return(unique(state_counties$Proj.County.Name)) 
                }
                if(input$project_applicant_radio == "Applicant state/county"){
                        state_counties <- arrange(state_data, Appl.Cnty.Name)
                        return(unique(state_counties$Appl.Cnty.Name)) 
                }
                if(input$project_applicant_radio == "Either project or applicant state/county"){
                        state_counties_proj <- state_data$Proj.County.Name
                        state_counties_app <- state_data$Appl.Cnty.Name
                        state_counties_combined <- c(state_counties_proj, state_counties_app)
                        state_counties <- sort(state_counties_combined)
                        return(unique(state_counties)) 
                }
        })
        
        # populate the select state input menu
        observe({
                states_all <- c("All states", state_list)
                updateSelectInput(session, "state", choices = states_all, selected = states_all[1])
        })
        
        # populate the select county input menu
        observe({
                counties_all <- c("All counties", as.character(counties()))
                updateSelectInput(session, "counties", choices = counties_all, selected = counties_all[1])
        })
        
        # filter data to only those states/counties selected
        data_table1 <- reactive({
                # assign previously created reactive variables to regular variables
                state_data <- state_data()
                counties <- counties()
                datafile1 <- data.frame()
                
                # create if statements to handle "All counties" option in dropdown menu
                if("All counties" %in% input$counties && input$project_applicant_radio == "Project state/county"){
                        data_table1 <- state_data                        
                }
                if("All counties" %in% input$counties && input$project_applicant_radio == "Applicant state/county"){
                        data_table1 <- state_data                    
                }
                if("All counties" %in% input$counties && input$project_applicant_radio == "Either project or applicant state/county"){
                        data_table1 <- state_data                    
                }
                if(!("All counties" %in% input$counties) && (input$project_applicant_radio == "Project state/county")){
                        data_table1 <- filter(state_data, Proj.County.Name %in% input$counties)                        
                }
                if(!("All counties" %in% input$counties) && (input$project_applicant_radio == "Applicant state/county")){
                        data_table1 <- filter(state_data, Appl.Cnty.Name %in% input$counties)                        
                }
                if(!("All counties" %in% input$counties) && (input$project_applicant_radio == "Either project or applicant state/county")){
                        data_table1 <- filter(state_data, Proj.County.Name %in% input$counties | Appl.Cnty.Name %in% input$counties)                        
                }
                
                data_table1
        })
        
        # filter data based on year input
        data_table2 <- reactive({
                data_table1 <- data_table1()
                range <- seq(input$years[1], input$years[2])
                data_table2 <- filter(data_table1, FY %in% range)
                data_table2
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
        
        # filter data based on advanced query inputs
        data_table4 <- reactive({
                data_table3 <- data_table3()
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

                        # create if statements to handle query term
                        if(query_term_placeholder$value == ""){
                                data_table4 <- data_table4
                        }
                        if(query_term_placeholder$value != ""){
                                data_table_placeholder <- data_table4        
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
                                                new_value <- str_replace_all(new_value, "' \\| '", "|")
                                                term_value[i] <- new_value
                                        }
                                }
                                
                                # filter datatable based on query term
                                for(i in 1:length(term_value)){
                                        data_table_placeholder <- filter(data_table_placeholder, 
                                                                         grepl(term_value[i], data_table_placeholder[ , term_var[i]]))
                                }
                                data_table4 <- data_table_placeholder
                        }
                        data_table4
                })
        })
        
        # create variable storing the full query term submitted so it can be downloaded
        saved_query <- reactive({
             program_input <- input$program_input
             initiatives_input <- input$initiatives_input
             query_term <- query_term_placeholder$value
             column_input <- input$column_input
             download_columns <- input$download_columns
             counties <- input$counties
             state <- input$state
             years <- input$years
             saved_query_list <- list(program_input, initiatives_input, query_term, column_input, download_columns, counties, 
                                      state, years)
             names(saved_query_list) <- c("program_input", "initiatives_input", "query_term", "column_input", "download_columns",
                                          "counties", "state", "years")
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
                                counties_all <- c("All counties", as.character(counties()))
                                updateSelectInput(session, "counties", choices = counties_all, selected = uploaded_query$counties)
                                states_all <- c("All states", state_list)
                                updateSelectInput(session, "state", choices = states_all, selected = uploaded_query$state)
                                updateSliderInput(session, "years", value = c(uploaded_query$years[1], uploaded_query$years[2]))
                        }
                })
        })
        
        
        # create variable data_table with latest selected data from advanced query, if necessary
        data_table <- reactive({
                data_table <- data.frame()
                data_table3 <- data_table3()
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
#                         return(datatable(data_table_output2, filter = "none", rownames = FALSE, options = list(pageLength = 5)))
                        return(DT::datatable(data_table_output2, filter = "none", rownames = FALSE, options = list(pageLength = 5)))
                }
                if(nrow(data_table_output) >= 1){
                        data_table_output2 <- data_table_output
#                         return(datatable(data_table_output2, filter = "top", options = list(pageLength = 5)))
                        return(DT::datatable(data_table_output2, filter = "top", options = list(pageLength = 5)))
                }
        },
                server = TRUE
        )
        
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
                
                data_table5_filtered <- select(data_table5_filtered, Appl.Short.Name, app_address, FY, Appr.Desc, Appropriation, Best.EDA.., app_lat, app_lon)
                data_table5_filtered <- na.omit(data_table5_filtered)
                
                # only run if at least one row of data is selected
                if(data_table5_filtered[1,1] != "no projects"){
                
                default_popup <- str_c(str_c("Applicant name:", data_table5_filtered$Appl.Short.Name, sep = " "), 
                                       str_c("Applicant Address:", data_table5_filtered$app_address, sep = " "),
                                       str_c("Fiscal year: FY", data_table5_filtered$FY, sep = " "),
                                       str_c("Program:", data_table5_filtered$Appr.Desc, sep = " "), 
                                       str_c("Appropriation:", data_table5_filtered$Appropriation, sep = " "), 
                                       str_c("EDA funds: $", prettyNum(data_table5_filtered$Best.EDA.., big.mark = ",", 
                                                                       scientific = FALSE),  sep = ""), 
                                       sep = "<br/>")
                
                selected_pal <- selected_pal()
                selected_title <- selected_title()
                selected_values <- selected_values()
                selected_size <- selected_size()
                selected_format <- selected_format()
                
                leafletProxy("map", data = data_table5_filtered) %>%
                        clearMarkers() %>%
                        addCircleMarkers(data = data_table5_filtered, lng = ~app_lon, lat = ~app_lat, popup = default_popup,
                                         color = ~selected_pal(selected_values), opacity = 1, radius = selected_size,
                                         fillColor = ~selected_pal(selected_values), fillOpacity = .2) %>%
                        clearControls() %>%
                        addLegend("bottomright", pal = selected_pal, values = selected_values,
                                  title = selected_title, opacity = 1, labFormat = labelFormat(prefix = selected_format))
                        
                }
        })
        
        # checkbox to display legend or not
        observeEvent(input$display_legend, {
                selected_pal <- selected_pal()
                selected_title <- selected_title()
                selected_values <- selected_values()
                selected_size <- selected_size()
                selected_format <- selected_format()
                if(input$display_legend == TRUE){
                        leafletProxy("map") %>%
                                addLegend("bottomright", pal = selected_pal, values = selected_values,
                                          title = selected_title, opacity = 1, labFormat = labelFormat(prefix = selected_format))
                }
                if(input$display_legend == FALSE){
                        leafletProxy("map") %>%
                                clearControls()
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
        
        # create reactive selected_values
        selected_values <- reactive({
                # select legend values
                data_table5_filtered <- data_table5_filtered()
                
                # only run if at least one row of data is selected
                if(data_table5_filtered[1,1] != "no projects"){
                        
#                         selected_values <- data_table5_filtered$Appropriation                        
                        selected_values <- data_table5_filtered$Appr.Desc
                        if(input$marker_type == "By program"){
                                selected_values <- data_table5_filtered$Appr.Desc
                        }
                        if(input$marker_type == "By appropriation"){
                                selected_values <- data_table5_filtered$Appropriation
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
                state_data <- state_data()
                dim(state_data)
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
        
        # download data
        output$downloadData <- downloadHandler(
                filename = function() {
                        str_c("datafile_", date, ".csv") 
                },
                content = function(file) {
                        write.csv(download_file(), file, row.names = FALSE)
                }
        )
        
        # download saved query
        output$download_query <- downloadHandler(
                filename = function() {
                        str_c("query_", date) 
                },
                content = function(file) {
                        write(saved_query(), file)
                }
        )
}

)