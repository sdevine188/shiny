list.of.packages <- c("shiny", "dplyr", "leaflet", "stringr", "DT", "rjson", "readr", "lubridate", "shinythemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0){
        install.packages(new.packages)
}

library(shiny)
library(dplyr)
library(stringr)
library(leaflet)
library(DT)
library(shinythemes)
library(rpivotTable)

shinyUI(
        navbarPage("", id = "navbar",
           tabPanel("Data",
                    fluidPage(theme = shinytheme("cerulean"),
                            # create logo, title, and date
                              fluidRow(
                                    column(3,
                                           img(src = "eda_logo.jpg", height = 150, width = 150)
                                    ),
                                    column(6, 
                                           h2(strong("Grants Viewer"), align = "center", style = "font-size:30pt"),
                                           h2("EDA Performance and National Programs", align = "center",
                                              style = "font-size:20pt")
                                    ),
                                    column(3,                       
                                           textOutput("as_of_date")
                                    )
                            ),
                            fluidRow(column(12,
                                            br())
                            ),
                            # create state and year drop down menus
                            fluidRow(
                                    column(3,                       
                                           selectInput("state", "Select project states", choices = "", multiple = TRUE)
                                    ),
                                    column(6, align = "center",
                                           sliderInput("years", label = "Select fiscal years", min = 1990, max = 2016, 
                                                       value = c(1990, 2016), width = "100%", sep = "")
                                    ),
                                    column(3, align = "center",
                                           downloadButton("downloadData", "Download Data")
                                    )
                            ),
                           fluidRow(
                                   column(12, 
                                          br()
                                   )
                           ),
                            fluidRow(
                                    column(12,
                                           # textOutput("rows_all"),
                                           verbatimTextOutput("rows_all"),
                                           DT::dataTableOutput("table")
                                    )
                            )
                            )
                    ),
           
           tabPanel("Query",
                    fluidPage( 
                            fluidRow(
                                    column(3,
                                           img(src = "eda_logo.jpg", height = 150, width = 150)
                                    ),
                                    column(3, 
                                           # textOutput("rows_all"),
                                           actionButton("submit_query", "Submit query")
                                    ),
                                    column(3, 
                                           actionButton("reset_all", "Reset all options to defaults")
                                    )
                            ),
                            fluidRow(
                                    column(3, offset = 3,
                                          textInput("download_query_title", "Query Title", value = "query_title"),
                                          downloadButton('download_query', 'Save query')
                                          ),
                                   column(3,
                                          fileInput("file_input", "Upload saved query",
                                                    accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv", 
                                                             ".json"))
                                   ),
                                   column(3,
                                          radioButtons("saved_query_radio", label = NULL,
                                                       choices = c("Apply uploaded query", "Do not apply uploaded query"),
                                                       selected = "Do not apply uploaded query"))
                            ),
                            fluidRow(
                                    column(12, 
                                           br()
                                    )
                            ),
                            fluidRow(
                                    column(6,
                                           wellPanel(
                                                   selectInput("column_input", label = "Select columns to display:", choices = "", 
                                                               multiple = TRUE),
                                                   actionButton("reset_columns", "Reset to default columns"),
                                                   checkboxInput("download_columns", "Truncate data download to include only displayed columns", value = FALSE)
                                           )
                                    ),
                                    
                                    column(6, 
                                           wellPanel(
                                                   # selectInput("program_input", "Select EDA programs to display", choices = 
                                                   #                     c("All programs", "Public Works", "Planning", "Econ Adjst", "Tech Asst", "Trade Adjst", "Disaster Supp",
                                                   #                       "GCCMIF", "Research", "CTAA"), multiple = TRUE, selected = "All programs"),
                                                   selectInput("program_input", "Select EDA programs to display", choices = "",
                                                                       multiple = TRUE, selected = "All programs"),
                                                   actionButton("reset_programs", "Reset to all programs")
                                           )
                                    )
                            ),
                            fluidRow(
                                    column(6,
                                           wellPanel(
                                                   selectInput("initiatives_input", "Select initiative codes to display", 
                                                               choices = "", multiple = TRUE),
                                                   actionButton("reset_initiatives", "Reset to all initiatives")
                                           )
                                    )
                            ),
                            fluidRow(
                                    column(12,
                                           fluidRow(
                                                   column(6,
                                                          wellPanel(
                                                                  flowLayout(
                                                                          selectizeInput(
                                                                                  'text_var1_input', 'Choose a variable to query', choices = "",
                                                                                  options = list(
                                                                                          onInitialize = I('function() { this.setValue(""); }')
                                                                                  )
                                                                          ),
                                                                          textInput("text_term1_input", "Create a query term")
                                                                  ),
                                                                  actionButton("enter_query_term", "Enter query term"),
                                                                  br(),
                                                                  br(),
                                                                  actionButton("reset_text_query", "Clear text query")
                                                          )
                                                   ),
                                                   column(6,
                                                          wellPanel(
                                                                textOutput("query_term_output")
                                                          )
                                                   )
                                           )
                                    )
                            )
                    )
           ),
           
           tabPanel("Maps",
                    fluidPage( 
                            fluidRow(
                                    column(3,
                                           img(src = "eda_logo.jpg", height = 150, width = 150)
                                        ),
                                    column(3,
                                           radioButtons("map_radio", "Select type of map to display:",
                                                        choices = c("Map with application icons", "Map with geographic boundaries"),
                                                        selected = "Map with application icons")
                                        ),
                                    column(3,
                                           actionButton("refresh_map", "Refresh map")
                                    )
                                   ),
                            fluidRow(column(12, align = "center",
                                    conditionalPanel(
                                            condition = "input.map_radio == 'Map with application icons'",
                                            
                                            column(3, offset = 3, align = "center",
                                                  selectInput("marker_type", "Select color-coding of award icons:", 
                                                                choices = c("By appropriation", "By program", "By fiscal year awarded", 
                                                              "By EDA funding level"), selected = "By appropriation")
                                                ),
                                            column(3, align = "center",
                                                   selectInput("circle_size", "Select size of award icons:", choices = c("Small circles", 
                                                                                        "Large circles"), selected = "Large circles")
                                                   ),
                                            column(3, align = "center",
                                                   checkboxInput("display_legend_applications", "Display map legend", value = TRUE)
                                                   ),
                                            column(12, 
                                                   br()),
                                            column(12, align = "center",
                                                   leafletOutput("map", width = 1200, height = 700)
                                                   )
                                        )
                                )),
                            fluidRow(column(12, align = "center",
                                   conditionalPanel(
                                           condition = "input.map_radio == 'Map with geographic boundaries'",
                                           
                                           column(3, offset = 3, align = "center",
                                                  selectInput("map_geography", "Select geographic boundaries to display", choices = 
                                                        c("State", "Congressional District", "County"), multiple = FALSE, selected = "State")
                                                  ),
                                           column(3, align = "center",
                                                  checkboxInput("display_legend_geography", "Display map legend", value = TRUE)
                                                  ),
                                           column(12, align = "center",
                                                  leafletOutput("map_boundaries", width = 1200, height = 700)
                                                  )
                                   )
                            )
                            ),
                            fluidRow(
                                    column(12, 
                                           br(),
                                           br(),
                                           br())
                                     )
                        )
                    ),
           
           tabPanel("Pivot Table",
                    fluidPage( 
                            fluidRow(
                                    # column(3,
                                    #        img(src = "eda_logo.jpg", height = 150, width = 150)
                                    # ),
                                    column(12, align = "center",
                                           rpivotTableOutput("pivot_table")
                                           )
                            )
                    )
           )
))