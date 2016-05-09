library(shiny)
library(dplyr)
library(stringr)
library(leaflet)
library(DT)
library(shinythemes)

shinyUI(
        navbarPage("", id = "navbar",
           tabPanel("View data",
                    fluidPage(theme = shinytheme("cerulean"),
                            fluidRow(
                                    column(3,
                                           img(src = "eda_logo.jpg", height = 150, width = 150)
                                    ),
                                    column(6, 
                                           h2(strong("Grants Viewer"), align = "center", style = "font-size:30pt"),
                                           h2("EDA Performance and National Programs", align = "center",
                                              style = "font-size:20pt")
                                           # h2("EDA Performance and National Programs", align = "center",
                                           #    style = "font-family: 'Open Sans', sans-serif"),
                                           # h2("EDA Performance and National Programs", align = "center"),
                                    ),
                                    column(3,                       
                                           textOutput("as_of_date")
                                    )
                            ),
                            fluidRow(column(12,
                                            br())
                            ),
                            fluidRow(
                                    column(3,                       
                                           selectInput("state", "Select a state", choices = "", multiple = TRUE),
                                           selectInput("counties", "Select a county:", choices = "", multiple = TRUE)                    
                                    ),
                                    column(3, 
                                           radioButtons("project_applicant_radio", "Select state/county based on:",
                                                        choices = c("Project state/county", "Applicant state/county", "Either project or applicant state/county"),
                                                        selected = "Project state/county")
                                    ),
                                    column(3, 
                                           sliderInput("years", label = "Select fiscal year(s)", min = 1990, max = 2016, value = c(1990, 2016), sep = "")                     
                                    )
                            ),
                            fluidRow(
                                    # column(6,
                                    #        checkboxInput("JobsPIFlag", "Include grantee estimates for jobs and private investment", value = FALSE),
                                    #        conditionalPanel(
                                    #                condition = "input.JobsPIFlag == true",
                                    #                helpText("Please note that checking the box to request projects containing grantee estimates for jobs and private investment 
                                    #                         will cause the dataset to be truncated."), 
                                    #                helpText("The truncated dataset will only include construction-related projects made under 
                                    #                         the Public Works or Economic Adjustment Assistance programs, since these are the only projects 
                                    #                         containing grantee estimates for jobs and private investment.")
                                    #                )
                                    #     ),
                                        column(3,
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
                                           DT::dataTableOutput("table")
                                    )
                            )
                            )
                    ),
           
           tabPanel("Advanced query",
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
                                                   selectInput("program_input", "Select EDA programs to display", choices = 
                                                                       c("All programs", "Public Works", "Planning", "Econ Adjst", "Tech Asst", "Trade Adjst", "Disaster Supp",
                                                                         "GCCMIF", "Research", "CTAA"), multiple = TRUE, selected = "All programs"),
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
           
           tabPanel("View map",
                    fluidPage( 
                            fluidRow(
                                    column(3,
                                           img(src = "eda_logo.jpg", height = 150, width = 150)
                                        ),
                                    column(3,
                                           selectInput("marker_type", "Select color-coding of project icons:", 
                                                       choices = c("By appropriation", "By program", "By fiscal year awarded", 
                                                                   "By EDA funding level"), selected = "By appropriation"),
                                           checkboxInput("display_legend", "Display map legend", value = TRUE)
                                        ),
                                    column(3,
                                           selectInput("circle_size", "Select size of project icons:", choices = c("Small circles", 
                                                                                                                   "Large circles"), selected = "Large circles"),
                                           actionLink("icon_link", "Note on project mapping"),
                                           conditionalPanel(
                                                   condition = "input.icon_link == true",
                                                   helpText("Please note that projects are mapped using the lead applicant's address that is entered into OPCS.
                                                            For an applicant with a P.O. Box address, the project will be mapped in the center of the 
                                                            applicant's zip code.")
                                                   )
                                           ),
                                    column(3,
                                           actionButton("refresh_map", "Refresh map")
                                           )
                                   ),
                            fluidRow(column(12,
                                            br())
                            ),
                            fluidRow(
                                    column(12, align = "center",
                                           # textOutput("rows_all"),
                                           leafletOutput("map", width = 1200, height = 700)
                                    )
                            ),
                            fluidRow(
                                    column(12, 
                                           br(),
                                           br(),
                                           br())
                                     )
                        )
                    )
           ))