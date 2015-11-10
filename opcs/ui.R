library(shiny)
library(dplyr)
library(stringr)
library(leaflet)
library(DT)

shinyUI(navbarPage("", id = "navbar",
                   tabPanel("View data",
                            
                            
                            fluidPage(
                                    fluidRow(
                                            column(3,
                                                   img(src = "eda_logo.jpg", height = 150, width = 150)
                                            ),
                                            column(6, 
                                                   titlePanel("EDA Project Data")
                                            ),
                                            column(3,                       
                                                   textOutput("as_of_date")
                                            )
                                    ),
                                    
                                    fluidRow(column(12,
                                                    br())
                                    ),
                                    
                                    fluidRow(
#                                             
# #                                             column(3,                       
# #                                                    uiOutput("state"),
# #                                                    selectInput("counties", "Select a county:", choices = "", multiple = TRUE)                    
# #                                             ),
#                                             column(3, 
#                                                    sliderInput("years", label = "Select fiscal year(s)", min = 1990, max = 2015, value = c(2014, 2015), sep = "")                     
#                                             ),
                                            column(12, offset = 1, 
                                                   downloadButton('downloadData', 'Download Data')
                                            )
                                    ),
                                    
                                    fluidRow(
                                            column(3, 
                                                   radioButtons("datafile_radio", "Select data", choices = c("FY 2012 to FY 2016",
                                                                "FY 1995 to FY 2016"), selected = "FY 2012 to FY 2016")
                                                   ),
#                                             column(3,
#                                                    actionButton("radio_submit", "Load data")
#                                                    ),
                                            column(6,
                                                   checkboxInput("JobsPIFlag", "Include grantee estimates for jobs and private investment", value = FALSE),
                                                   conditionalPanel(
                                                           condition = "input.JobsPIFlag == true",
                                                           helpText("Please note that checking the box to request projects containing grantee estimates for jobs and private investment 
                                                                    will cause the dataset to be truncated."), 
                                                           helpText("The truncated dataset will only include construction-related projects made under 
                                                                    the Public Works or Economic Adjustment Assistance programs, since these are the only projects 
                                                                    containing grantee estimates for jobs and private investment.")
                                                           )
                                                )
                                   ),
                                    
                                    fluidRow(
                                            column(12,
                                                   textOutput("rows_all"),   
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
#                                                                                   selectInput("text_var1_input", "Choose a text variable to query", 
#                                                                                               choices = ""),
                                                                                  selectizeInput(
                                                                                          'text_var1_input', 'Choose a text variable to query', choices = "",
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
                                                                  textOutput("query_term_output")
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
                                                               choices = c("By program type", "By fiscal year awarded", 
                                                                           "By EDA funding level"), selected = "By program type")
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
                                            column(12,
                                                   leafletOutput("map")
                                            )
                                    )
                                            )
                            )
                   ))