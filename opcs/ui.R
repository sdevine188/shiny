library(shiny)
library(dplyr)
library(stringr)
library(leaflet)
library(DT)

# provide data "as of date"
date <- "20150827"
as_of_date <- str_c("Data as of: ", date)

# define default_columns for data table to display
# default_columns <- c("Project.No.", "FY", "EDA.Program", "EDA.", "Appl.Short.Name", 
#                      "Project.Short.Descrip", "Project.Location", "Proj.ST.Abbr")

shinyUI(navbarPage("", id = "navbar",
# shinyUI(navbarPage("",
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
                                                   
                                                   p(as_of_date)
                                                   
                                            )
                                    ),
                                    
                                    fluidRow(column(12,
                                                    br())
                                    ),
                                    
                                    fluidRow(
                                            column(3,                       
                                                uiOutput("state"),
                                                selectInput("counties", "Select a county:", choices = "", multiple = TRUE)                    
                                            ),
                                            
                                            column(3, 
                                                   sliderInput("years", label = "Select fiscal year(s)", min = 1990, max = 2015, value = c(2014, 2015), sep = "")                     
                                            ),
                                            
                                            column(3, offset = 1, 
                                                   downloadButton('downloadData', 'Download Data')
                                            )
                                        ),
                                    
                                    fluidRow(
                                           column(7,
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
#                                                 textOutput("rows_all"),   
                                                DT::dataTableOutput("table")
                                            )
                                    )
                            )
),

tabPanel("Advanced query",
         fluidPage( 
                 fluidRow(
                         column(3,
                                selectInput("column_input", label = "Select columns to display:", choices = "", 
                                            multiple = TRUE),
                                actionButton("reset_columns", "Reset to default columns")
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