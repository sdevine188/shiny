library(shiny)
library(dplyr)
library(stringr)

# provide data "as of date"
date <- "20150827"
as_of_date <- str_c("Data as of: ", date)

# Read in data file
file <- str_c("data/datafile_", date, ".csv")
datafile <- read.csv(file, stringsAsFactors = FALSE)
datafile <- arrange(datafile, Proj.ST.Abbr)
state_choices <- c("All states", unique(datafile$Proj.ST.Abbr))

shinyUI(navbarPage("",
        tabPanel("Data table",
                
        fluidPage(
          
                fluidRow(
                        column(3,
                               img(src = "eda_logo.jpg", height = 150, width = 150)
                        ),
                        
                        column(6, 
                               titlePanel("EDA Project Data")
                               ),
                        
                        column(3,                       
                               helpText(as_of_date)
                               )
                ),
                
                fluidRow(column(12,
                                br())
                         ),
                
                fluidRow(
                        column(3,                       
                                selectInput("state", "Select a State:", 
                                           choices = state_choices, multiple = TRUE, selected = state_choices[1]),
                                                      
                                selectInput("counties", "Select a County:", choices = "", multiple = TRUE)                    
                        ),
                        
                        column(3, 
                               sliderInput("years", label = "Select fiscal year(s)", min = 1990, max = 2015, value = c(2014, 2015), sep = "")                     
                        ),
                               
                        column(3,
                               checkboxInput("JobsPIFlag", "Include grantee estimates for jobs and private investment", value = FALSE),
                               
                               conditionalPanel(
                                       condition = "input.JobsPIFlag == true",
                                       helpText("Please note that checking the box to request projects containing grantee estimates for jobs and private investment 
                                                            will cause the dataset to be truncated."), 
                                       helpText("The truncated dataset will only include construction-related projects made under 
                                                the Public Works or Economic Adjustment Assistance programs, since these are the only projects 
                                                containing grantee estimates for jobs and private investment.")
                               )
                         ),                    
                                    
                        column(3, 
                               downloadButton('downloadData', 'Download')
                        )
                ),
                
                fluidRow(
                        column(12,
                               htmlOutput("table")
                               )
                )
        )
        ),
        
        tabPanel("Map",
        fluidPage( 
                fluidRow(
                        column(12, 
                                helpText("Note that projects are mapped to pins using the lead applicant's address entered into OPCS.  For an applicant 
                                with a P.O. Box address, the project will be mapped to a pin located in the center of the applicant's zip code.")
                                )
                        ),
                        
                fluidRow(
                        column(12,
                               htmlOutput("map")
                                )
                        )
                )
        )
))
                                             
                        
                      
