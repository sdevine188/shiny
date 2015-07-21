library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        # Application title
        titlePanel("censusVis"),
        
        # Sidebar with a slider input for the number of bins
        sidebarLayout(
                
                sidebarPanel(
                        helpText("Create demographic maps with 
                                 information from the 2010 U.S.
                                 Census."),
                        
                        selectInput("var", label = "Choose a 
                                variable to display", choices =
                                list("Percent White" = 1, "Percent
                                Black" = 2, "Percent Hispanic" = 3,
                                "Percent Asian" = 4), selected = 1),
                        
                        sliderInput("range", label = "Range of 
                                    interest", min = 0, max = 100,
                                    value = c(0, 100))
                ),
                
                mainPanel(
                        textOutput("text1"))
        )
))
                                                 