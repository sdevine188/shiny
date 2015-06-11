# ui.R

shinyUI(fluidPage(
        titlePanel(h1("My Shiny App")),
        sidebarLayout(
                sidebarPanel(
                        h2("Installation"),
                        br(),
                        p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
                        code('install.packages("shiny")'),
                        br(),
                        br(),
                        br(),
                        img(src = "bigorb.png", height = 72, width = 72, "shiny is a product of ", 
                            span("R Studio", style = "color:blue"))
                        ),
                mainPanel(
                        h2("Introducing Shiny"),
                        p("Shiny is a new package from RStudio that makes it",
                          em("incredibly easy"), "to build interactive web applications with R."),
                        br(),
                        h3("Features"),
                        p("* Build useful web applications with only a few lines of code - no JavaScript required."),
                        p("* Shiny applications are automatically 'live' in the same way that", strong("spreadsheets"), "are live.
                                        Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
                        )
                )
        ))