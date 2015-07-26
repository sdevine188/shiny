library(shiny)

# load data
choropleth_input <- read.csv("data/choropleth.csv")
state_map <- read.csv("data/state_map.csv")

shinyUI(fluidPage(
        titlePanel("U.S. County Unemployment Rate Choropleth Tool"),
        
        sidebarLayout(
                sidebarPanel(
                        helpText("Create a choropleth map of county unemployment rates by U.S. state*"),
                        
                        selectInput("state", label = h3("Select State(s)"), 
                                           choices = list("All States" = "all", "Alabama" = "alabama", "Arizona" = "arizona", 
                                        "Arkansas" = "arkansas", "California" = "california", "Colorado" = "colorado",
                                        "Connecticut" = "connecticut", "Delaware" = "delaware", "Florida" = "florida",
                                        "Georgia" = "georgia", "Idaho" = "idaho", "Illinois" = "illinois", 
                                        "Indiana" = "indiana", "Iowa" = "iowa", "Kansas" = "kansas", "Kentucky" = "kentucky",
                                        "Louisiana" = "louisiana", "Maine" = "maine", "Maryland" = "maryland", 
                                        "Massachusetts" = "massachusetts", "Michigan" = "michigan", "Minnesota" = "minnesota",
                                        "Mississippi" = "mississippi", "Missouri" = "missouri", "Montana" = "montana",
                                        "Nebraska" = "nebraska", "Nevada" = "nevada", "New Hampshire" = "new hampshire",
                                        "New Jersey" = "new jersey", "New Mexico" = "new mexico", "New York" = "new york",
                                        "North Carolina" = "north carolina", "North Dakota" = "north dakota", "Ohio" = "ohio",
                                        "Oklahoma" = "oklahoma", "Oregon" = "oregon", "Pennsylvania" = "pennsylvania", 
                                        "Rhode Island" = "rhode island", "South Carolina" = "south carolina", 
                                        "South Dakota" = "south dakota", "Tennessee" = "tennessee", "Texas" = "texas",
                                        "Utah" = "utah", "Vermont" = "vermont", "Virginia" = "virginia", 
                                        "Washington" = "washington", "West Virginia" = "west virginia", 
                                        "Wisconsin" = "wisconsin", "Wyoming" = "wyoming"), selected = "all", 
                                        multiple = TRUE),
                        
                        selectInput("year", label = h3("Select Year"), 
                                    choices = list("2014" = 2014, "2013" = 2013, "2012" = 2012, 
                                                   "2011" = 2011, "2010" = 2010, "2009" = 2009, "2008" = 2008, 
                                                   "2007" = 2007, "2006" = 2006, "2005" = 2005, "2004" = 2004,
                                                   "2003" = 2003, "2002" = 2002, "2001" = 2001, "2000" = 2000,
                                                   "1999" = 1999, "1998" = 1998, "1997" = 1997, "1996" = 1996,
                                                   "1995" = 1995,  "1994" = 1994, "1992" = 1993, "1991" = 1991,
                                                   "1990" = 1990), selected = 2014),
                        
                        br(),
                        
                        helpText("*Note that Alaska and Hawaii have been excluded for efficient plotting")
                ),
                
                mainPanel(
                        tabsetPanel(
                                tabPanel("Choropleth", plotOutput("choropleth")),
                                tabPanel("Readme", 
                                         h2("Readme"),
                                         br(),
                                         h4("This app lets you create choropleths depicting the unemployment
                                        rate in U.S. counties.  To use the app, select one or more states from the 
                                        \"Select State\" dropdown menu, then select a year from the 
                                        \"Select Year\" dropdown menu.  The app will automatically update to 
                                        reflect your choice of state(s) and year.")
                                        )
                                )
                        )
                )
        )
)