library(shiny)
library(RCurl)
library(XML)
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(datasets)
library(maps)
library(scales)
library(RColorBrewer)
library(mapproj)

# load data
choropleth <- read.csv("data/choropleth.csv")
state_map <- read.csv("data/state_map.csv")

colors <- brewer.pal(9, "Blues")
pal <- colorRampPalette(colors)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        output$choropleth <- renderPlot({ 
                
                # assign default placeholder inputs 
                choropleth_input <- choropleth
                state_map_input <- state_map
                
                # keep the entire choropleth_input dataframe if "All States" is selected
                # or subset choropleth_input to the states selected
                if("all" %in% input$state){
                        choropleth_input <- choropleth_input
                        state_map_input <- state_map_input
                } else {
                        state_input <- input$state
                        choropleth_input <- filter(choropleth_input, region %in% state_input)
                        state_map_input <- filter(state_map_input, region %in% state_input)
                }
                
                # assign year variable and subset choropleth_input for year_input
                year_input <- input$year
                choropleth_input <- select(choropleth_input, 1:9, which(names(choropleth_input) == str_c("y", year_input)))
                
                # rename the year column "rade_d2"
                names(choropleth_input)[10] <- "rate_d2"
                
                # discretize year variables as ordered factors
                levels <- c("0% - 2%", "2% - 4%", "4% - 6%", "6% - 8%", "8% - 10%", "10% - 12%", "12% - 14%", "14% - 16%",
                            "16% - 18%", "18% - 20%", "20% - 22%", "22% - 24%", "24% - 26%", "26% - 28%", "28% - 30%", "30% - 32%",
                            "32% - 34%", "34% - 36%", "36% - 38%", "38% - 40%", "40% - 42%")
                choropleth_input$rate_d2 <- factor(choropleth_input$rate_d2, levels = levels, ordered = TRUE)
                
                # re-order based on the order variable to ensure the polygons plot correctly
                choropleth_input <- choropleth_input[order(choropleth_input$order ), ]
               
                # plot choropleth
                ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
                        geom_polygon(aes(fill = rate_d2), colour = alpha("white", 1/2), size = 0.2) + 
                        geom_polygon(data = state_map_input, colour = "black", fill = NA) +
                        scale_fill_manual(values = pal(length(unique(choropleth_input$rate_d2)))) + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
                        axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title = element_text(size=20, face = "bold")) + 
                        labs(x = "", y = "", title = str_c("Unemployment Rate in ", year_input), fill = "Unemployment Rate") + coord_fixed() + coord_map(project = "conic", lat0 = 30)
                })
        }
)