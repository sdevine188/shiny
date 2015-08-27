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
library(Hmisc)

# import bls unemployment data from flat FTP:http://www.bls.gov/lau/lausad.htm#flat , http://download.bls.gov/pub/time.series/la/
# measures: 3-unemployment rate, 4-unemployment, 5-employment, 6-laborforce
bls <- getURL("http://download.bls.gov/pub/time.series/la/la.data.64.County")
write(bls, file = "unemployment/bls.csv")
bls2 <- read.table(file = "unemployment/bls.csv", sep = "\t")

# clean data 
bls3 <- bls2[-1, -5]
names(bls3) <- c("id", "year", "month", "rate")
bls3 <- mutate(bls3, fips_state_county = str_sub(bls3[ , 1], start = 6, end = 10))
bls3$fips_state_county2 <- str_replace(bls3$fips_state_county, "^[0]+", "")
bls3 <- mutate(bls3, measure = str_sub(bls3[ , 1], start = 20, end = 20))

# filter bls3 down to just annual average unemployment rates 
bls4 <- filter(bls3, month == "M13", measure == "3") 
bls4 <- select(bls4, year, rate, fips_state_county2)
bls4$rate <- as.character(bls4$rate)

# for some reason, seven counties have no rate listed for 2005 and 2006
dash <- unique(bls4$rate)[236]
dash <- as.character(dash)
bad_data <- filter(bls4, rate == dash)

# average rate for 2004 and 2007, and impute this rate for the missing 2005 and 2006
for(i in unique(bad_data$fips_state_county2)){
        y2004 <- filter(bls4, fips_state_county2 == get("i"), year == 2004)[1, 2]
        y2007 <- filter(bls4, fips_state_county2 == get("i"), year == 2007)[1, 2]
        avg <- mean(c(as.numeric(as.character(y2004)), as.numeric(as.character(y2007))))
        row2004 <- which(bls4$fips_state_county2 == get("i") & bls4$year == 2004)
        bls4$rate[row2004 + 1] <- avg
        bls4$rate[row2004 + 2] <- avg
}

# convert year, rate, and fips_state_county2 to numeric
bls4$year <- as.numeric(as.character(bls4$year))
bls4$rate <- as.numeric(as.character(bls4$rate))
bls4$fips_state_county2 <- as.numeric(as.character(bls4$fips_state_county2))

# create discretized factor for rate to use in color fill legend
bls4$rate_d <- cut(bls4$rate, c(seq(from = 0, to = 40, by = 2)))

# clean up discrete unemployment rate buckets for use in legend
bls4$rate_d2 <- str_replace(bls4$rate_d, "\\(", "")
bls4$rate_d2 <- str_replace(bls4$rate_d2, "\\]", "")
bls4$rate_d2 <- str_replace(bls4$rate_d2, ",", "% - ")
bls4$rate_d2 <- str_c(bls4$rate_d2, "%", sep = "")

# dcast bls4 to go from long format to wide format
bls5 <- dcast(bls4, fips_state_county2 ~ year, value.var = "rate_d2")

# rename year variables to avoid using numbers as variable names
for(i in 2:26){
        names(bls5)[i] <- str_c("y", names(bls5[i]))
}

# discretize year variables as ordered factors
levels <- c("0% - 2%", "2% - 4%", "4% - 6%", "6% - 8%", "8% - 10%", "10% - 12%", "12% - 14%", "14% - 16%",
            "16% - 18%", "18% - 20%", "20% - 22%", "22% - 24%", "24% - 26%", "26% - 28%", "28% - 30%", "30% - 32%",
            "32% - 34%", "34% - 36%", "36% - 38%", "38% - 40%", "40% - 42%")

for(i in 2:26){
        bls5[ , i] <- factor(bls5[ , i], levels = levels, ordered = TRUE)
}

# load lat/long map data for county and state boundaries
county_map <- map_data("county")
state_map <- map_data("state")

# attach county fips code data
data(county.fips)

# create states dataframe with state names and abbreviations
states <- data.frame(state.abb, state.name)
states$state.name <- tolower(state.name)

# merge states with county_map to create county_df
county_df <- merge(county_map, states, by.x = "region", by.y = "state.name")

# create a polyname variable in county_df to use for merge with county.fips
county_df$polyname <- str_c(county_df$region, county_df$subregion, sep = ",")

# merge county_df with county.fips based on polyname
county_df <- merge(county_df, county.fips, by = "polyname")

# merge county_df with unemp based on fips
choropleth <- merge(county_df, bls5, by.x = "fips", by.y = "fips_state_county2")

# re-order choropleth by order variable to keep shape file lat/long points in order
choropleth <- choropleth[order(choropleth$order), ]

# select color palette using brewer.pal
# then use colorRampPalette to build a function "pal" which divides the palette by a given number of factors
# display.brewer.all()
colors <- brewer.pal(9, "Blues")
pal <- colorRampPalette(colors)
# pal is a function, which takes a number as it's argument eg. pal(14)
# to generalize: pal(length(unique(choropleth$rate_d1)))

# create choropleth of all counties in US
# will take a minute to load 
choropleth_input <- choropleth
state_map_input <- state_map

ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = rate_d2), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_map_input, colour = "black", fill = NA) +
        scale_fill_manual(values = pal(length(unique(choropleth_input$rate_d2)))) + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title=element_text(size=20,face="bold")) + 
        labs(x = "", y = "", title = str_c("Unemployment rate in ", year_input), fill = "Unemployment rate") + coord_fixed() + coord_map(project = "conic", lat0 = 30)

# create choropleth of just counties in alabama
# filter choropleth and state_map down to just alabama rows
# state names should be all lower case full names
state_input <- c("louisiana", "alabama", "mississippi")
year_input <- "2011"
choropleth_input <- filter(choropleth, region %in% state_input)
choropleth_input <- select(choropleth_input, 1:9, which(names(choropleth_input) == str_c("y", year_input)))
choropleth_input <- choropleth_input[order(choropleth_input$order ), ]
names(choropleth_input)[10] <- "rate_d2"
state_map_input <- filter(state_map, region %in% state_input)

ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = rate_d2), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_map_input, colour = "black", fill = NA) +
        scale_fill_manual(values = pal(length(unique(choropleth_input$rate_d2)))) + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title = element_text(size=20, face = "bold")) + 
        labs(x = "", y = "", title = str_c(year_input, " Unemployment Rate"), fill = "Unemployment rate") + coord_fixed() + coord_map(project = "conic", lat0 = 30)














