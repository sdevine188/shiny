state_boundaries <- readOGR("data/cb_2015_us_state_20m/cb_2015_us_state_20m.shp",
                            layer = "cb_2015_us_state_20m", verbose = FALSE)

county_boundaries <- readOGR("data/cb_2015_us_county_20m/cb_2015_us_county_20m.shp",
                            layer = "cb_2015_us_county_20m", verbose = FALSE)

cd_boundaries <- readOGR("data/cb_2014_us_cd114_20m/cb_2014_us_cd114_20m.shp",
                             layer = "cb_2014_us_cd114_20m", verbose = FALSE)

pal <- colorNumeric(
        palette = "Blues",
        domain = as.numeric(as.character(state_boundaries$STATEFP))
)

pal <- colorNumeric(
        palette = "Blues",
        domain = as.numeric(as.character(county_boundaries$COUNTYFP))
)

pal <- colorNumeric(
        palette = "Blues",
        domain = as.numeric(as.character(cd_boundaries$CD114FP))
)

pal <- colorFactor(
        palette = "Set1",
        domain = factor(cd_boundaries$CD114FP)
)

leaflet(cd_boundaries) %>% addTiles() %>% fitBounds(-160.0583, 20.65798, -60.954694, 60.60825) %>%
        addPolygons(
                stroke = FALSE, fillOpacity = 0.75, smoothFactor = 0.5,
                color = ~ pal(as.numeric(as.character(cd_boundaries$CD114FP))),
                popup = ~ cd_boundaries$CD114FP
        )

leaflet(cd_boundaries) %>% addTiles() %>% fitBounds(-160.0583, 20.65798, -60.954694, 60.60825) %>%
        addPolygons(
                stroke = TRUE, color = "white", weight = 1, fillOpacity = 0.75, smoothFactor = 0.5,
                fillColor = ~ pal(cd_boundaries$CD114FP),
                popup = ~ cd_boundaries$CD114FP
        )

leaflet(county_boundaries) %>% addTiles() %>%
        addPolygons(
                stroke = FALSE, fillOpacity = 0.75, smoothFactor = 0.5,
                color = ~ pal(as.numeric(as.character(county_boundaries$COUNTYFP))),
                popup = ~ county_boundaries$NAME
        )






# datafile_map <- datafile %>% select(Appl.FIPS.ST, Best.EDA..)
datafile_map <- datafile %>% filter(Appl.State.Abbr %in% c("CT", "MA", "NY"), !is.na(Appl.FIPS.ST)) %>% 
        select(Appl.FIPS.ST, Best.EDA..)
state_list <- data.frame("state_fips" = state_boundaries$STATEFP)
state_list$state_fips <- as.numeric(state_list$state_fips)
state_list <- filter(state_list, state_fips %in% unique(datafile_map$Appl.FIPS.ST))
state_funding <- datafile_map %>% group_by(Appl.FIPS.ST) %>% summarize(funding = sum(Best.EDA..))
map_data <- left_join(state_list, state_funding, by = c("state_fips" = "Appl.FIPS.ST"))
map_data$state_fips <- factor(str_pad(map_data$state_fips, width = 2, side = "left", pad = "0"))
state_boundaries2 <- subset(state_boundaries, state_boundaries$STATEFP %in% unique(map_data$state_fips))
state_boundaries2$funding <- map_data$funding




# for cd map
datafile_map <- shiny_app_data %>% filter(!is.na(Appl.FIPS.ST), !is.na(Appl.Cong.Dist), Best.EDA.. < 50000000) %>% 
        select(Appl.FIPS.ST, Appl.Cong.Dist, Best.EDA..)
datafile_map$state_cd_fips <- str_c(str_pad(datafile_map$Appl.FIPS.ST, width = 2, side = "left", pad = "0"),
                                    str_pad(datafile_map$Appl.Cong.Dist, width = 2, side = "left", pad = "0"))
cd_list <- data.frame("state_fips" = cd_boundaries$STATEFP, "cd_fips" = cd_boundaries$CD114FP)
for(i in 1:nrow(cd_list)){
        if(cd_list$cd_fips[i] == "00"){
                cd_list$cd_fips[i] <- "01"
        }
}
cd_list$cd_fips <- str_pad(cd_list$cd_fips, width = 2, side = "left", pad = "0")
cd_list$state_cd_fips <- str_pad(str_c(cd_list$state_fips, cd_list$cd_fips), width = 4, 
                                 side = "left", pad = "0")
cd_boundaries$state_cd_fips <- cd_list$state_cd_fips
cd_list <- filter(cd_list, as.character(state_cd_fips) %in% unique(datafile_map$state_cd_fips))
cd_funding <- datafile_map %>% group_by(state_cd_fips) %>% summarize(funding = sum(Best.EDA..), count = n())
map_data <- left_join(cd_list, cd_funding, by = c("state_cd_fips" = "state_cd_fips"))
map_data$state_cd_fips <- factor(map_data$state_cd_fips)
map_boundaries <- subset(cd_boundaries, cd_boundaries$state_cd_fips %in% unique(map_data$state_cd_fips))
map_boundaries$funding <- map_data$funding
map_boundaries$count <- map_data$count

fund_pal <- colorNumeric(
        palette = "Blues",
        domain = map_boundaries$funding
)

leaflet(map_boundaries) %>% addTiles() %>%
        addPolygons(
                stroke = FALSE, fillOpacity = 0.75, smoothFactor = 0.5,
                color = ~ fund_pal(funding),
                popup = ~ str_c(CD114FP, funding, count, sep = "<br/>")
        )


x <- filter(datafile_map, Appl.FIPS.ST == 16)
head(x)
unique(x$state_cd_fips)
filter(shiny_app_data, Appl.FIPS.ST == 10) %>% select(Appl.FIPS.ST, Appl.Cong.Dist, Appl.State.Abbr) %>% head(.)
filter(cd_list, state_fips == 10)
filter(cd_list, state_fips == 16)
x <- shiny_app_data %>% filter(Appl.FIPS.ST == 16) %>% select(Appl.State.Abbr, Appl.Cong.Dist)
unique(x$Appl.Cong.Dist)
filter(x, Appl.Cong.Dist == 98)
cd_boundaries$STATEFP[which(!(cd_boundaries$STATEFP %in% map_boundaries$STATEFP))]
unique(datafile_map$Appl.FIPS.ST)
datafile_map %>% filter(Appl.FIPS.ST == 10) %>% head(.)
which(cd_list$state_cd_fips == 1001)
which(unique(datafile_map$Appl.FIPS.ST == 10))
which(unique(map_boundaries$STATEFP) == 16) # delaware, ak, sd, nd, mt, wy, vt
which(unique(cd_boundaries$STATEFP) == 10)
shiny_app_data %>% filter(Appl.FIPS.ST == 10) %>% select(Appl.State.Abbr, Appl.Cong.Dist) %>% head(.)
state.fips %>% filter(fips == 50) %>% select(fips, polyname)



# for county map
datafile_map <- shiny_app_data %>% filter(!is.na(Appl.FIPS.ST), !is.na(Appl.FIPS.Cnty)) %>% 
        select(Appl.FIPS.ST, Appl.FIPS.Cnty, Best.EDA..)
datafile_map$state_county_fips <- str_c(str_pad(datafile_map$Appl.FIPS.ST, width = 2, side = "left", pad = "0"),
                                        str_pad(datafile_map$Appl.FIPS.Cnty, width = 3, side = "left", pad = "0"))
# datafile_map <- datafile_map %>% filter(Best.EDA.. < 10000000)
county_list <- data.frame("state_fips" = county_boundaries$STATEFP, "county_fips" = county_boundaries$COUNTYFP)
county_list$state_county_fips <- str_pad(str_c(county_list$state_fips, county_list$county_fips), width = 5, 
                                         side = "left", pad = "0")
county_boundaries$state_county_fips <- county_list$state_county_fips
county_list <- filter(county_list, as.character(state_county_fips) %in% unique(datafile_map$state_county_fips))
county_funding <- datafile_map %>% group_by(state_county_fips) %>% summarize(funding = sum(Best.EDA..))
map_data <- left_join(county_list, county_funding, by = c("state_county_fips" = "state_county_fips"))
map_data$state_county_fips <- factor(map_data$state_county_fips)
map_boundaries <- subset(county_boundaries, county_boundaries$state_county_fips %in% unique(map_data$state_county_fips))
map_boundaries$funding <- map_data$funding


dropped <- which(!(unique(datafile_map$state_county_fips) %in% county_list$state_county_fips))
dropped_fips <- unique(datafile_map$state_county_fips)[dropped]
dropped_records <- datafile_map[which(datafile_map$state_county_fips %in% dropped_fips), ]
head(unique(dropped_records$state_county_fips))
which(county_list$state_county_fips == "02270")
which(county_list2$state_county_fips == 05121)
which(county_list2$state_county_fips == "05121")
which(datafile_map$state_county_fips == "05121")
which(datafile_map$state_county_fips == 05121)
which(unique(datafile_map$state_county_fips) == "05121")
county_list$state_county_fips[540]
str(county_list)
str(datafile_map)
dim(county_list2)
length(unique(datafile_map$state_county_fips))
ggplot(county_funding, aes(x = funding)) + geom_histogram()
max(county_funding$funding)
essex <- datafile_map %>% filter(state_county_fips == "34013") %>% 
        select(Best.EDA.., Appl.Short.Name, Appl.Cnty.Name, state_county_fips, Status) %>% data.frame(.)
high <- datafile_map %>% filter(Best.EDA.. > 10000000) %>% 
        select(Best.EDA.., Appl.Short.Name, Appl.Cnty.Name, state_county_fips, Status) %>% data.frame(.)
max(high$Best.EDA..)
high %>% arrange(desc(Best.EDA..))
ggplot(high, aes(x = Best.EDA..)) + geom_histogram()

library(map)
# https://www.census.gov/geo/reference/codes/cou.html
head(state.fips)
head(county.fips)
county.fips %>% filter(fips == "34013")
county.fips %>% filter(grepl("florida", polyname, ignore.case = TRUE))
shiny_app_data %>% filter(Appl.FIPS.ST == "6", Appl.FIPS.Cnty == "10") %>% select(Appl.Cnty.Name) %>% dim(.)
datafile_map %>% filter(state_county_fips == "64003") %>% select(Appl.Cnty.Name, Appl.State.Abbr) %>% head(., 1)

head(state_boundaries$STATEFP)
head(state_list)
head(state_funding)
datafile %>% filter(Appl.FIPS.ST == 1) %>% summarize(funding = sum(Best.EDA..))
head(state_boundaries)
state_boundaries$NAME
attributes(state_boundaries)
str(state_boundaries)
state_boundaries[[2]]
names(state_boundaries)
length(state_boundaries$STATEFP)
state_boundaries$STATEFP
length(unique(datafile$Appl.FIPS.ST))
head(state_boundaries$funding)
test <- data.frame("state" = state_boundaries2$STATEFP, "funding" = state_boundaries2$funding)
test <- test %>% arrange(state)
map_data2 <- arrange(map_data, state_fips)
head(test)
head(map_data2)

fund_pal <- colorNumeric(
        palette = "Blues",
        domain = state_boundaries2$funding
)
fund_pal(257902961)


leaflet(state_boundaries2) %>% addTiles() %>%
        addPolygons(
                stroke = FALSE, fillOpacity = 0.75, smoothFactor = 0.5,
                color = ~ fund_pal(state_boundaries2$funding),
                popup = ~ str_c(state_boundaries2$NAME, state_boundaries2$funding, sep = "<br/>")
        )




# update map shape file with funding data for selected records
# datafile_map <- data_table5_filtered %>% filter(!is.na(Appl.FIPS.ST)) %>% select(Appl.FIPS.ST, Best.EDA..)
datafile_map <- datafile %>% filter(!is.na(Appl.FIPS.ST)) %>% select(Appl.FIPS.ST, Best.EDA..)
state_list <- data.frame("state_fips" = state_boundaries$STATEFP)
state_list$state_fips <- as.numeric(as.character(state_list$state_fips))
state_list <- filter(state_list, state_fips %in% unique(datafile_map$Appl.FIPS.ST))
state_funding <- datafile_map %>% group_by(Appl.FIPS.ST) %>% summarize(funding = sum(Best.EDA..))
map_data <- left_join(state_list, state_funding, by = c("state_fips" = "Appl.FIPS.ST"))
map_data$state_fips <- factor(str_pad(map_data$state_fips, width = 2, side = "left", pad = "0"))
state_boundaries2 <- subset(state_boundaries, state_boundaries$STATEFP %in% unique(map_data$state_fips))
state_boundaries2$funding <- map_data$funding

map_boundaries_fund_pal <- colorNumeric(
        palette = "Blues",
        domain = state_boundaries2$funding
)

leaflet(state_boundaries2) %>% addTiles() %>%
        addPolygons(
                stroke = FALSE, fillOpacity = 0.75, smoothFactor = 0.5,
                color = ~ map_boundaries_fund_pal(state_boundaries2$funding),
                popup = ~ str_c(state_boundaries2$NAME, state_boundaries2$funding, sep = "<br/>")
        )


which(unique(datafile_map$Appl.FIPS.ST) == 53)
which(state_list$state_fips == 53)
state_list[17:19, ]
