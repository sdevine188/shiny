library(ggmap)
library(stringr)
library(dplyr)
library(lubridate)
library(taRifx.geo)

# set working directory where master_data file is saved
setwd("G:/PNP/Performance Measurement/master_data")

# read in master_data
master_data_filename <- list.files()[str_detect(list.files(), "master_data_20")]
master_data <- read.csv(master_data_filename, stringsAsFactors = FALSE, colClasses = c("Control." = "character", 
                                 "Project.No." = "character", "Proj.ZIP" = "character", "Appl..Zip" = "character",
                                 "Initiatives" = "character"))
md <- master_data

# compile address field to use in geocoding
md$address <- str_c(md$Appl.Street.Addr.1, md$Appl.City.Name, md$Appl.State.Abbr, md$Appl..Zip, sep = ", ")
md$address <- as.character(md$address)

# split into 2500 row sizes to stay below goolge maps api daily limit
md1 <- md[1:3, ]
md1 <- md[1:2500, ]
md2 <- md[2501:5000, ]

# geocode addresses
for(i in 1:nrow(md2)){
        coordinates <- tryCatch(geocode(md2$address[i], service = "bing"), 
                                warning = function(w) {
                                        if(grepl("geocode failed with status ZERO_RESULTS", w)){
                                                print("this is a warning")
                                                address <- md2$address[i]
                                                zip <- md2$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                },
                                error = function(e) {
                                        if(grepl("replacement has length zero", e)){
                                                print("this is an error")
                                                address <- md2$address[i]
                                                zip <- md2$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                } 
        )
        coordinates <- unlist(coordinates)
        print(i)
        print(coordinates)
        if(is.null(coordinates)){
                print("error: coordinates are null")
                address <- md2$address[i]
                zip <- md2$Appl..Zip[i]
                coordinates <- geocode(zip, service = "bing")
                coordinates <- unlist(coordinates)
                print(coordinates)
                md2$lon[i] <- coordinates[1]
                md2$lat[i] <- coordinates[2]
        }
        md2$lon[i] <- coordinates[1]
        md2$lat[i] <- coordinates[2]
}

# which(is.na(md1$lat))
# which(md1$Appl..Zip == "") 

df <- data.frame(address = c("56 talmadge hill road, prospect, ct 06712", "22 skyline drive, prospect, ct 06712", 
                             "prospect, ct 06712", "1 Univerisity Boulevard, St. Louis, MO, 63121", 
                             "Post Office Box 333, Toronto, SD, 57268"))
df$address <- as.character(df$address)
for(i in 1:nrow(df)){
        coordinates <- geocode(df$address[1])
        coordinates2 <- unlist(coordinates)
        df$lon[1] <- coordinates2[1]
        df$lat[1] <- coordinates2[2]
}

df$Appl..Zip <- str_sub(df$address, start = -5)
for(i in 1:nrow(df)){
        coordinates <- tryCatch(geocode(df$address[i], service = "bing"), 
                                warning = function(w) {
                                        if(grepl("geocode failed with status ZERO_RESULTS", w)){
                                                print("this is a warning")
                                                address <- df$address[i]
                                                zip <- df$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                },
                                error = function(e) {
                                        if(grepl("replacement has length zero", e)){
                                                print("this is an error")
                                                address <- df$address[i]
                                                zip <- df$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                } 
        )
        coordinates <- unlist(coordinates)
        print(i)
        print(coordinates)
        if(is.null(coordinates)){
                print("error: coordinates are null")
                address <- df$address[i]
                zip <- df$Appl..Zip[i]
                coordinates <- geocode(zip, service = "bing")
                coordinates <- unlist(coordinates)
                print(coordinates)
                df$lon[i] <- coordinates[1]
                df$lat[i] <- coordinates[2]
        }
        df$lon[i] <- coordinates[1]
        df$lat[i] <- coordinates[2]
}

# Error in md1$lon[i] <- coordinates[1] : replacement has length zero


options(BingMapsKey = "QFG8iBCiInAj6ER1ubuD~I5piVwPPZghOvhCJzBP-1g~AicfV1u7mkoKlY53KfatxR67u-NHXCfu1iEA8dBryA8vlUJy3yu3y0u2cZLWf-D4")

gc <- geocode("Post Office Box 333, Toronto, SD, 57268", service = "bing")
gc2 <- geocode("22 skyline drive, prospect, ct 06712")
gc_df <- bind_rows(gc, gc2)
gc_df$lat_long <- str_c(gc_df$lat, gc_df$lon, sep = ":")