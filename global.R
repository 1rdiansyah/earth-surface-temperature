#package
library(tidyverse)
library(plotly)
library(scales)
library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgdal)
library(leaflet)
library(stringr)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(DT)

#main-data
temp_country <- read.csv("GlobalLandTemperaturesByCountry.csv")

#data-cleansing
temp_country <- temp_country %>%
  separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE)

temp_country <-na.omit(temp_country)

#data-smoohtplot
temp_country2 <- temp_country %>%
  filter(Year>1900) %>%
  group_by(Year) %>% 
  summarise(Temp = max(AverageTemperature))

temp_month <- temp_country %>%
  mutate(Month = month.abb[as.factor(Month)],
         Country = as.factor(Country),
         Temp = as.numeric(AverageTemperature)) %>%
  select(Month, Country, Temp)

#data-barplot
temp_country_ <- temp_country %>% 
  filter(AverageTemperature>30) %>%
  mutate(Country = as.factor(Country),
         Year = as.factor(Year),
         Temp = as.numeric(AverageTemperature)) %>%
  select(Year, Country, Temp)

#data-barplot2
temp_less <- temp_country %>% 
  filter(AverageTemperature<30 & AverageTemperature>25) %>%
  mutate(Country = as.factor(Country),
         Year = as.factor(Year),
         Temp = as.numeric(AverageTemperature)) %>%
  select(Year, Country, Temp)

#hist-per-country
temp_hist <- temp_country %>%
  filter(Year > 1975) %>%
  mutate(Country = as.factor(Country),
         Year = as.factor(Year),
         Temp = as.numeric(AverageTemperature)) %>%
  select(Year, Country, Temp)

#leaflet-data
leafdata <- temp_country %>% 
  filter(Year > 1975) %>%
  mutate(Country = as.factor(Country)) %>% 
  group_by(Country) %>% 
  summarise(max_temp = max(AverageTemperature), min_temp = min(AverageTemperature)) %>% 
  arrange(desc(max_temp)) %>% 
  rename(NAME = Country) %>% 
  ungroup()

#Leaflet-shape
shape <- raster::shapefile("TM_WORLD_BORDERS_SIMPL-0.3.shp")

#combining-data-into-shape-data
shape@data <- shape@data %>% dplyr::left_join(leafdata, by = "NAME")

#replacing-column-for-rows-with-different-country
shape@data[shape@data$NAME=="Iran (Islamic Republic of)",c(12:13)] <- leafdata[leafdata$NAME=="Iran",c(2:3)]
shape@data[shape@data $NAME=="Libyan Arab Jamahiriya",c(12:13)] <- leafdata[leafdata$NAME=="Libya",c(2:3)]
shape@data[shape@data $NAME=="United Republic of Tanzania",c(12:13)] <- leafdata[leafdata$NAME=="Tanzania",c(2:3)]
shape@data[shape@data $NAME=="Democratic Republic of the Congo",c(12:13)] <- leafdata[leafdata$NAME=="Congo (Democratic Republic Of The)",c(2:3)]
shape@data[shape@data$NAME=="Cote d'Ivoire",c(12:13)] <- leafdata[leafdata$NAME=="Côte D'Ivoire",c(2:3)]
shape@data[shape@data$NAME=="Guinea-Bissau",c(12:13)] <- leafdata[leafdata$NAME=="Guinea Bissau",c(2:3)]
shape@data[shape@data$NAME=="Syrian Arab Republic",c(12:13)] <- leafdata[leafdata$NAME=="Syria",c(2:3)]
shape@data[shape@data$NAME=="Bosnia and Herzegovina",c(12:13)] <- leafdata[leafdata$NAME=="Bosnia And Herzegovina",c(2:3)]
shape@data[shape@data$NAME=="Republic of Moldova",c(12:13)] <- leafdata[leafdata$NAME=="Moldova",c(2:3)]
shape@data[shape@data$NAME=="The former Yugoslav Republic of Macedonia",c(12:13)] <- leafdata[leafdata$NAME=="Macedonia",c(2:3)]
shape@data[shape@data$NAME=="Lao People's Democratic Republic",c(12:13)] <- leafdata[leafdata$NAME=="Laos",c(2:3)]
shape@data[shape@data$NAME=="Viet Nam",c(12:13)] <- leafdata[leafdata$NAME=="Vietnam",c(2:3)]
shape@data[shape@data$NAME=="Timor-Leste",c(12:13)] <- leafdata[leafdata$NAME=="Timor Leste",c(2:3)]
shape@data[shape@data$NAME=="Korea, Democratic People's Republic of",c(12:13)] <- leafdata[leafdata$NAME=="North Korea",c(2:3)]
shape@data[shape@data$NAME=="Korea, Republic of",c(12:13)] <- leafdata[leafdata$NAME=="South Korea",c(2:3)]
shape@data[shape@data$NAME=="Svalbard",c(12:13)] <- leafdata[leafdata$NAME=="Svalbard And Jan Mayen",c(2:3)]
shape@data[shape@data$NAME=="Falkland Islands (Malvinas)",c(12:13)] <- leafdata[leafdata$NAME=="Falkland Islands (Islas Malvinas)",c(2:3)]
shape@data[shape@data$NAME=="South Georgia South Sandwich Islands",c(12:13)] <- leafdata[leafdata$NAME=="South Georgia And The South Sandwich Isla",c(2:3)]
shape@data[shape@data $NAME=="French Southern and Antarctic Lands",c(12:13)] <- leafdata[leafdata$NAME=="French Southern And Antarctic Lands",c(2:3)]
shape@data[shape@data$NAME=="Micronesia, Federated States of",c(12:13)] <- leafdata[leafdata$NAME=="Federated States Of Micronesia",c(2:3)]
shape@data[shape@data$NAME=="Åland Islands",c(12:13)] <- leafdata[leafdata$NAME=="Åland",c(2:3)]
shape@data[shape@data$NAME=="Isle of Man",c(12:13)] <- leafdata[leafdata$NAME=="Isle Of Man",c(2:3)]
shape@data[shape@data$NAME=="Antigua and Barbuda",c(12:13)] <- leafdata[leafdata$NAME=="Antigua And Barbuda",c(2:3)]
shape@data[shape@data$NAME=="Trinidad and Tobago",c(12:13)] <- leafdata[leafdata$NAME=="Trinidad And Tobago",c(2:3)]
shape@data[shape@data$NAME=="Saint Kitts and Nevis",c(12:13)] <- leafdata[leafdata$NAME=="Saint Kitts And Nevis",c(2:3)]
shape@data[shape@data$NAME=="Heard Island and McDonald Islands",c(12:13)] <- leafdata[leafdata$NAME=="Heard Island And Mcdonald Islands",c(2:3)]
shape@data[shape@data$NAME=="Sao Tome and Principe",c(12:13)] <- leafdata[leafdata$NAME=="Sao Tome And Principe",c(2:3)]
shape@data[shape@data$NAME=="Sao Tome and Principe",c(12:13)] <- leafdata[leafdata$NAME=="Sao Tome And Principe",c(2:3)]
shape@data[shape@data$NAME=="United States Virgin Islands",c(12:13)] <- leafdata[leafdata$NAME=="Virgin Islands",c(2:3)]
shape@data[shape@data$NAME=="Turks and Caicos Islands",c(12:13)] <- leafdata[leafdata$NAME=="Turks And Caicas Islands",c(2:3)]

#color-palette
mypalette <- colorNumeric(c("yellow", "red"), NULL,na.color="transparent")

#label
mytext <- paste(shape@data$NAME) %>%
  lapply(htmltools::HTML)

#pop-up
popup_shape <- paste("<h3><b>", shape@data$NAME, "</b></h3>", 
                     "Maximum Temperature: ", shape@data$max_temp, "<br>", 
                     "Minimum Temperature: ", shape@data$min_temp, "<br>",
                     sep="")
#table

datatable <- shape@data %>% 
         select(NAME, LON, LAT, max_temp, min_temp) %>% 
         group_by(NAME) %>% 
         arrange(desc(max_temp))