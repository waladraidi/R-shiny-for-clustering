#libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(rlang)
library(broom)
library(readxl)
library(writexl)
library(viridis)
library(rgdal)
library(tidyverse)
library(sf)
library(tmap)
library(spgwr)
library(olsrr)
library(caret)
library(leaps)
library(MASS)
library(readr)
setwd("C:/Users/n10677313/OneDrive - Queensland University of Technology/Desktop/31-3 shiny")
Data<- read.csv("heat_maps.csv")
Data$X.1 = NULL
## Shapefiles:
AUS_SA2_SHP <- read_sf("C:/Users/n10677313/OneDrive - Queensland University of Technology/Desktop/31-3 shiny","SA2_2016_AUST") %>% filter(!st_is_empty(.)) # SA2.
QLD_SA2_SHP <- AUS_SA2_SHP %>% filter(STE_CODE16==3) %>% filter(!st_is_empty(.)) # QLD, SA2.
mydata_and_myMap<- inner_join(QLD_SA2_SHP,
                              Data,
                              by = c("SA2_NAME16" = "SA2_NAME16"))

new_shape<- na.omit(mydata_and_myMap)
library(sp)
library(sf)
library(rgdal)
st_write(new_shape, "C:/Users/n10677313/OneDrive - Queensland University of Technology/Desktop/31-3 shiny", "new_shape", 
         driver = "ESRI Shapefile")
AUS_SA2_mymap <- read_sf("C:/Users/n10677313/OneDrive - Queensland University of Technology/Desktop/31-3 shiny","new_shape") %>% filter(!st_is_empty(.))
