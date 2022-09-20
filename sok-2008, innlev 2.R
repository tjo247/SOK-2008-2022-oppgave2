### UTFORDRING 2.3

#Har brukt hjelpearket.

library("ggplot2")
library("tidyverse")
library("readr")
library("plyr")
library("dplyr")


# Set your working directory to the correct folder. 
# Insert your file path for 'YOURFILEPATH'. 
getwd()

# You will need the following libraries for the assignment:

library(readr) # fileformat of the dataset
library(ggplot2)     # the ggplot package
library(tidyverse)  # the tidyverse package


# To carry out the assignment, you will need to combine the union_unempl data with map data. 

union_unempl <- read_csv("https://uit-sok-2008-h22.github.io/Assets/union_unempl.csv")
 #This loads the data with information about the variables of interest
View(union_unempl) #Displays the data
#To combine the unemployment and union data with the map data, we merge on country name. 
#We face two problems here: 1) United Kingdom is called "UK" in the map data, 2) the variable "country" is called "region" in the map data. We need to change this.

#Changing the name of a single observation. The below code changes all observations called "United Kingdom" to "UK" in the union data. 
union_unempl$country <- gsub("United Kingdom", "UK", union_unempl$country)

# Renaming a variable. The below code renames the variable "Country" to "Region".
names(union_unempl)[names(union_unempl) == "country"] <- "region"

# Creating a new variable. To create a map showing "Excess coverage", you need to create a new variable. 
# The below code shows how to create a new variable in R. 
union_unempl$newvar2<-union_unempl$var1 + union_unempl$var2 #A sum
union_unempl$newvar1<-union_unempl$var1 - union_unempl$var2 #A difference
union_unempl$newvar3<-(union_unempl$var1 + union_unempl$var2)/2 # A mean value

# You are now ready to create your maps! Follow the tutorial at https://www.youtube.com/watch?v=AgWgPSZ7Gp0 

# The "Coord" variable takes 5 discrete levels. It may therefore be better to use a discrete scale for the coloring. 
# To do this, simply replace "scale_fill_gradient(name="name", low="color1", high="color2", na.value="grey50")" 
# with "scale_fill_brewer(name="name", palette = "Set1")" (or another set you prefer)

# Utfordring 2.3:

#1. Lag kart over Europa som viser 1) arbeidsledighetsrate i ulike land.

# slår  sammen kartdata med datasettet "union_unempl" til ett datasett
mapdata <- map_data("world")

mapdata <- left_join(mapdata, union_unempl, by = "region")


# beholder kun Europa:
mapdata_1 <- mapdata %>% 
  filter(!is.na(mapdata$unempl))


# Lager kart for arbeidsledighetsrate "uempl":
Unempl <- ggplot(mapdata_1, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = unempl), color = "black") +
  scale_fill_gradient(name = "% unemployed", low = "white", high = "green", na.value = "black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Unemployment in Europe")
Unempl


#2. Lag kart over Europa som viser 1) fagforeningsdensitet, 2) “Excess coverage”, 
#og 3) Koordinering av lønnsfastsettelse.

# 1. Fagforeningsdensitet
#  bruker variablen "density" 
union_unempl_density <- ggplot(mapdata_1, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = density), color = "black") +
  scale_fill_gradient(name = "union density in %", low = "white", high = "green", na.value = "black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Union density in Europe")
union_unempl_density


# 2. Excess coverage
#  ny variabel "cov" som er diff mellom organisasjonsprosent målt i andelen av arbeidsstyrken og 
# andelen som er omfattet av tariffavtaler.

mapdata_1$cov <- mapdata_1$coverage - mapdata_1$density
mapdata_1

Excess_coverage <- ggplot(mapdata_1, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = excov), color = "black") +
  scale_fill_gradient(name = "Excess coverage in %", low = "white", high = "green", na.value = "black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Excess coverage in Europe")
Excess_coverage

# 3. Koordinering

Coordination <- ggplot(mapdata_1, aes(x =long, y = lat, group = group)) +
  geom_polygon(aes(fill = coord), color = "black") +
  scale_fill_brewer(name="Coordination level", palette = "Set5") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_blank()) +
  ggtitle("Coordination of wage bargaining in Europe")
Coordination

#3. Diskuter det du ser i dine grafer ut ifra kapittel 3 i Boeri og van Ours. 
# Kommenter hvordan det kan komme seg at de nordiske landene har sterke fagforeninger 
# og relativt lav arbeidsledighet

######_____Gjort i word-dokument_____
