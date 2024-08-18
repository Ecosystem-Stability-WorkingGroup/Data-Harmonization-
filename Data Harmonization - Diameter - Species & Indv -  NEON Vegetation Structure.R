############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#NEON - Vegetation Structure
# - Combined Woody Data
# - NEON.Woody.Plants.Combined.csv
NEON.struc.woody.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Multiple/NEON/NEON_struct-plant/NEON.Woody.Plants.Combined.csv")
str(NEON.struc.woody.dat)

#NEON - Vegetation Structure
# - Combined NonWoody Data
# - NEON.NonWoody.Plants.Combined.csv
NEON.struc.nonwoody.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Multiple/NEON/NEON_struct-plant/NEON.NonWoody.Plants.Combined.csv")
str(NEON.struc.nonwoody.dat)

#NEON - Unified Coordinates
NEON.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Multiple/NEON/NEON.Unified.Coordinates.RYF.csv")
str(NEON.coords)

############################################################
#Formatting Data

#NEON - Vegetation Structure
# - Combined Woody Data
# - NEON.Woody.Plants.Combined.csv
NEON.struc.woody.DIAdat<-NEON.struc.woody.dat%>%
  filter(!is.na(plotID.y))%>%
  mutate(site = plotID.y, 
         Parameter = "Abundance, Diameter, Height",
         data = "NEON - Vegetation Structure - Woody",
         Taxa = "Vegetation",
         Specificity = "Individuals",
         ecosystem = nlcdClass,
         std_id = paste0("", "_", site),
         plot = subplotID,
         transect = 999,
         date = lubridate::parse_date_time(date.y, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = scientificName,
         tree_id = as.character(individualID),
         stem_id = "no_stem_id",
         diameter = bandStemDiameter,
         diam_units = "cm")%>%
  left_join(NEON.coords, by = c("site" = "plotID"))%>%
  mutate(latitude = Latitude.y,
         longitude = Longitude.y)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'subplotID' - middle value represents plot size. Diameter taken from 'bandStemDiameter'")

str(NEON.struc.woody.dat)
str(NEON.struc.woody.DIAdat)

#Species checking
spp.check<-NEON.struc.woody.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()



#NEON - Vegetation Structure
# - Combined NonWoody Data
# - NEON.NonWoody.Plants.Combined.csv
NEON.struc.nonwoody.DIAdat<-NEON.struc.nonwoody.dat%>%
  mutate(site = plotID.y, 
         Parameter = "Abundance, Diameter, Height",
         data = "NEON - Vegetation Structure - Non-Woody",
         Taxa = "Vegetation",
         Specificity = "Individuals",
         ecosystem = nlcdClass,
         std_id = paste0("", "_", site),
         plot = subplotID,
         transect = 999,
         date = lubridate::parse_date_time(date.y, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = scientificName.x,
         tree_id = as.character(individualID),
         stem_id = "no_stem_id",
         diameter = stemDiameter,
         diam_units = "cm")%>%
  left_join(NEON.coords, by = c("site" = "plotID"))%>%
  mutate(latitude = Latitude.y,
         longitude = Longitude.y)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'subplotID' - middle value represents plot size. Diameter taken from 'stemDiameter'")


str(NEON.struc.nonwoody.dat)
str(NEON.struc.nonwoody.DIAdat)

#Species checking
spp.check<-NEON.struc.nonwoody.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


############################################################
#Separating Files by Ecosystem Type - NOTE: Removing "pastureHay", and "NA"

#NEON - Vegetation Structure
# - Combined Woody Data
# - NEON.Woody.Plants.Combined.csv
NEON.struc.woody.forest.DIAdat<-NEON.struc.woody.DIAdat%>%
  filter(ecosystem == "deciduousForest" |
           ecosystem == "evergreenForest" |
           ecosystem == "mixedForest")%>%
  mutate(ecosystem = "Forests",
         std_id = paste0("forest_", "NEON_", site))

NEON.struc.woody.sme.DIAdat<-NEON.struc.woody.DIAdat%>%
  filter(ecosystem == "woodyWetlands" |
           ecosystem == "emergentHerbaceousWetlands")%>%
  mutate(ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "NEON_", site))

NEON.struc.woody.grassland.DIAdat<-NEON.struc.woody.DIAdat%>%
  filter(ecosystem == "shrubScrub" |
           ecosystem == "grasslandHerbaceous" |
           ecosystem == "sedgeHerbaceous" |
           ecosystem == "dwarfScrub")%>%
  mutate(ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "NEON__", site))

#NEON - Vegetation Structure
# - Combined NonWoody Data
# - NEON.NonWoody.Plants.Combined.csv
NEON.struc.nonwoody.forest.DIAdat<-NEON.struc.nonwoody.DIAdat%>%
  filter(ecosystem == "deciduousForest" |
           ecosystem == "evergreenForest" |
           ecosystem == "mixedForest")%>%
  mutate(ecosystem = "Forests",
         std_id = paste0("forest_", "NEON_", site))

NEON.struc.nonwoody.sme.DIAdat<-NEON.struc.nonwoody.DIAdat%>%
  filter(ecosystem == "woodyWetlands" |
           ecosystem == "emergentHerbaceousWetlands")%>%
  mutate(ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "NEON_", site))

NEON.struc.nonwoody.grassland.DIAdat<-NEON.struc.nonwoody.DIAdat%>%
  filter(ecosystem == "shrubScrub" |
           ecosystem == "grasslandHerbaceous" |
           ecosystem == "sedgeHerbaceous" |
           ecosystem == "dwarfScrub")%>%
  mutate(ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "NEON_", site))


############################################################
#Combine Datasets

NEON.struc.all.DIAdat<-bind_rows(
  NEON.struc.woody.forest.DIAdat,
  NEON.struc.woody.sme.DIAdat,
  NEON.struc.woody.grassland.DIAdat,
  NEON.struc.nonwoody.forest.DIAdat,
  NEON.struc.nonwoody.sme.DIAdat,
  NEON.struc.nonwoody.grassland.DIAdat

)

gps.check<-NEON.struc.all.DIAdat[,c("std_id", "latitude", "longitude")]%>%
  distinct()


duplicates<- gps.check %>%
  mutate(duplicate = duplicated(paste(std_id)) | duplicated(paste(std_id), fromLast = TRUE))

str(NEON.struc.all.DIAdat)


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(NEON.struc.all.DIAdat, file="Diameter.Species.Indv.Multi.NEON.Veg.Structure.csv", row.names = F)
############################################################
