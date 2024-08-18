############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#Niwot Ridge (NR) LTER - Plant Species Composition Data for Saddle Grid
# - saddptqd.hh.data.csv
NR.spp.comp.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/NR LTER/saddptqd.hh.data.csv")
str(NR.spp.comp.dat)


############################################################
#Formatting Data

#Niwot Ridge (NR) LTER - Plant Species Composition Data for Saddle Grid
# - saddptqd.hh.data.csv
NR.spp.comp.pcdat<-NR.spp.comp.dat%>%
  mutate(site = "Saddle Grid", 
         Parameter = "Percent Cover",
         data = "NR LTER - Plant Species Composition",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "NR.LTER_", site),
         plot = plot,
         transect = 999,
         year = year,
         month = 999,
         day = 999,
         spp_full_name = USDA_name,
         latitude = (40.058564 + 40.0543984)/2,
         longitude =  (-105.5935122 + -105.5867879)/2)%>%
  group_by(site, plot, year)%>%
  mutate(points = length(unique(point)))%>%
  ungroup()%>%
  group_by(site, plot, year, spp_full_name)%>%
  mutate(spp.points = length(unique(point)))%>%
  ungroup()%>%
  mutate(percent_cover = (spp.points/points)*100)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  unique()%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Bare ground" | 
                             spp_full_name == "Elk scat" |
                             spp_full_name == "Forb (herbaceous, not grass nor grasslike)" |
                             spp_full_name == "Hole" |
                             spp_full_name == "Lichen" |
                             spp_full_name == "Litter" |
                             spp_full_name == "Moss" |
                             spp_full_name == "Rock, fragments" |
                             spp_full_name == "Scat" |
                             spp_full_name == "Unknown" |
                             spp_full_name == "Unknown composite" |
                             spp_full_name == "Graminoid (grass or grasslike)" |
                             spp_full_name == "Unknown soil crust" ~ "998" ,
                             TRUE ~ genus),
         species = case_when(spp_full_name == "Bare ground" | 
                             spp_full_name == "Elk scat" |
                             spp_full_name == "Forb (herbaceous, not grass nor grasslike)" |
                             spp_full_name == "Hole" |
                             spp_full_name == "Lichen" |
                             spp_full_name == "Litter" |
                             spp_full_name == "Moss" |
                             spp_full_name == "Rock, fragments" |
                             spp_full_name == "Scat" |
                             spp_full_name == "Unknown" |
                             spp_full_name == "Unknown composite" |
                             spp_full_name == "Graminoid (grass or grasslike)" |
                             spp_full_name == "Unknown soil crust" ~ "998" ,
                           TRUE ~ species),
         genus = case_when(spp_full_name == "Unknown Carex spp 1" | 
                             spp_full_name == "Unknown Carex spp 8" |
                             spp_full_name == "Unknown Carex spp 9" ~ "Carex" ,
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Unknown Carex spp 1" | 
                             spp_full_name == "Unknown Carex spp 8" |
                             spp_full_name == "Unknown Carex spp 9" |
                             species == "sp." ~ "998",
                           TRUE ~ species),
         species = ifelse(is.na(species), "998", species))%>%
  mutate(notes = "aggregation at site/plot/y/m/d; full coverage so minimum total % cover = 100")


str(NR.spp.comp.dat)
str(NR.spp.comp.pcdat)

spp.check<-NR.spp.comp.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

#Backcheck for percent cover aggregation levels
check1<-NR.spp.comp.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()




############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(NR.spp.comp.pcdat, file="Percent.Cover.Species.GrassSav.NR.LTER.SppComp.csv", row.names = F)
############################################################
