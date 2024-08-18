############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#Dataset Name 
# - CSV Name
ORNL.wbw.veg.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ORNL DAAC/NPP_WBW_819/data/WBW_veg_inventory_all_20100629.csv")
str(ORNL.wbw.veg.dat)


#Species Names
ORNL.spp<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ORNL DAAC/NPP_WBW_819/data/WBW_veg_species_2006.csv")
str(ORNL.spp)

############################################################
#Formatting Data

#Dataset Name 
# - CSV Name
ORNL.wbw.veg.DIAdat<-ORNL.wbw.veg.dat%>%
  filter(!STATUS %in% c("1", "2"))%>%  #Removing dead invididuals
  mutate(year = YEAR,
         site = "Walker Branch Watershed", 
         Parameter = "DBH",
         data = "ORNL DAAC - WBW Veg Inventory",
         Taxa = "Trees",
         Specificity = "Individuals",
         ecosystem = "Forests",
         std_id = paste0("forest_", "ORNL_WBW_", site),
         plot = PLOT,
         transect = 999,
         #date = lubridate::parse_date_time(XXXX, orders = c("mdy", "dmy", "ymd")),
         #year = year(date),
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         tree_id = as.character(TAG),
         stem_id = as.character("no_stem_id"),
         diameter = DBH_cm,
         diam_units = "cm",
         latitude = 35.90,
         longitude =  -84.30)%>%
  left_join(ORNL.spp, by = c("SPECIES"))%>%
  mutate(spp_full_name = Scientific_Name)%>%
  mutate(spp_full_name = case_when(spp_full_name == "Eastern Hemlock (Tsuga canadensis (L.) Carr.)" ~ "Tsuga canadensis",
                                   spp_full_name == "Pyrus Malus L." ~ "Pyrus malus",
                                   SPECIES == "Unknown" ~ "998",
                                   SPECIES == "Missing" ~ "998",
                                   TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(species = case_when(spp_full_name == "998" |
                               species == "spp" |
                               species == "spp." ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%    
  mutate(notes = "plot marks 'PLOT'")


str(ORNL.wbw.veg.dat)
str(ORNL.wbw.veg.DIAdat)

#Species checking
spp.check<-ORNL.wbw.veg.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(ORNL.wbw.veg.DIAdat, file="Diameter.Species.Indv.Forest.ORNL.WBW.csv", row.names = F)
############################################################
