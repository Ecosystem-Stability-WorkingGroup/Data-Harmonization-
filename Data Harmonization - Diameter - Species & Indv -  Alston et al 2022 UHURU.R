############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#Alston et al. 2022: 12 years of data from the UHURU experiment
# _ Site Coordinates
# - PLOT_COORDINATES.csv
UHURU.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Savanna/Alston et al 2022/PLOT_COORDINATES.csv")
str(UHURU.coords)

#Finding Mean of Site_Blocks for Mapping
UHURU.coords<-UHURU.coords%>%
  mutate(Site2 = case_when(site =="SOUTH" ~ "S",
                           site == "CENTRAL" ~ "C",
                           site == "NORTH" ~ "N",
                           TRUE ~ site))%>%
  group_by(Site2, block)%>%
  summarise(Latitude = mean(dd_lat),
            Longitude = mean(dd_long))

#Alston et al. 2022: 12 years of data from the UHURU experiment
# _ Annual Tree Surveys and Measurements
# - TREE_SURVEYS_2009-2019.csv
UHURU.treesurv.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Savanna/Alston et al 2022/TREE_SURVEYS_2009-2019.csv")
str(UHURU.treesurv.dat)
#Trimming to Control Sites
UHURU.treesurv.dat<-UHURU.treesurv.dat%>%
  filter(treatment == "OPEN")

############################################################
#Formatting Data

#Alston et al. 2022: 12 years of data from the UHURU experiment
# _ Annual Tree Surveys and Measurements
# - TREE_SURVEYS_2009-2019.csv
UHURU.treesurv.DIAdat<-UHURU.treesurv.dat%>%
  left_join(UHURU.coords, by = c("site" = "Site2", "block" = "block"))%>%
  filter(dead != "Y")%>%
  mutate(site = plot, 
         Parameter = "Height, Diameter",
         data = "UHURU - Tree Surveys & Measurements",
         Taxa = "Trees",
         Specificity = "Individuals",
         ecosystem = "Savanna",
         std_id = paste0("grass.sav_", "UHURU_", site),
         plot = 999,
         transect = 999,
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = species,
         tree_id = as.character(tag_number),
         stem_id = as.character("no_stem_id"),
         diameter = circ_cm/pi,
         diam_units = "cm",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = "_", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%    
  mutate(notes = "")



str(UHURU.treesurv.dat)
str(UHURU.treesurv.DIAdat)

#Species checking
spp.check<-UHURU.treesurv.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()
############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(UHURU.treesurv.DIAdat, file="Diameter.Species.Indv.GrassSav.UHURU.TreeSurv.csv", row.names = F)
############################################################
