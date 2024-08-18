############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#Sevilleta (SEV) LTER - Site Coordinates
SEV.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/SEV LTER/SEV.LTER.Coordinates.csv")
str(SEV.coords)

#Sevilleta (SEV) LTER - Fall Season Aboveground NPP in the Monsoon Rainfall Manipulation Experiment (MRME)
# - rfbrown-mrme-anpp-20231011.csv
SEV.monsoon.npp.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/SEV LTER/rfbrown-mrme-anpp-20231011.csv")
str(SEV.monsoon.npp.dat)


############################################################
#Trimming to Control Sites  where Necessary

#Sevilleta (SEV) LTER - Fall Season Aboveground NPP in the Monsoon Rainfall Manipulation Experiment (MRME)
# - rfbrown-mrme-anpp-20231011.csv
SEV.monsoon.npp.dat<-SEV.monsoon.npp.dat%>%
  filter(h2o == "A" & fert != "F")

############################################################
#Formatting Data

#Sevilleta (SEV) LTER - Fall Season Aboveground NPP in the Monsoon Rainfall Manipulation Experiment (MRME)
# - rfbrown-mrme-anpp-20231011.csv
SEV.monsoon.npp.nppdat<-SEV.monsoon.npp.dat%>%
  mutate(site = "MRME")%>%
  left_join(SEV.coords, by = c("site" = "Site"))%>%
  mutate(site = "SEV.Monsoon.Control", 
         Parameter = "NPP",
         data = "SEV LTER - Monsoon NPP",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "SEV.LTER_", site),
         plot = plot,
         transect = paste0(subplot, "_", quad),
         #date = lubridate::parse_date_time(XXXX, orders = c("mdy", "dmy", "ymd")),
         year = year,
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         spp_full_name = paste0(genus, " ", species),
         tree_id = "species_level_data",
         stem_id = "species_level_data",
         npp = npp,
         npp_units = "g per m2 - time unclear, potentially annual", 
         latitude = Latitude,
         longitude = Longitude)%>%
  mutate(spp_full_name = gsub(" NA", "", spp_full_name),
         species = case_when(is.na(species) ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, npp,  npp_units)%>%
  filter(!is.na(npp))%>%
  mutate(notes = "plot marks 'plot', transect marks 'subplot/quad'")

  
str(SEV.monsoon.npp.dat)
str(SEV.monsoon.npp.nppdat)

#Species checking
spp.check<-SEV.monsoon.npp.nppdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-SEV.monsoon.npp.nppdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(npp))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(npp))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(npp))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - NPP - Species & Individuals")
#Save Files
write.csv(SEV.monsoon.npp.nppdat, file="NPP.Species.Indv.GrassSav.SEV.LTER.csv", row.names = F)
############################################################
