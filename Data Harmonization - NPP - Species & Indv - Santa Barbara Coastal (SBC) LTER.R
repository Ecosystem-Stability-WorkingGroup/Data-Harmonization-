############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#SBC Site Coordinates
SBC.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Santa Barbara Coastal LTER/SBC_LTER_Site_Coords.csv")
str(SBC.coords)

#Santa Barbara Coastal (SBC) LTER - Ongoing Time-Series of Net Primary Production of Giant Kelp
# - Macrocystis pyrifera net primary production and growth with SE_20240325.csv
SBC.kelp.npp.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Santa Barbara Coastal LTER/Macrocystis pyrifera net primary production and growth with SE_20240325.csv")
str(SBC.kelp.npp.dat)

############################################################
#Formatting Data

#Santa Barbara Coastal (SBC) LTER - Ongoing Time-Series of Net Primary Production of Giant Kelp
# - Macrocystis pyrifera net primary production and growth with SE_20240325.csv
SBC.kelp.npp.nppdat<-SBC.kelp.npp.dat%>%
  left_join(SBC.coords, by=c("Site" = "Site"))%>%
  mutate(site = Site, 
         Parameter = "NPP",
         data = "SBC LTER - Kelp NPP",
         Taxa = "Kelp",
         Specificity = "Species",
         ecosystem = "Kelp",
         std_id = paste0("kelp_", "SBC.LTER_", site),
         plot = Season,
         transect = 999,
         #date = lubridate::parse_date_time(XXXX, orders = c("mdy", "dmy", "ymd")),
         year = Year,
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         spp_full_name = "Macrocystis pyrifera",
         tree_id = "species_level_data",
         stem_id = "species_level_data",
         npp = NPP_dry*1000,
         npp_units = "g per m2 per day",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, npp, npp_units)%>%
  filter(!is.na(npp) & npp>=0)%>%
  mutate(notes = "plot marks 'Season', aggregation at site level")



str(SBC.kelp.npp.dat)
str(SBC.kelp.npp.nppdat)

#Species checking
spp.check<-SBC.kelp.npp.nppdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for aggregation levels
check1<-SBC.kelp.npp.nppdat%>%
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
write.csv(SBC.kelp.npp.nppdat, file="NPP.Species.Indv.Kelp.SCB.LTER.csv", row.names = F)
############################################################
