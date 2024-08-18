############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#Florida Coastal Everglades (FCE) LTER -Site Coordinates
# - FCE_LTER_Seagrass_Coordinates.csv
FCE.seagrass.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Seagrass/FCE LTER/FCE_LTER_Seagrass_Coordinates.csv")
str(FCE.seagrass.coords)

#Calculate Center of Bounding Boxes
FCE.seagrass.coords$Latitude <- (FCE.seagrass.coords$Northern + FCE.seagrass.coords$Southern) / 2
FCE.seagrass.coords$Longitude <- (FCE.seagrass.coords$Western + FCE.seagrass.coords$Eastern) / 2

#Florida Coastal Everglades (FCE) LTER - Florida Bay Productivity
# - LT_PP_Fourqurean_004.csv
FCE.seagrass.npp.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Seagrass/FCE LTER/LT_PP_Fourqurean_004.csv")
str(FCE.seagrass.npp.dat)

############################################################
#Formatting Data

#Florida Coastal Everglades (FCE) LTER - Florida Bay Productivity
# - LT_PP_Fourqurean_004.csv
FCE.seagrass.npp.biodat<-FCE.seagrass.npp.dat%>%
  mutate(SITENAME = gsub("TS/Ph7a", "TS/Ph7", SITENAME))%>%    #Renaming site to match syntax of other datasets
  left_join(FCE.seagrass.coords, by = c("SITENAME" = "Site"))%>%
  mutate(site = SITENAME, 
         Parameter = "Density, Biomass, NPP",
         data = "FCE LTER - Seagrass NPP",
         Taxa = "Thalassia testudinum",
         Specificity = "Species",
         ecosystem = "Seagrass",
         std_id = paste0("sgrass_", "FCE.LTER_", site),
         plot = 999,
         transect = 999,
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = "Thalassia testudinum",
         biomass = StandingCrop,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  mutate(notes = "aggregation at site level, Thalassia testudinum only")


str(FCE.seagrass.npp.dat)
str(FCE.seagrass.npp.biodat)

#Species checking
spp.check<-FCE.seagrass.npp.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

#Backcheck for aggregation levels
check1<-FCE.seagrass.npp.biodat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(biomass))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Biomass - Species & Individuals")
#Save Files
write.csv(FCE.seagrass.npp.biodat, file="Biomass.Species.Indv.Seagrass.FCE.LTER.NPP.BMass.csv", row.names = F)
############################################################