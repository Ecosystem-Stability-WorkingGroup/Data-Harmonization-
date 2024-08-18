############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#Florida Coastal Everglades (FCE) LTER -Site Coordinates
# - FCE_LTER_Seagrass_Coordinates.csv
FCE.seagrass.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Seagrass/FCE LTER/FCE_LTER_Seagrass_Coordinates.csv")
str(FCE.seagrass.coords)

#Calculate Center of Bounding Boxes
FCE.seagrass.coords$Latitude <- (FCE.seagrass.coords$Northern + FCE.seagrass.coords$Southern) / 2
FCE.seagrass.coords$Longitude <- (FCE.seagrass.coords$Western + FCE.seagrass.coords$Eastern) / 2

#Florida Coastal Everglades (FCE) LTER - Florida Bay Braun Blanquet - Seagrass Frequency, Abundance, and Density
# - LT_PP_Fourqurean_001.csv
FCE.seagrass.den.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Seagrass/FCE LTER/LT_PP_Fourqurean_001.csv")
str(FCE.seagrass.den.dat)

#Florida Coastal Everglades (FCE) LTER - Florida Bay Productivity
# - LT_PP_Fourqurean_004.csv
FCE.seagrass.npp.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Seagrass/FCE LTER/LT_PP_Fourqurean_004.csv")
str(FCE.seagrass.npp.dat)

############################################################
#Formatting Data

#Florida Coastal Everglades (FCE) LTER - Florida Bay Braun Blanquet - Seagrass Frequency, Abundance, and Density
# - LT_PP_Fourqurean_001.csv
FCE.seagrass.den.abundat<-FCE.seagrass.den.dat%>%
  left_join(FCE.seagrass.coords, by = c("SITENAME" = "Site"))%>%
  mutate(site = SITENAME, 
         Parameter = "Density",
         data = "FCE LTER - Seagrass Density/Abundance/Freq",
         Taxa = "Seagrass (3 spp)",
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
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, Thalassia_Abundance, Syringodium_Abundance, Halodule_Abundance, TotalCalcareousGreen_Abundance)%>%
  rename("Thalassia testudinum" = "Thalassia_Abundance",
         "Syringodium filiforme" = "Syringodium_Abundance",
         "Halodule wrightii" = "Halodule_Abundance",
         "Calcereous Green Algae" = "TotalCalcareousGreen_Abundance")%>%
  pivot_longer(cols = 12:ncol(.), 
               names_to = "spp_full_name", 
               values_to = "abundance")%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance)

str(FCE.seagrass.den.dat)
str(FCE.seagrass.den.abundat)


FCE.seagrass.den.dendat<-FCE.seagrass.den.dat%>%
  left_join(FCE.seagrass.coords, by = c("SITENAME" = "Site"))%>%
  mutate(site = SITENAME, 
         Parameter = "Density",
         data = "FCE LTER - Seagrass Density/Abundance/Freq",
         Taxa = "Seagrass (3 spp)",
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
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, Thalassia_Density, Syringodium_Density, Halodule_Density, TotalCalcareousGreen_Density)%>%
  rename("Thalassia testudinum" = "Thalassia_Density",
         "Syringodium filiforme" = "Syringodium_Density",
         "Halodule wrightii" = "Halodule_Density",
         "Calcereous Green Algae" = "TotalCalcareousGreen_Density")%>%
  pivot_longer(cols = 12:ncol(.), 
               names_to = "spp_full_name", 
               values_to = "density")%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(dens_units = "unclear")%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, density, dens_units)
  


FCE.seagrass.den.ADdat<-bind_cols(FCE.seagrass.den.abundat, FCE.seagrass.den.dendat%>%select(density, dens_units))%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
  mutate(genus = case_when(spp_full_name == "Calcereous Green Algae" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Calcereous Green Algae" ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "aggregation at site level")

spp.check<-FCE.seagrass.den.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(FCE.seagrass.den.ADdat, file="Biomass.Species.Indv.Seagrass.FCE.Braun.csv", row.names = F)
############################################################

#Florida Coastal Everglades (FCE) LTER - Florida Bay Productivity
# - LT_PP_Fourqurean_004.csv
FCE.seagrass.npp.ADdat<-FCE.seagrass.npp.dat%>%
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
         abundance = -9999,
         density = SSDensity,
         dens_units = "short shoot per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
  mutate(notes = "aggregation at site level, Thalassia testudinum only")

str(FCE.seagrass.npp.dat)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(FCE.seagrass.npp.ADdat, file="Biomass.Species.Indv.Seagrass.FCE.NPP.AD.csv", row.names = F)
############################################################