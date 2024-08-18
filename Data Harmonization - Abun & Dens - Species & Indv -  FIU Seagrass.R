############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing
FIU.Combined.Seagrass.Data<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Seagrass/FIU density timeseries/FIU.Combined.Seagrass.Data.csv")
str(FIU.Combined.Seagrass.Data)


############################################################
#Formatting Data

FIU.sgrass.ADdat<-FIU.Combined.Seagrass.Data%>%
  mutate(site = Site,
         std_id = paste0("sgrass_", "FIU.Dens_", site),
         ecosystem = "Seagrass",
         data = "FIU Density Timeseries", 
         plot = 999,
         transect = 999,
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         abundance = -9999,
         density = Value,
         dens_units = "unclear",
         latitude = Latitude,
         longitude = Longitude)%>%
  mutate(spp_full_name = case_when(Species == "Thalassia" ~ "Thalassia testudinum",
                                    Species == "Syringodium" ~ "Syringodium filiforme",
                                    Species == "Halodule" ~ "Halodule wrightii",
                                    Species == "Calc Green Total" ~ "Calcereous Green Algae"))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Calcereous Green Algae" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Calcereous Green Algae" ~ "998",
                           TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
mutate(notes = "aggregation at site level")


#Species checking
spp.check<-FIU.sgrass.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(FIU.sgrass.ADdat, file="AbunDens.Species.Indv.Seagrass.FIU.Density.csv", row.names = F)
############################################################
