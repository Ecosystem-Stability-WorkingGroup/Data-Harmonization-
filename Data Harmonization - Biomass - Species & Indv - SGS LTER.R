############################################################
#Load Packages
library(tidyverse)
library(lubridate)

###########################################################
#Automatic CSV Importing

#Shortgrass Steppe (SGS) LTER - 1983-2008 Annual Aboveground NPP, ARS Study Number 6 - anpp_data.csv
SGS.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/SGS LTER/anpp_data.csv")
str(SGS.dat)

############################################################
#Formatting Data

#Shortgrass Steppe (SGS) LTER - 1983-2008 Annual Aboveground NPP, ARS Study Number 6 - anpp_data.csv
SGS.biodat<-SGS.dat%>%
  mutate(site = site_name, 
         Parameter = "Biomass",
         data = "SGS LTER - Aboveground Biomass",
         Taxa = "Vegetation (~95 spp)",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "SGS.LTER_", site),
         plot = plot,
         transect = transect,
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = ScientificName,
         biomass = weight * 4,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude = case_when(
           site == "MIDSLOPE" ~ -104.730809,    #Fixing typo, no "." in longitude for MIDSLOPE site
           TRUE ~ Longitude))%>%
  mutate(spp_full_name = case_when(species == "PESP" ~ "Penstemon sp.",
                                   TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(spp_full_name = case_when(spp_full_name == "" |
                                     spp_full_name == "unknown" ~ "998",
                                   TRUE ~ spp_full_name),
         genus = case_when(spp_full_name == "998" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "998" |
                               species == "sp." |
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  mutate(notes = "plot marks 'plot', transect marks 'transect', aggregation likely site/plot/transect/y/m/d")



str(SGS.dat)
str(SGS.biodat)

#Species checking
spp.check<-SGS.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

#Backcheck for percent cover aggregation levels
check1<-SGS.biodat%>%
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
write.csv(SGS.biodat, file="Biomass.Species.Indv.GrassSav.SGS.LTER.csv", row.names = F)
############################################################