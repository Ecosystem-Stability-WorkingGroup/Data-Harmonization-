############################################################
#Load Packages
library(tidyverse)

###########################################################
#Automatic CSV Importing

#Jornada Basin (JB) LTER - Aboveground Net Primary Production
# - JRN011001_NPP_quadrat_estimates_SppSiteSeas.csv
JB.ag.npp.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/JB LTER/JRN011001_NPP_quadrat_estimates_SppSiteSeas.csv")
str(JB.ag.npp.dat)

#USDA Species Codes
JB.spp<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/USDA.clean.species.codes.csv")

############################################################
#Formatting Data

#Jornada Basin (JB) LTER - Aboveground Net Primary Production
# - JRN011001_NPP_quadrat_estimates_SppSiteSeas.csv
JB.ag.npp.biodat<-JB.ag.npp.dat%>%
  mutate(xsite = site,
         site = "JB NPP (15 sites)", 
         Parameter = "NPP",
         data = "JB LTER - Aboveground NPP",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "JB.LTER_", site),
         plot = quad,
         transect = paste0(xsite, "_", zone),
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         biomass = biomass,
         bmass_units = "g per m2",
         latitude = (32.669000 + 32.488000)/2,
         longitude =  (-106.865000 + -106.713000)/2)%>%
  left_join(JB.spp, by = c("USDA_code" = "USDA_code"))%>%
  mutate(spp_full_name = species_name)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(spp_full_name = case_when(is.na(spp_full_name) ~ "998",
                                   TRUE ~ spp_full_name),
         genus = case_when(is.na(genus) ~ "998",
                           TRUE ~ genus),
         species = case_when(is.na(species) |
                               species == "Ruiz" |
                               species == "R." |
                               species == "P." |
                               species == "Nutt." |
                               species == "Mill." |
                               species == "Lehm." |
                               species == "Lag." |
                               species == "L." |
                               species == "Cav." |
                               species == "Adans." |
                               species == "A." |
                               species == "(Nutt." ~ "998",
                             TRUE ~ species))%>%
  group_by(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, bmass_units)%>%
  summarise(biomass = sum(biomass, na.rm=T))%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  mutate(notes = "plot marks 'quad'; transect marks (site - within data)/zone; species codes incomplete: missing ~9% of species (998s in spp_full_name)")

str(JB.ag.npp.dat)
str(JB.ag.npp.biodat)

#Species checking
spp.check<-JB.ag.npp.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

#Backcheck for aggregation levels
check1<-JB.ag.npp.biodat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(biomass))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()


JB.ag.npp.biodat<-as.data.frame(JB.ag.npp.biodat)
############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Biomass - Species & Individuals")
#Save Files
write.csv(JB.ag.npp.biodat, file="Biomass.Species.Indv.GrassSav.JBLTER.NPP.csv", row.names = F)
############################################################


  
  