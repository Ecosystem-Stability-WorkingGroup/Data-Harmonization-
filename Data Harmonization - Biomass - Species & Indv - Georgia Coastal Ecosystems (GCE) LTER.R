############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#Georgia Coastal Ecosystems (GCE) LTER - Sapelo Island Long-Term Plant Biomass - PLT-GCES-1609_Observations_6_0.CSV
#Multi-step to remove blank and other superfluous rows
lines_to_skip <- c(1, 2, 4, 5)
data_lines <- readLines("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/GCE LTER/PLT-GCES-1609_Observations_6_0.CSV")
data_lines <- data_lines[-lines_to_skip]
GCE.sapelo.dat <- read.csv(text = data_lines, header = TRUE)

str(GCE.sapelo.dat)

#Georgia Coastal Ecosystems (GCE) LTER - Altamaha River Plant Transition Sites Long-Term Plant Biomass - PLT-GCES-1609c_Observations_4_0.CSV
#Multi-step to remove blank and other superfluous rows
lines_to_skip <- c(1, 2, 4, 5)
data_lines <- readLines("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/GCE LTER/PLT-GCES-1609c_Observations_4_0.CSV")
data_lines <- data_lines[-lines_to_skip]
GCE.ar.bio.dat <- read.csv(text = data_lines, header = TRUE)

str(GCE.ar.bio.dat)


############################################################
#Formatting Data

#Calculating Midpoints of GPS Plot Coordinates
GCE.sapelo.coords<-GCE.sapelo.dat%>%
  group_by(Location)%>%
  summarise(Site_Name = unique(Site_Name),
            lat = mean(Latitude, na.rm = T),   #Mean accounts for occasional typos in late decimals of gps points
            lon = mean(Longitude, na.rm = T))%>%
  ungroup()%>%
  group_by(Site_Name)%>%
  summarise(Latitude = mean(lat, na.rm = T),
            Longitude = mean(lon, na.rm = T))

############################################################
#Georgia Coastal Ecosystems (GCE) LTER - Sapelo Island Long-Term Plant Biomass - PLT-GCES-1609_Observations_6_0.CSV
GCE.sapelo.biodat<-GCE.sapelo.dat%>%
  left_join(GCE.sapelo.coords, by=c("Site_Name" = "Site_Name"))%>%
  mutate(site = Site_Name, 
         Parameter = "Biomass",
         data = "GCE LTER - Sapelo Isl. Plant Biomass",
         Taxa = "Vegetation (~10 spp)",
         Specificity = "Species",
         ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "GCE.LTER_", site),
         plot = Plot,
         transect = Location,
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         biomass = Plant_Biomass * (1/Quadrat_Area),
         bmass_units = "g per m2",
         latitude = Latitude.y,
         longitude = Longitude.y)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  mutate(species = case_when(species == "spp." ~ "998",
                             TRUE ~ species),
         species = str_to_lower(species))%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  mutate(notes = "plot marks 'Plot', transect marks 'Location', aggregation likely site/plot/transect/y/m/d")

str(GCE.sapelo.dat)
str(GCE.sapelo.biodat)

#Species checking
spp.check<-GCE.sapelo.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-GCE.sapelo.biodat%>%
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
write.csv(GCE.sapelo.biodat, file="Biomass.Species.Indv.SMEW.GCE.LTER.Sapelo.csv", row.names = F)
############################################################


############################################################
#Georgia Coastal Ecosystems (GCE) LTER - Altamaha River Plant Transition Sites Long-Term Plant Biomass - PLT-GCES-1609c_Observations_4_0.CSV
GCE.ar.bio.biodat<-GCE.ar.bio.dat%>%
  mutate(site = Site, 
         Parameter = "Biomass",
         data = "GCE LTER - Altamaha Riv. Plant Biomass",
         Taxa = "Vegetation (~6 spp)",
         Specificity = "Species",
         ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "GCE.LTER_", site),
         plot = Plot,
         transect = 999,
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         biomass = Plant_Biomass * (1/Quadrat_Area),
         bmass_units = "g per m2",
         latitude = case_when(
           Site == "SCSA" ~ 31.323028,                #Center of Polygons - See Metadata
           Site == "ZSC1" ~ 31.328344,
           Site == "ZSC2" ~ 31.340495
         ),
         longitude = case_when(
           Site == "SCSA" ~ -81.374012,                #Center of Polygons - See Metadata
           Site == "ZSC1" ~ -81.450646,
           Site == "ZSC2" ~ -81.453296
         ))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  mutate(species = case_when(species == "spp." ~ "998",
                             TRUE ~ species),
         species = str_to_lower(species))%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  mutate(notes = "plot marks 'Plot', transect marks 'Location', aggregation likely site/plot/transect/y/m/d")


str(GCE.ar.bio.dat)
str(GCE.ar.bio.biodat)

#Species checking
spp.check<-GCE.ar.bio.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-GCE.ar.bio.biodat%>%
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
write.csv(GCE.ar.bio.biodat, file="Biomass.Species.Indv.SMEW.GCE.LTER.Altamaha.csv", row.names = F)
############################################################

