############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#NEON - Vegetation Structure
# - Combined Woody Data
# - NEON.Woody.Plants.Combined.csv
NEON.struc.woody.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Multiple/NEON/NEON_struct-plant/NEON.Woody.Plants.Combined.csv")
str(NEON.struc.woody.dat)

#NEON - Vegetation Structure
# - Combined NonWoody Data
# - NEON.NonWoody.Plants.Combined.csv
NEON.struc.nonwoody.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Multiple/NEON/NEON_struct-plant/NEON.NonWoody.Plants.Combined.csv")
str(NEON.struc.nonwoody.dat)

#NEON - Vegetation Structure
# - Shrub Data
# - NEON.Shrub.Plants.Combined.csv
NEON.struc.shrub.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Multiple/NEON/NEON_struct-plant/NEON.Shrub.Plants.Combined.csv")
str(NEON.struc.shrub.dat)

#NEON - Unified Coordinates
NEON.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Multiple/NEON/NEON.Unified.Coordinates.RYF.csv")
str(NEON.coords)


############################################################
#Formatting Data

#NEON - Vegetation Structure
# - Combined Woody Data
# - NEON.Woody.Plants.Combined.csv
NEON.struc.woody.ADdat<-NEON.struc.woody.dat%>%
  filter(!is.na(plotID.y))%>%
  mutate(site = plotID.y, 
         Parameter = "Abundance, Diameter, Height",
         data = "NEON - Vegetation Structure - Woody",
         Taxa = "Vegetation",
         Specificity = "Individuals",
         ecosystem = nlcdClass,
         std_id = paste0("", "_", site),
         plot = subplotID,
         transect = 999,
         date = lubridate::parse_date_time(date.y, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = scientificName,
         dens_units = "# per m2",
         plot.size = str_extract(subplotID, "(?<=_)[^_]+(?=_)"),
         plot.size = case_when(is.na(plot.size) ~ str_extract(subplotID, "(?<=_).*"),
                               TRUE ~ plot.size),
         plot.size = case_when(plot.size == "unknown" ~ "0",
                               TRUE ~ plot.size),
         plot.size = as.numeric(plot.size))%>%
  left_join(NEON.coords, by = c("site" = "plotID"))%>%
  mutate(latitude = Latitude.y,
         longitude = Longitude.y)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  group_by(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day, spp_full_name)%>%
    mutate(abundance = n(),
           density = abundance/plot.size)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
    distinct()%>%
  mutate(density = case_when(density == Inf ~ -9999,
                             TRUE ~ density),
         dens_units = case_when(density == -9999 ~ "no density measure",
                                TRUE ~ dens_units))%>%
  mutate(species = case_when(species == "sp." ~ "998", 
                             TRUE ~ species))%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
mutate(notes = "plot marks 'subplotID' - middle value represents plot size. Aggregation at site/plot/y/m/d; Be careful with vars. some genus/species may not be unique; Some sites do not have plot information - abunance only '-9999'")

str(NEON.struc.woody.dat)
str(NEON.struc.woody.ADdat)

#Species checking
spp.check<-NEON.struc.woody.ADdat[,c("spp_full_name", "genus", "species")]%>%
  distinct()

spp.check2<-NEON.struc.woody.ADdat%>%
  filter(genus == "Prunus" & species == "pensylvanica")

sizecheck<-NEON.struc.woody.ADdat%>%
  select(subplotID, plot.size)%>%
  distinct()

#Backcheck for aggregation levels
check1<-NEON.struc.woody.ADdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(abundance))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(abundance))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(abundance))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

check2<-NEON.struc.woody.ADdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(density))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(density))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(density))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()


#NEON - Vegetation Structure
# - Combined NonWoody Data
# - NEON.NonWoody.Plants.Combined.csv
NEON.struc.nonwoody.ADdat<-NEON.struc.nonwoody.dat%>%
  mutate(site = plotID.y, 
         Parameter = "Abundance, Diameter, Height",
         data = "NEON - Vegetation Structure - Non-Woody",
         Taxa = "Vegetation",
         Specificity = "Individuals",
         ecosystem = nlcdClass,
         std_id = paste0("", "_", site),
         plot = subplotID,
         transect = 999,
         date = lubridate::parse_date_time(date.y, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = scientificName.x,
         dens_units = "# per m2",
         plot.size = str_extract(subplotID, "(?<=_)[^_]+(?=_)"),
         plot.size = case_when(is.na(plot.size) ~ str_extract(subplotID, "(?<=_).*"),
                               TRUE ~ plot.size),
         plot.size = case_when(plot.size == "unknown" ~ "0",
                               TRUE ~ plot.size),
         plot.size = as.numeric(plot.size))%>%
  left_join(NEON.coords, by = c("site" = "plotID"))%>%
  mutate(latitude = Latitude.y,
         longitude = Longitude.y)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  group_by(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day, spp_full_name)%>%
  mutate(abundance = n(),
         density = abundance/plot.size)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  distinct()%>%
  filter(!is.na(site))%>%
  mutate(density = case_when(density == Inf ~ -9999,
                             TRUE ~ density),
         dens_units = case_when(density == -9999 ~ "no density measure",
                                TRUE ~ dens_units))%>%
  mutate(species = case_when(species == "sp." ~ "998", 
                             TRUE ~ species))%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
  mutate(notes = "plot marks 'subplotID' - middle value represents plot size. Aggregation at site/plot/y/m/d; Be careful with vars. some genus/species may not be unique; Some sites do not have plot information - abunance only '-9999'")


str(NEON.struc.nonwoody.dat)
str(NEON.struc.nonwoody.ADdat)

spp.check<-NEON.struc.nonwoody.ADdat[,c("spp_full_name", "genus", "species")]%>%
  distinct()

check1<-NEON.struc.nonwoody.ADdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(abundance))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(abundance))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(abundance))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()


#NEON - Vegetation Structure
# - Shrub Data
# - NEON.Shrub.Plants.Combined.csv
NEON.struc.shrub.ADdat<-NEON.struc.shrub.dat%>%
  mutate(site = plotID, 
         Parameter = "Abundance, Area, Height",
         data = "NEON - Vegetation Structure - Shrubs",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = nlcdClass,
         std_id = paste0("", "_", site),
         plot = subplotID,
         transect = 999,
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = scientificName,
         dens_units = "# per m2",
         plot.size = str_extract(subplotID, "(?<=_)[^_]+(?=_)"),
         plot.size = case_when(is.na(plot.size) ~ str_extract(subplotID, "(?<=_).*"),
                               TRUE ~ plot.size),
         plot.size = case_when(plot.size == "unknown" ~ "0",
                               TRUE ~ plot.size),
         plot.size = as.numeric(plot.size))%>%
  left_join(NEON.coords, by = c("site" = "plotID"))%>%
  mutate(latitude = Latitude.y,
         longitude = Longitude.y)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  group_by(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day, spp_full_name)%>%
  mutate(abundance = n(),
         density = abundance/plot.size)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  distinct()%>%
  filter(!is.na(site))%>%
  mutate(density = case_when(density == Inf ~ -9999,
                             TRUE ~ density),
         dens_units = case_when(density == -9999 ~ "no density measure",
                                TRUE ~ dens_units))%>%
  mutate(species = case_when(species == "sp." ~ "998", 
                             TRUE ~ species))%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
  mutate(notes = "plot marks 'subplotID' - middle value represents plot size. Aggregation at site/plot/y/m/d; Be careful with vars. some genus/species may not be unique; Some sites do not have plot information - abunance only '-9999'")


str(NEON.struc.shrub.dat)
str(NEON.struc.shrub.ADdat)

spp.check<-NEON.struc.shrub.ADdat[,c("spp_full_name", "genus", "species")]%>%
  distinct()

check1<-NEON.struc.shrub.ADdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(abundance))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(abundance))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(abundance))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

############################################################
#Separating Files by Ecosystem Type - NOTE: Removing "pastureHay", and "NA"

#NEON - Vegetation Structure
# - Combined Woody Data
# - NEON.Woody.Plants.Combined.csv
NEON.struc.woody.forest.ADdat<-NEON.struc.woody.ADdat%>%
  filter(ecosystem == "deciduousForest" |
           ecosystem == "evergreenForest" |
           ecosystem == "mixedForest")%>%
  mutate(ecosystem = "Forests",
         std_id = paste0("forest_", "NEON_", site))

NEON.struc.woody.sme.ADdat<-NEON.struc.woody.ADdat%>%
  filter(ecosystem == "woodyWetlands" |
           ecosystem == "emergentHerbaceousWetlands")%>%
  mutate(ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "NEON_", site))

NEON.struc.woody.grassland.ADdat<-NEON.struc.woody.ADdat%>%
  filter(ecosystem == "shrubScrub" |
           ecosystem == "grasslandHerbaceous" |
           ecosystem == "sedgeHerbaceous" |
           ecosystem == "dwarfScrub")%>%
  mutate(ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "NEON__", site))

#NEON - Vegetation Structure
# - Combined NonWoody Data
# - NEON.NonWoody.Plants.Combined.csv
NEON.struc.nonwoody.forest.ADdat<-NEON.struc.nonwoody.ADdat%>%
  filter(ecosystem == "deciduousForest" |
           ecosystem == "evergreenForest" |
           ecosystem == "mixedForest")%>%
  mutate(ecosystem = "Forests",
         std_id = paste0("forest_", "NEON_", site))

NEON.struc.nonwoody.sme.ADdat<-NEON.struc.nonwoody.ADdat%>%
  filter(ecosystem == "woodyWetlands" |
           ecosystem == "emergentHerbaceousWetlands")%>%
  mutate(ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "NEON_", site))

NEON.struc.nonwoody.grassland.ADdat<-NEON.struc.nonwoody.ADdat%>%
  filter(ecosystem == "shrubScrub" |
           ecosystem == "grasslandHerbaceous" |
           ecosystem == "sedgeHerbaceous" |
           ecosystem == "dwarfScrub")%>%
  mutate(ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "NEON_", site))

#NEON - Vegetation Structure
# - Shrub Data
# - NEON.Shrub.Plants.Combined.csv
NEON.struc.shrub.forest.ADdat<-NEON.struc.shrub.ADdat%>%
  filter(ecosystem == "deciduousForest" |
           ecosystem == "evergreenForest" |
           ecosystem == "mixedForest")%>%
  mutate(ecosystem = "Forests",
         std_id = paste0("forest_", "NEON_", site))

#NEON.struc.shrub.sme.ADdat<-NEON.struc.shrub.ADdat%>%  #None in Dataset
#  filter(ecosystem == "woodyWetlands" |
#           ecosystem == "emergentHerbaceousWetlands")%>%
#  mutate(ecosystem = "Salt Marsh & Estuary",
#         std_id = paste0("", "_", site))

NEON.struc.shrub.grassland.ADdat<-NEON.struc.shrub.ADdat%>% 
  filter(ecosystem == "shrubScrub" |
           ecosystem == "grasslandHerbaceous" |
           ecosystem == "sedgeHerbaceous" |
           ecosystem == "dwarfScrub")%>%
  mutate(ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "NEON_", site))


############################################################
#Combine Datasets

NEON.struc.all.ADdat<-bind_rows(
  NEON.struc.woody.forest.ADdat,
  NEON.struc.woody.sme.ADdat,
  NEON.struc.woody.grassland.ADdat,
  NEON.struc.nonwoody.forest.ADdat,
  NEON.struc.nonwoody.sme.ADdat,
  NEON.struc.nonwoody.grassland.ADdat,
  NEON.struc.shrub.forest.ADdat,
  NEON.struc.shrub.grassland.ADdat
)

gps.check<-NEON.struc.all.ADdat[,c("std_id", "latitude", "longitude")]%>%
  distinct()


duplicates<- gps.check %>%
  mutate(duplicate = duplicated(paste(std_id)) | duplicated(paste(std_id), fromLast = TRUE))

str(NEON.struc.all.ADdat)


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(NEON.struc.all.ADdat, file="Biomass.Species.Indv.Multi.NEON.Veg.Structure.csv", row.names = F)
############################################################