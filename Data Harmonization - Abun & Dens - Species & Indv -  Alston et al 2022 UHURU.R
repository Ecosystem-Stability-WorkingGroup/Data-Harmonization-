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
# _ Annual Tree Census Summary
# - TREE_CENSUS_SUMMARY_2009-2019.csv
UHURU.treecens.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Savanna/Alston et al 2022/TREE_CENSUS_SUMMARY_2009-2019.csv")
str(UHURU.treecens.dat)
#Trimming to Control Sites
UHURU.treecens.dat<-UHURU.treecens.dat%>%
  filter(treatment == "OPEN")

#Alston et al. 2022: 12 years of data from the UHURU experiment
# _ Pinhit Vegetation Data
# - UNDERSTORY_PIN_2008-2019.csv
UHURU.pinhit.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Savanna/Alston et al 2022/UNDERSTORY_PIN_2008-2019.csv")
str(UHURU.pinhit.dat)
#Trimming "Notes" Columns and Rows
UHURU.pinhit.dat<-UHURU.pinhit.dat[-c(1:3), -1]
#Trimming to Control Sites
UHURU.pinhit.dat<-UHURU.pinhit.dat%>%
  filter(treatment == "OPEN")

############################################################
#Formatting Data

#Alston et al. 2022: 12 years of data from the UHURU experiment
# _ Annual Tree Census Summary
# - TREE_CENSUS_SUMMARY_2009-2019.csv
UHURU.treecens.ADdat<-UHURU.treecens.dat%>%
  left_join(UHURU.coords, by = c("site" = "Site2", "block" = "block"))%>%
  mutate(site = plot, 
         Parameter = "Abundance",
         data = "UHURU - Tree Census Data",
         Taxa = "Trees",
         Specificity = "Species",
         ecosystem = "Savanna",
         std_id = paste0("grass.sav_", "UHURU_", site),
         plot = 999,
         transect = 999,
         #date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year,
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         spp_full_name = species,
         abundance = total,
         density = total/10000,
         dens_units = "# per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = "_", 
    remove = FALSE)%>%
  mutate(species = case_when(species == "spp" ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
mutate(notes = "aggregation at site level")


str(UHURU.treecens.dat)
str(UHURU.treecens.ADdat)

#Species checking
spp.check<-UHURU.treecens.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(UHURU.treecens.ADdat, file="AbunDens.Species.Indv.UHURU.TreeCens.csv", row.names = F)
############################################################


#Alston et al. 2022: 12 years of data from the UHURU experiment
# _ Pinhit Vegetation Data
# - UNDERSTORY_PIN_2008-2019.csv
UHURU.pinhit.ADdat<-UHURU.pinhit.dat%>%
  left_join(UHURU.coords, by = c("site" = "Site2", "block" = "block"))%>%
  mutate(site = plot, 
         Parameter = "Abundance",
         data = "UHURU - Pinhit Vegetation Data",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Savanna",
         std_id = paste0("", "_", site),
         plot = 999,
         transect = 999,
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         dens_units = "",
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, 8:330)%>%
  mutate(across(12:ncol(.), as.numeric))%>%
  pivot_longer(cols = 12:ncol(.), 
               names_to = "spp_full_name", 
               values_to = "hits")%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = "_", 
    remove = FALSE)%>%
  mutate(species = case_when(spp_full_name == "unknown3B" |
                               spp_full_name == "unknownS3" |
                               spp_full_name == "Unknown" |
                               spp_full_name == "unknown_31" ~ "998",
                             TRUE ~ species))


str(UHURU.pinhit.dat)
str(UHURU.pinhit.ADdat)

#Species checking
spp.check<-UHURU.pinhit.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

hmm<-UHURU.pinhit.ADdat%>%
  group_by(site, plot, transect, year, month, day)%>%
  summarise(sum(hits, na.rm=T))
