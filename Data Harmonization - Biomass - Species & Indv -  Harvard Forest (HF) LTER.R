############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#Plot-Level GPS Coordinates
# - hf375-02-field-sites-plots.csv
HF.plot.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HF LTER/hf375-02-field-sites-plots.csv")
str(HF.plot.coords)

#Harvard Forest (HF) LTER - Biomass Inventories at Harvard Forest EMS Tower Since 1993 - Tree Summary
# - hf069-09-ems-trees.csv
HF.EMS.tree.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HF LTER/hf069-09-ems-trees.csv")
str(HF.EMS.tree.dat)

#Harvard Forest (HF) LTER - Biomass Inventories at Harvard Forest EMS Tower Since 1993 - Understory Summary
# - hf069-11-understory-trees.csv
HF.EMS.US.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HF LTER/hf069-11-understory-trees.csv")
str(HF.EMS.US.dat)



############################################################
#Formatting Data

#Harvard Forest (HF) LTER - Biomass Inventories at Harvard Forest EMS Tower Since 1993 - Tree Summary
# - hf069-09-ems-trees.csv
HF.EMS.tree.biodat<-HF.EMS.tree.dat%>%
  mutate(Site = paste("EMS", plot))%>%    #Updating Plot Names to Match Coord Data
  left_join(HF.plot.coords[,c("NAME", "Latitude", "Longitude")], by = c("Site" = "NAME"))%>%
  mutate(site = Site,
         Parameter = "DBH, Biomass",
         data = "HF LTER - EMS Tower Trees",
         Taxa = "Trees",
         Specificity = "Individuals",
         ecosystem = "Forests",
         std_id = paste0("forest_", "HF.LTER_", site),
         plot = plot,
         transect = plottag,
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         biomass = biomass_kgc,
         bmass_units = "individual - kg",
         latitude = Latitude,
         longitude =  Longitude)%>%
  mutate(spp_full_name = case_when(species == 	"yb"	~	"Betula alleghaniensis",
                                    species == 	"rm"	~	"Acer rubrum",
                                    species == 	"ro"	~	"Quercus rubrum",
                                    species == 	"bo"	~	"Quercus velutina",
                                    species == 	"beech"	~	"Fagus grandifolia",
                                    species == 	"sp"	~	"Pinus sylvestris",
                                    species == 	"cherry"	~	"Prunus sp.",
                                    species == 	"bb"	~	"Betula lenta",
                                    species == 	"ash"	~	"Fraxinus americana",
                                    species == 	"gb"	~	"Betula populifolia",
                                    species == 	"hem"	~	"Tsuga canadensis",
                                    species == 	"wb"	~	"Betula papyrifera",
                                    species == 	"ws"	~	"Picea glauca",
                                    species == 	"sm"	~	"Acer saccharum",
                                    species == 	"rp"	~	"Pinus resinosa",
                                    species == 	"chestnut"	~	"Castanea dentata",
                                    species == "wp" ~ "Pinus strobus",
                                    TRUE ~ "XXXX"))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  mutate(notes = "plot marks 'plot' (but also included in 'site' already); transect marks 'plottag' to match with diameter data - to aggregate up, plots have 10m radius")

str(HF.EMS.tree.dat)
str(HF.EMS.tree.biodat)

#Species checking
spp.check<-HF.EMS.tree.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

#Backcheck for percent cover aggregation levels
check1<-HF.EMS.tree.biodat%>%
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
write.csv(HF.EMS.tree.biodat, file="Biomass.Species.Indv.Forest.HF.LTER.EMS.Trees.csv", row.names = F)
############################################################

#Harvard Forest (HF) LTER - Biomass Inventories at Harvard Forest EMS Tower Since 1993 - Understory Summary
# - hf069-11-understory-trees.csv
HF.EMS.US.biodat<-HF.EMS.US.dat%>%
  mutate(Site = paste("EMS", plot))%>%    #Updating Plot Names to Match Coord Data
  left_join(HF.plot.coords[,c("NAME", "Latitude", "Longitude")], by = c("Site" = "NAME"))%>%
  filter(site == "ems")%>%
  mutate(site = Site,
         Parameter = "DBH, Biomass",
         data = "HF LTER - EMS Tower Understory Trees",
         Taxa = "Trees",
         Specificity = "Individuals",
         ecosystem = "Forests",
         std_id = paste0("forest_", "HF.LTER_", site),
         plot = plot,
         transect = 999,
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         biomass = biomass_kgc*nindivs,
         bmass_units = "species - kg",
         latitude = Latitude,
         longitude =  Longitude)%>%
  mutate(spp_full_name = case_when(species == 	"yb"	~	"Betula alleghaniensis",
                                   species == 	"rm"	~	"Acer rubrum",
                                   species == 	"ro"	~	"Quercus rubrum",
                                   species == 	"bo"	~	"Quercus velutina",
                                   species == 	"beech"	~	"Fagus grandifolia",
                                   species == 	"sp"	~	"Pinus sylvestris",
                                   species == 	"cherry"	~	"Prunus sp.",
                                   species == 	"bb"	~	"Betula lenta",
                                   species == 	"ash"	~	"Fraxinus americana",
                                   species == 	"gb"	~	"Betula populifolia",
                                   species == 	"hem"	~	"Tsuga canadensis",
                                   species == 	"wb"	~	"Betula papyrifera",
                                   species == 	"ws"	~	"Picea glauca",
                                   species == 	"sm"	~	"Acer saccharum",
                                   species == 	"rp"	~	"Pinus resinosa",
                                   species == 	"chestnut"	~	"Castanea dentata",
                                   species == "wp" ~ "Pinus strobus",
                                   species == "hbb" ~ "Vaccinium sp.",
                                   species == "nwr" ~ "Viburnum sp.",
                                   species == "spi" ~ "Spiraea sp.",
                                   species == "wh" ~ "Hamamelis virginiana",
                                   species == "ss" ~ "Rhus typhina",
                                   species == "pb" ~ "Betula papyrifera",
                                   species == "wo" ~ "Quercus alba",
                                   species == "osvi" ~ "Ostrya virginiana",
                                   TRUE ~ "998"))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             is.na(species) ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  group_by(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day, spp_full_name, genus, species,bmass_units)%>%
  summarise(biomass = sum(biomass))%>% #Biomass is calculated at species level here, rather than individuals
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  mutate(notes = "plot marks 'plot' (but also included in 'site' already) - to aggregate up, plots have 10m radius, species code missing 3 species")


str(HF.EMS.US.dat)
str(HF.EMS.US.biodat)

#Species checking
spp.check<-HF.EMS.US.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-HF.EMS.US.biodat%>%
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
write.csv(HF.EMS.US.biodat, file="Biomass.Species.Indv.Forest.HF.LTER.EMS.Understory.csv", row.names = F)
############################################################
