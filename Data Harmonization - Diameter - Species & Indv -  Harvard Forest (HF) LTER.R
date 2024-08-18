############################################################
#Load Packages
library(tidyverse)


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

#Harvard Forest (HF) LTER - Tree Growth and Above-Ground Biomass at Harvard Forest HEM and LPH Towers Since 2001
# - Hemlock Sites 
# - hf149-01-hem-dendro.csv
HF.Hem.tree.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HF LTER/hf149-01-hem-dendro.csv")
str(HF.Hem.tree.dat)
#Name Conversions - NEED TO VALIDATE GPS POINTS, MAKING ASSUMPTIONS HERE***
HF.Hem.name.conv<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HF LTER/Hemlock.Site.Name.Coversions.for.Mapping.csv")

hf.spp.codes<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HF LTER/hf106-01-species-codes.csv")
str(hf.spp.codes)
############################################################

#Harvard Forest (HF) LTER - Biomass Inventories at Harvard Forest EMS Tower Since 1993 - Tree Summary
# - hf069-09-ems-trees.csv
HF.EMS.tree.DIAdat<-HF.EMS.tree.dat%>%
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
         tree_id = as.character(plottag),
         stem_id = as.character("no_stem_id"),
         diameter = dbh_cm,
         diam_units = "cm",
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
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%    
  mutate(notes = "plot marks 'plot' (but also included in 'site' already); transect duplicates plottag to match with biomass data")


str(HF.EMS.tree.dat)
str(HF.EMS.tree.DIAdat)

#Species checking
spp.check<-HF.EMS.tree.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()
############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(HF.EMS.tree.DIAdat, file="Diameter.Species.Indv.Forest.HF.LTER.EMS.Trees.csv", row.names = F)
############################################################

#Harvard Forest (HF) LTER - Tree Growth and Above-Ground Biomass at Harvard Forest HEM and LPH Towers Since 2001
# - Hemlock Sites 
# - hf149-01-hem-dendro.csv
HF.Hem.tree.DIAdat<-HF.Hem.tree.dat%>%
  left_join(HF.Hem.name.conv, by=c("plot" = "Original"))%>%  #Converting Names - NEED TO CROSSCHECK
  mutate(Site = paste("Hem", New),
         Site = gsub("_", "-", Site))%>%    #Updating Plot Names to Match Coord Data *NOTE - NEED TO CHECK NAME CONVERSION
  left_join(HF.plot.coords[,c("NAME", "Latitude", "Longitude")], by = c("Site" = "NAME"))%>%
  mutate(site = Site,
         Parameter = "DBH",
         data = "HF LTER - Hemlock Tower Trees",
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
         tree_id = as.character(tag),
         stem_id = as.character("no_stem_id"),
         diameter = dbh_i + (caliper_mm*0.1),
         diam_units = "cm",
         latitude = Latitude,
         longitude =  Longitude)%>%
left_join(hf.spp.codes%>%
            select(code, name_Haines_2011), by = c("species_code" = "code"))%>%
  mutate(spp_full_name = name_Haines_2011)%>%
  mutate(spp_full_name = case_when(species_code == "PIRU" ~ "Picea rubens",
                                    species_code == "Picea sp." ~ "Picea sp.",
                                    TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%    
  mutate(notes = "plot marks 'plot'")


str(HF.Hem.tree.dat)
str(HF.Hem.tree.DIAdat)

#Species checking
spp.check<-HF.Hem.tree.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(HF.Hem.tree.DIAdat, file="Diameter.Species.Indv.Forest.HF.LTER.HEM.Trees.csv", row.names = F)
############################################################
