############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#Hubbard Brook (HB) LTER - Forest Inventories of a Northern Hardwood Forest: Watershed 6
# - Multiple
HB.inv.65.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HB LTER/w6_1965veg.txt")
HB.inv.77.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HB LTER/w6_1977veg.txt")
HB.inv.82.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HB LTER/w6_1982veg.txt")
HB.inv.87.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HB LTER/w6_1987veg.txt")
HB.inv.92.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HB LTER/w6_1992veg.txt")
HB.inv.97.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HB LTER/w6_1997veg.txt")
HB.inv.02.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HB LTER/w6_2002veg.txt")
HB.inv.07.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HB LTER/w62007veg.txt")
HB.inv.12.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HB LTER/w62012veg.txt")
HB.inv.17.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/HB LTER/w6_2017_vegInventory.csv")


############################################################
#Adding Year Column & Combining Data 

#Hubbard Brook (HB) LTER - Forest Inventories of a Northern Hardwood Forest: Watershed 6
HB.inv.65.dat$Year<-1965
HB.inv.77.dat$Year<-1977
HB.inv.82.dat$Year<-1982
HB.inv.87.dat$Year<-1987
HB.inv.92.dat$Year<-1992
HB.inv.97.dat$Year<-1997
HB.inv.02.dat$Year<-2002
HB.inv.07.dat$Year<-2007
HB.inv.12.dat$Year<-2012
HB.inv.17.dat$Year<-2017

hb.inv.all<-HB.inv.65.dat%>%
  bind_rows(HB.inv.77.dat, HB.inv.82.dat, HB.inv.87.dat, HB.inv.92.dat,
            HB.inv.97.dat, HB.inv.02.dat, HB.inv.07.dat, HB.inv.12.dat,
            HB.inv.17.dat)

str(hb.inv.all)

############################################################
#Formatting Data

#Hubbard Brook (HB) LTER - Forest Inventories of a Northern Hardwood Forest: Watershed 6
# - Multiple
hb.inv.all.biodat<-hb.inv.all%>%
  filter(Vigor == 0)%>% #Live Trees Only
  mutate(site = "Watershed 6", 
         Parameter = "DBH, Biomass",
         data = "HB LTER - Forest Inventory W6",
         Taxa = "Trees",
         Specificity = "Species",
         ecosystem = "Forests",
         std_id = paste0("forest_", "HB.LTER_", site),
         plot = Plot,
         transect = paste0(Plot, "_", Tag),
         #date = lubridate::parse_date_time(XXXX, orders = c("mdy", "dmy", "ymd")),
         year = Year,
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         biomass = case_when(Dbh < 10 ~ (AbvBmss*1000)/TwotoTenArea,
                             Dbh >= 10 ~ (AbvBmss*1000)/X10Area),
         bmass_units = "g per m2",
         
         latitude = (43.9570010 + 43.949928)/2,
         longitude =  (-71.7356490 + -71.7434620)/2)%>%
  mutate(spp_full_name = case_when(spp_full_name =="SARA" ~ "Sambucus racemosa",
                                   spp_full_name =="ACSA" ~ "Acer saccharum",
                                   spp_full_name =="ACSA3" ~ "Acer saccharum",
                                   spp_full_name =="FAGR" ~ "Fagus grandifolia",
                                   spp_full_name =="FRAM" ~ "Fraxinus americana",
                                   spp_full_name =="ACSP" ~ "Acer spicatum",
                                   spp_full_name =="ACPE" ~ "Acer pensylvanicum",
                                   spp_full_name =="PRPE" ~ "Prunus pensylvanica",
                                   spp_full_name =="PRPE2" ~ "Prunus pensylvanica",
                                   spp_full_name =="PRPE3" ~ "Prunus pensylvanica",
                                   spp_full_name =="PRVI" ~ "Prunus virginiana",
                                   spp_full_name =="ABBA" ~ "Abies balsamea",
                                   spp_full_name =="PIRU" ~ "Picea rubens",
                                   spp_full_name =="BEPA" ~ "Betula papyrifera",
                                   spp_full_name =="SOAM" ~ "Sorbus americana",
                                   spp_full_name =="ACRU" ~ "Acer rubrum",
                                   spp_full_name =="TSCA" ~ "Tsuga canadensis",
                                   spp_full_name =="UNKN" ~ "998",
                                   spp_full_name =="POTR" ~ "Populus tremuloides",
                                   spp_full_name =="PRSE" ~ "Prunus serotina",
                                   spp_full_name =="PRSE2" ~ "Prunus serotina",
                                   spp_full_name =="AMSP" ~ "Amelanchier sp.",
                                   spp_full_name =="POGR" ~ "Populus grandidentata",
                                   spp_full_name =="SASP" ~ "Salix sp.",
                                   spp_full_name =="COAL" ~ "Cornus alternifolia",
                                   spp_full_name =="PRSP" ~ "Prunus sp.",
                                   spp_full_name =="BECO" ~ "Betula cordifolia",
                                   spp_full_name =="BEAL" ~ "Betula alleghaniensis",
                                   TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
mutate(species = case_when(species == "sp." | 
                             is.na(species) ~ "998",
                           TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  mutate(notes = "plot marks 'Plot'; transect marks 'Plot/Tag' to match Tree ID in diameter data")

str(hb.inv.all)
str(hb.inv.all.biodat)

#Species checking
spp.check<-hb.inv.all.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-hb.inv.all.biodat%>%
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
############################################################
#Examining Biomass Calculation Issue
hb.inf.backcheck<-hb.inv.all%>%
  mutate(site = "Watershed 6", 
         Parameter = "DBH, Biomass",
         data = "HB LTER - Forest Inventory W6",
         Taxa = "Trees",
         Specificity = "Species",
         ecosystem = "Forests",
         std_id = paste0("forest_", "HB.LTER_", site),
         plot = Plot,
         transect = 999,
         #date = lubridate::parse_date_time(XXXX, orders = c("mdy", "dmy", "ymd")),
         year = Year,
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         biomass = case_when(Dbh < 10 ~ (AbvBmss*1000)/TwotoTenArea,
                             Dbh >= 10 ~ (AbvBmss*1000)/X10Area),
         bmass_units = "g per m2",
         
         latitude = (43.9570010 + 43.949928)/2,
         longitude =  (-71.7356490 + -71.7434620)/2)%>%
  mutate(spp_full_name = case_when(spp_full_name =="SARA" ~ "Sambucus racemosa",
                                   spp_full_name =="ACSA" ~ "Acer saccharum",
                                   spp_full_name =="ACSA3" ~ "Acer saccharum",
                                   spp_full_name =="FAGR" ~ "Fagus grandifolia",
                                   spp_full_name =="FRAM" ~ "Fraxinus americana",
                                   spp_full_name =="ACSP" ~ "Acer spicatum",
                                   spp_full_name =="ACPE" ~ "Acer pensylvanicum",
                                   spp_full_name =="PRPE" ~ "Prunus pensylvanica",
                                   spp_full_name =="PRPE2" ~ "Prunus pensylvanica",
                                   spp_full_name =="PRPE3" ~ "Prunus pensylvanica",
                                   spp_full_name =="PRVI" ~ "Prunus virginiana",
                                   spp_full_name =="ABBA" ~ "Abies balsamea",
                                   spp_full_name =="PIRU" ~ "Picea rubens",
                                   spp_full_name =="BEPA" ~ "Betula papyrifera",
                                   spp_full_name =="SOAM" ~ "Sorbus americana",
                                   spp_full_name =="ACRU" ~ "Acer rubrum",
                                   spp_full_name =="TSCA" ~ "Tsuga canadensis",
                                   spp_full_name =="UNKN" ~ "998",
                                   spp_full_name =="POTR" ~ "Populus tremuloides",
                                   spp_full_name =="PRSE" ~ "Prunus serotina",
                                   spp_full_name =="PRSE2" ~ "Prunus serotina",
                                   spp_full_name =="AMSP" ~ "Amelanchier sp.",
                                   spp_full_name =="POGR" ~ "Populus grandidentata",
                                   spp_full_name =="SASP" ~ "Salix sp.",
                                   spp_full_name =="COAL" ~ "Cornus alternifolia",
                                   spp_full_name =="PRSP" ~ "Prunus sp.",
                                   spp_full_name =="BECO" ~ "Betula cordifolia",
                                   spp_full_name =="BEAL" ~ "Betula alleghaniensis",
                                   TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(species = case_when(species == "sp." | 
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units, Dbh, X10Area, TwotoTenArea)%>%
  filter(year %in% c(2007, 2012) & Dbh <10)

hb.twoten.cross<-hb.inf.backcheck%>%
  select(year, plot, TwotoTenArea)%>%
  filter(TwotoTenArea >0)%>%
  distinct()%>%
  arrange(year, plot)

duplicates<- hb.twoten.cross %>%
  mutate(duplicate = duplicated(paste(year, plot)) | duplicated(paste(year, plot), fromLast = TRUE))

dup.back<-hb.inf.backcheck%>%
  filter(year == 2012 & plot == 12 |
           year == 2012 & plot == 43 |
           year == 2012 & plot == 75)

plot.area.resets<-duplicates%>%
  filter(duplicate == "FALSE")

############################################################
############################################################
#Resetting Inf Values with Plot Area Resets

hb.inv.all.biodat2<-hb.inv.all%>%
  mutate(site = "Watershed 6", 
         Parameter = "DBH, Biomass",
         data = "HB LTER - Forest Inventory W6",
         Taxa = "Trees",
         Specificity = "Species",
         ecosystem = "Forests",
         std_id = paste0("forest_", "HB.LTER_", site),
         plot = Plot,
         transect = 999,
         #date = lubridate::parse_date_time(XXXX, orders = c("mdy", "dmy", "ymd")),
         year = Year,
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         biomass = case_when(Dbh < 10 ~ (AbvBmss*1000)/TwotoTenArea,
                             Dbh >= 10 ~ (AbvBmss*1000)/X10Area),
         bmass_units = "g per m2",
         
         latitude = (43.9570010 + 43.949928)/2,
         longitude =  (-71.7356490 + -71.7434620)/2)%>%
  left_join(plot.area.resets, by=c("year", "plot"))%>%
  mutate(biomass = case_when(biomass == Inf ~ (AbvBmss*1000)/TwotoTenArea.y,
                             TRUE ~ biomass))%>%
  mutate(spp_full_name = case_when(spp_full_name =="SARA" ~ "Sambucus racemosa",
                                   spp_full_name =="ACSA" ~ "Acer saccharum",
                                   spp_full_name =="ACSA3" ~ "Acer saccharum",
                                   spp_full_name =="FAGR" ~ "Fagus grandifolia",
                                   spp_full_name =="FRAM" ~ "Fraxinus americana",
                                   spp_full_name =="ACSP" ~ "Acer spicatum",
                                   spp_full_name =="ACPE" ~ "Acer pensylvanicum",
                                   spp_full_name =="PRPE" ~ "Prunus pensylvanica",
                                   spp_full_name =="PRPE2" ~ "Prunus pensylvanica",
                                   spp_full_name =="PRPE3" ~ "Prunus pensylvanica",
                                   spp_full_name =="PRVI" ~ "Prunus virginiana",
                                   spp_full_name =="ABBA" ~ "Abies balsamea",
                                   spp_full_name =="PIRU" ~ "Picea rubens",
                                   spp_full_name =="BEPA" ~ "Betula papyrifera",
                                   spp_full_name =="SOAM" ~ "Sorbus americana",
                                   spp_full_name =="ACRU" ~ "Acer rubrum",
                                   spp_full_name =="TSCA" ~ "Tsuga canadensis",
                                   spp_full_name =="UNKN" ~ "998",
                                   spp_full_name =="POTR" ~ "Populus tremuloides",
                                   spp_full_name =="PRSE" ~ "Prunus serotina",
                                   spp_full_name =="PRSE2" ~ "Prunus serotina",
                                   spp_full_name =="AMSP" ~ "Amelanchier sp.",
                                   spp_full_name =="POGR" ~ "Populus grandidentata",
                                   spp_full_name =="SASP" ~ "Salix sp.",
                                   spp_full_name =="COAL" ~ "Cornus alternifolia",
                                   spp_full_name =="PRSP" ~ "Prunus sp.",
                                   spp_full_name =="BECO" ~ "Betula cordifolia",
                                   spp_full_name =="BEAL" ~ "Betula alleghaniensis",
                                   TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(species = case_when(species == "sp." | 
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  mutate(notes = "plot marks 'Plot', aggregation likely site/plot/y/m/d")

#Species checking
spp.check<-hb.inv.all.biodat2%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-hb.inv.all.biodat2%>%
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
write.csv(hb.inv.all.biodat2, file="Biomass.Species.Indv.Forest.HB.LTER.W6.csv", row.names = F)
############################################################
