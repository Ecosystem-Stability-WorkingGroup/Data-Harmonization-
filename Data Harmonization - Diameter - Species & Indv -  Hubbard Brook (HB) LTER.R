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
hb.inv.all.DIAdat<-hb.inv.all%>%
  filter(Vigor == 0)%>% #Live Trees Only
  filter(Tag != 0)%>%
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
         tree_id = paste0(Plot, "_", Tag),
         stem_id = as.character("no_stem_id"),
         diameter = Dbh,
         diam_units = "cm",
         
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
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%    
  mutate(notes = "plot marks 'Plot'; transect duplicates 'Plot/Tag' to match Tree ID to biomass data")

str(hb.inv.all)
str(hb.inv.all.DIAdat)

#Species checking
spp.check<-hb.inv.all.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(hb.inv.all.DIAdat, file="Diameter.Species.Indv.Forest.HB.LTER.W6.csv", row.names = F)
############################################################