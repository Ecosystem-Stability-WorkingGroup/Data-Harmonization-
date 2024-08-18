############################################################
#Load Packages
library(tidyverse)
library(lubridate)


############################################################
#Automatic CSV Importing

#Virginia Coast Reserve (VCR) LTER - End of Year Marsh Plant Biomass - EOYB_data.csv
VCR.eoyb.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/VCR LTER/EOYB_data.csv",
                       skip = 21)
str(VCR.eoyb.dat)

#Virginia Coast Reserve (VCR) LTER - End of Year Marsh Plant Biomass in Transition Plots - UPT_data.csv
VCR.upt.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/VCR LTER/UPT_data.csv",
                      skip = 21)
str(VCR.upt.dat)

############################################################
#Formatting Data

#Virginia Coast Reserve (VCR) LTER - End of Year Marsh Plant Biomass - EOYB_data.csv
VCR.eoyb.biodat<-VCR.eoyb.dat%>%
  mutate(site = siteName,
         site = case_when(site == "Hog" & marshAbbrev =="HOGS" ~ "Hog_South",
                          site == "Hog" & marshAbbrev =="HOGN" ~ "Hog_North",
                          TRUE ~ site),
         Parameter = "Biomass",
         data = "VCR LTER - End of Year Biomass",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "VCR.LTER_", site),
         plot = Replicate,
         transect = Transect,
         date = lubridate::parse_date_time(collectDate, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         year = ifelse(is.na(year), EOYBYear, year),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = speciesName,
         biomass = liveMass * (1/0.625),
         bmass_units = "g per m2",
         latitude = latitude,
         longitude = longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Pine cones" |
                             spp_full_name == "Combined Dead" |
                             spp_full_name == "Plant debris - leaves" |
                             spp_full_name == "Woody debris" |
                             spp_full_name == "Pine needles" |
                             spp_full_name == "Algae: unidentified" |
                             spp_full_name == "Unidentified vine" |
                             spp_full_name == "Unidentified" |
                             spp_full_name == "" ~ "998",
                             spp_full_name == "Algae: Gracilaria" ~ "Gracilaria",
                             spp_full_name == "Algae: Ulva" ~ "Ulva",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Pine cones" |
                             spp_full_name == "Combined Dead" |
                             spp_full_name == "Plant debris - leaves" |
                             spp_full_name == "Woody debris" |
                             spp_full_name == "Pine needles" |
                             spp_full_name == "Algae: unidentified" |
                             spp_full_name == "Unidentified vine" |
                             spp_full_name == "Unidentified" |
                             spp_full_name == "" ~ "998",
                           spp_full_name == "Algae: Gracilaria" ~ "998",
                           spp_full_name == "Algae: Ulva" ~ "998",
                           species == "sp." ~ "998",
                           TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  filter(spp_full_name != "Snail")%>%
  mutate(notes = "plot marks 'Replicate', transect marks 'Transect', aggregation likely site/plot/transect/y/m/d")

str(VCR.eoyb.dat)
str(VCR.eoyb.biodat)

#Species checking
spp.check<-VCR.eoyb.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

#Backcheck for aggregation levels
check1<-VCR.eoyb.biodat%>%
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
write.csv(VCR.eoyb.biodat, file="Biomass.Species.Indv.SMEW.VCR.LTER.EOYBmass.csv", row.names = F)
############################################################

#Virginia Coast Reserve (VCR) LTER - End of Year Marsh Plant Biomass in Transition Plots - UPT_data.csv
VCR.upt.biodat<-VCR.upt.dat%>%
  mutate(site = siteName, 
         Parameter = "Biomass",
         data = "VCR LTER - End of Year Biomass - Transition Plots",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "VCR.LTER_", site),
         plot = Replicate,
         transect = Transect,
         date = lubridate::parse_date_time(collectDate, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         year = ifelse(is.na(year), EOYBYear, year),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = speciesName,
         biomass = liveMass * (1/0.625),
         bmass_units = "g per m2",
         latitude = latitude,
         longitude = longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Pine cones" |
                             spp_full_name == "Combined Dead" |
                             spp_full_name == "Plant debris - leaves" |
                             spp_full_name == "Woody debris" |
                             spp_full_name == "Pine needles" |
                             spp_full_name == "Algae: unidentified" |
                             spp_full_name == "Unidentified vine" |
                             spp_full_name == "Unidentified" |
                             spp_full_name == "" ~ "998",
                           spp_full_name == "Algae: Gracilaria" ~ "Gracilaria",
                           spp_full_name == "Algae: Ulva" ~ "Ulva",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Pine cones" |
                               spp_full_name == "Combined Dead" |
                               spp_full_name == "Plant debris - leaves" |
                               spp_full_name == "Woody debris" |
                               spp_full_name == "Pine needles" |
                               spp_full_name == "Algae: unidentified" |
                               spp_full_name == "Unidentified vine" |
                               spp_full_name == "Unidentified" |
                               spp_full_name == "" ~ "998",
                             spp_full_name == "Algae: Gracilaria" ~ "998",
                             spp_full_name == "Algae: Ulva" ~ "998",
                             species == "sp." ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  filter(spp_full_name != "Snail")%>%
  mutate(notes = "plot marks 'Replicate', transect marks 'Transect', aggregation likely site/plot/transect/y/m/d")

str(VCR.upt.dat)
str(VCR.upt.biodat)

#Species checking
spp.check<-VCR.upt.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

#Backcheck for aggregation levels
check1<-VCR.upt.biodat%>%
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
write.csv(VCR.upt.biodat, file="Biomass.Species.Indv.SMEW.VCR.LTER.UPTBmass.csv", row.names = F)
############################################################

#Site Crosscheck

eoy<-VCR.eoyb.biodat%>%
  select(site, plot, transect, year, month, day)%>%
  distinct()%>%
  mutate(eoy = "X")

upt<-VCR.upt.biodat%>%
  select(site, plot, transect, year, month, day)%>%
  distinct()%>%
  mutate(upt = "X")

vcr.cross<-eoy%>%
  full_join(upt, by=c("site", "plot", "transect", "year", "month", "day"))%>%
  mutate(both = case_when(eoy == "X" & upt == "X" ~ "X",
                          TRUE ~ NA),
         eoy.only = case_when(eoy == "X" & is.na(upt) ~ "X",
                              TRUE ~ NA),
         upt.only = case_when(upt == "X" & is.na(eoy) ~ "X",
                              TRUE ~ NA))


#duplicate crosscheck
vcr.all<-bind_rows(VCR.eoyb.biodat, VCR.upt.biodat)%>%
  select(std_id, data, latitude, longitude, ecosystem, site)%>%
  distinct()%>%
  mutate(duplicated = duplicated(paste(std_id)) | duplicated(paste(std_id), fromLast = TRUE))


vcr.dups<-VCR.eoyb.dat%>%
  filter(siteName=="Hog")
