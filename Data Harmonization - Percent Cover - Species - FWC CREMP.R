############################################################
#Load Packages
library(tidyverse)
library(sf)
library(lubridate)


############################################################
#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All SItes - Initial Data Mapping")

############################################################
#Automatic CSV Importing
flk.coverdat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/FWC CREMP/CREMP_CSV_Files/CREMP_Pcover_1996-2022_Stony_Coral_Species.csv")
dt.coverdat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/FWC CREMP/DTCREMP_CSV_Files/DTCREMP_Pcover_1999-2022_Stony_Coral_Species.csv")
se.coverdat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/FWC CREMP/SECREMP_CSV_Files/SECREMP_Pcover_2003-2022_Stony_Coral_Species.csv")

############################################################
############################################################
#Importing Coordinates from Geodatabase

#Viewing List of Layers
st_layers("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/FWC CREMP/CREMP_Geodatabase/CREMP.gdb")
st_layers("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/FWC CREMP/DTCREMP_Geodatabase/DTCREMP.gdb")
st_layers("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/FWC CREMP/SECREMP_Geodatabase/SECREMP.gdb")

#Import Coordinate Layer
flk.coords<-st_read("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/FWC CREMP/CREMP_Geodatabase/CREMP.gdb", layer = "CREMP_Station_Locations_pnt_Albers")
dt.coords<-st_read("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/FWC CREMP/DTCREMP_Geodatabase/DTCREMP.gdb", layer = "DTCREMP_Station_Locations_pnt_Albers")
se.coords<-st_read("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/FWC CREMP/SECREMP_Geodatabase/SECREMP.gdb", layer = "SECREMP_Station_Locations_pnt_Albers")

############################################################
#Mapping Coordinates onto Data 

############################################################
#Florida Keys Data

flk.coords2<-flk.coords%>%
  select(SiteID, Site_name, latDD, lonDD)%>%
  group_by(SiteID, Site_name)%>%
  mutate(latitude = mean(latDD, na.rm = T),
         longitude = mean(lonDD, na.rm = T))%>%
  distinct()

flk.coords2<-as.data.frame(flk.coords2)%>%
  select(-c(latDD, lonDD))%>%
  select(-c(Shape))%>%
  distinct()

############################################################
#Dry Tortugas Data

dt.coords2<-dt.coords%>%
  select(SiteID, Site_name, latDD, lonDD)%>%
  group_by(SiteID, Site_name)%>%
  mutate(latitude = mean(latDD, na.rm = T),
         longitude = mean(lonDD, na.rm = T))%>%
  distinct()

dt.coords2<-as.data.frame(dt.coords2)%>%
  select(-c(latDD, lonDD))%>%
  select(-c(Shape))%>%
  distinct()

############################################################
#SE Florida Data

se.coords2<-se.coords%>%
  select(SiteID, Site_name, latDD, lonDD)%>%
  group_by(SiteID, Site_name)%>%
  mutate(latitude = mean(latDD, na.rm = T),
         longitude = mean(lonDD, na.rm = T))%>%
  distinct()

se.coords2<-as.data.frame(se.coords2)%>%
  select(-c(latDD, lonDD))%>%
  select(-c(Shape))%>%
  distinct()
############################################################
############################################################
#Formatting Data


############################################################
#Florida Keys Data

flk.fwc.cremp.pcdat<-flk.coverdat%>%
  left_join(flk.coords2, by=c("SiteID", "Site_name"))%>%
  mutate(site = Site_name,
         Parameter = "Percent Cover",
         Taxa = "Stony Corals",
         Specificity = "Species",
         ecosystem = "Coral Reef",
         data = "FWC CREMP",
         date = parse_date_time(Date, orders = c("dmy", "mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         year = as.numeric(format(date, "%Y")),
         std_id = paste0("coral_", "FWC.CREMP_", site),
         plot = StationID,
         transect = 999)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, c(10:49))%>%
  pivot_longer(cols = 12:51, 
               names_to = "spp_full_name", 
               values_to = "percent_cover")%>%
  mutate(spp_full_name = gsub("_", " ", spp_full_name),
         percent_cover = percent_cover*100)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  filter(percent_cover > 0)%>%
  filter(!is.na(year))%>%
  mutate(species = case_when(species == "sp" |
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'StationID', aggregation site/plot/y/m/d")




str(flk.coverdat)
str(flk.fwc.cremp.pcdat)

#Species checking
spp.check<-flk.fwc.cremp.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-flk.fwc.cremp.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()
############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(flk.fwc.cremp.pcdat, file="Percent.Cover.Species.CoralReef.FWC.CREMP.FLKEYS.csv", row.names = F)
############################################################

############################################################
#Dry Tortugas Data

dt.fwc.cremp.pcdat<-dt.coverdat%>%
  left_join(dt.coords2, by=c("SiteID", "Site_name"))%>%
  mutate(site = Site_name,
         Parameter = "Percent Cover",
         Taxa = "Stony Corals",
         Specificity = "Species",
         ecosystem = "Coral Reef",
         data = "FWC CREMP",
         date = parse_date_time(Date, orders = c("dmy", "mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         day = day(date),
         year = as.numeric(format(date, "%Y")),
         std_id = paste0("coral_", "FWC.CREMP_", site),
         plot = StationID,
         transect = 999)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, c(9:46))%>%
  pivot_longer(cols = 12:ncol(.), 
               names_to = "spp_full_name", 
               values_to = "percent_cover")%>%
  mutate(spp_full_name = gsub("_", " ", spp_full_name),
         percent_cover = percent_cover*100)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  filter(percent_cover > 0)%>%
  filter(!is.na(year))%>%
  mutate(species = case_when(species == "sp" |
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(notes = "plot marks 'StationID', aggregation site/plot/y/m/d")




str(dt.coverdat)
str(dt.fwc.cremp.pcdat)

#Species checking
spp.check<-dt.fwc.cremp.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-dt.fwc.cremp.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()
############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(dt.fwc.cremp.pcdat, file="Percent.Cover.Species.CoralReef.FWC.CREMP.DT.csv", row.names = F)
############################################################



############################################################
#SE Florida Data

se.fwc.cremp.pcdat<-se.coverdat%>%
  left_join(se.coords2, by=c("SiteID", "Site_name"))%>%
  mutate(site = Site_name,
         Parameter = "Percent Cover",
         Taxa = "Stony Corals",
         Specificity = "Species",
         ecosystem = "Coral Reef",
         data = "FWC CREMP",
         date = parse_date_time(Date, orders = c("dmy", "mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         day = day(date),
         year = as.numeric(format(date, "%Y")),
         std_id = paste0("coral_", "FWC.CREMP_", site),
         plot = StationID,
         transect = 999)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, c(9:37))%>%
  pivot_longer(cols = 12:ncol(.), 
               names_to = "spp_full_name", 
               values_to = "percent_cover")%>%
  mutate(spp_full_name = gsub("_", " ", spp_full_name),
         percent_cover = percent_cover*100)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  filter(percent_cover > 0)%>%
  filter(!is.na(year))%>%
  mutate(species = case_when(species == "sp" |
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(notes = "plot marks 'StationID', aggregation site/plot/y/m/d")





str(se.coverdat)
str(se.fwc.cremp.pcdat)

#Species checking
spp.check<-se.fwc.cremp.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-se.fwc.cremp.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(se.fwc.cremp.pcdat, file="Percent.Cover.Species.CoralReef.FWC.CREMP.SEFL.csv", row.names = F)
############################################################

#Ensuring no Duplicate Site Names

hmm<-bind_rows(flk.coords2,
               dt.coords2,
               se.coords2)

df_duplicates <- hmm%>%
  group_by(!!sym("Site_name")) %>%
  filter(n() > 1) %>%
  ungroup()
