############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#Plum Island Ecosystems (PIE) LTER - Salt Marsh Vegetation Cover - Monitored Transects - Rowly MA
# - PIE-MD-VEGQUADS_2022.csv
PIE.cover.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/PIE LTER/PIE-MD-VEGQUADS_2022.csv")
str(PIE.cover.dat)


############################################################
#Trimming to Control Sites
PIE.cover.dat<-PIE.cover.dat%>%
  filter(Site %in% c("CC", "PUH"))


############################################################
#Formatting Data

#Dataset Name 
# - CSV Name
PIE.cover.pcdat<-PIE.cover.dat%>%
  mutate(site = Site, 
         Parameter = "Percent Cover",
         data = "PIE LTER - Salt Marsh Veg. Cover",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "PIE.LTER_", site),
         plot = 999,
         transect = Transect,
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         latitude = (42.747 + 42.74)/2,
         longitude =  (-70.847 + -70.836)/2)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, c(7:42))%>%
  pivot_longer(cols = 12:ncol(.), 
               names_to = "spp_full_name", 
               values_to = "percent_cover")%>%
  mutate(percent_cover = percent_cover *100)%>%
  mutate(spp_full_name = gsub("\\.", " ", spp_full_name),
         spp_full_name = gsub(" sp $", " sp.", spp_full_name),
         spp_full_name = ifelse(spp_full_name == "Carex cf stricta", "Carex stricta", spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  filter(!is.na(percent_cover) & percent_cover > 0)%>%
  mutate(notes = "transect marks 'Transect'; aggregation at site/transect/y/m/d; very large total cover values")

str(PIE.cover.dat)
str(PIE.cover.pcdat)

spp.check<-PIE.cover.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


check1<-PIE.cover.pcdat%>%
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
write.csv(PIE.cover.pcdat, file="Percent.Cover.Species.SMEW.PIE.LTER.PCRowly.csv", row.names = F)
############################################################


