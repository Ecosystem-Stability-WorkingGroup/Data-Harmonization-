############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#Louisiana Coastwide Reference Monitoring System (CRMS) - Vegetation Percent Cover
# - 15431.csv
CRMS.vegcov.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/Louisiana CRMS/15431_VegPercentCover/15431.csv")
str(CRMS.vegcov.dat)

# - Coordinates - 15431_Coordinates.csv
CRMS.vegcov.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/Louisiana CRMS/15431_VegPercentCover/15431_Coordinates.csv")

############################################################
#Formatting Data

#Louisiana Coastwide Reference Monitoring System (CRMS) - Vegetation Percent Cover
# - 15431.csv
CRMS.vegcov.pcdat<-CRMS.vegcov.dat%>%
  left_join(CRMS.vegcov.coords, by = c("Site_ID"))%>%
  mutate(site = Site_ID, 
         Parameter = "Percent Cover",
         data = "Louisiana CRMS - Vegetation Percent Cover",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "LA.CRMS_", site),
         plot = 999,
         transect = 999,
         date = lubridate::parse_date_time(Collection_Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         percent_cover = percent_cover_derived,
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Unknown #1" |
                             spp_full_name == "Unknown #2" |
                             spp_full_name == "Unknown #3" |
                             spp_full_name == "Unknown #4" |
                             spp_full_name == "Unknown #5" |
                             spp_full_name == "Unknown #6" |
                             spp_full_name == "Unknown #7" |
                             spp_full_name == "Unknown #8" |
                             spp_full_name == "Unknown #9" |
                             spp_full_name == "Unknown" |
                             spp_full_name == "Bare Ground" |
                             spp_full_name == "Water" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Unknown #1" |
                             spp_full_name == "Unknown #2" |
                             spp_full_name == "Unknown #3" |
                             spp_full_name == "Unknown #4" |
                             spp_full_name == "Unknown #5" |
                             spp_full_name == "Unknown #6" |
                             spp_full_name == "Unknown #7" |
                             spp_full_name == "Unknown #8" |
                             spp_full_name == "Unknown #9" |
                             spp_full_name == "Unknown" |
                             spp_full_name == "Bare Ground" |
                             spp_full_name == "Water" |
                             species == "(Hitchc." |
                             species == "(Raf.)" |
                             species == "(Rchb.)" |
                             species == "Adans." |
                             species == "C." |
                             species == "Cass." |
                             species == "Döll" |
                             species == "F.H." |
                             species == "Fourn." |
                             species == "Hill" |
                             species == "J.F." |
                             species == "Jacq." |
                             species == "L." |
                             species == "Less." |
                             species == "Lour." |
                             species == "Loureiro" |
                             species == "Michx." |
                             species == "Mill." |
                             species == "Nash" |  
                             species == "Nees" |  
                             species == "P." |  
                             species == "P.J." |  
                             species == "R." |  
                             species == "Raf." |  
                             species == "Rich." |  
                             species == "Salisb." |  
                             species == "Savi" |  
                             species == "Schmidel" |  
                             species == "Schott" |  
                             species == "Schreb." |  
                             species == "Scop." |  
                             species == "Steud." |  
                             species == "Sw." |  
                             species == "Vahl" |  
                             species == "Aubl." |
                             species == "Banks" |
                             species == "Brongn." |
                             species == "Juss." |
                             species == "Willd."  ~ "998",
                           TRUE ~ species))%>%
  filter(percent_cover > 0)%>%
  mutate(notes = "aggregation at site")


str(CRMS.vegcov.dat)
str(CRMS.vegcov.pcdat)

spp.check<-CRMS.vegcov.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

df_with_capitals <- spp.check %>%
  filter(str_detect(species, "[A-Z]"))

check1<-CRMS.vegcov.pcdat%>%
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
write.csv(CRMS.vegcov.pcdat, file="Percent.Cover.Species.SMEW.LA.CRMS.csv", row.names = F)
############################################################


