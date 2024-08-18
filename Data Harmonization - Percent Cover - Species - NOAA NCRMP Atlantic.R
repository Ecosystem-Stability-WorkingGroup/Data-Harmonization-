############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#NOAA NCRMP Benthic Cover - Puerto Rico
# - CRCP_Benthic_Cover_Puerto_Rico_1220_55cd_a163.csv
lines_to_skip <- c(2)
data_lines <- readLines("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/NOAA NCRMP/CRCP_Benthic_Cover_Puerto_Rico_1220_55cd_a163.csv")
data_lines <- data_lines[-lines_to_skip]
NOAA.pr.dat<-read.csv(text = data_lines, header = TRUE)
str(NOAA.pr.dat)

#NOAA NCRMP Benthic Cover - Florida
# - CRCP_Benthic_Cover_Puerto_Rico_1220_55cd_a163.csv
lines_to_skip <- c(2)
data_lines <- readLines("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/NOAA NCRMP/CRCP_Benthic_Cover_Florida_59f9_2164_efa8.csv")
data_lines <- data_lines[-lines_to_skip]
NOAA.fl.dat<-read.csv(text = data_lines, header = TRUE)
str(NOAA.fl.dat)

#NOAA NCRMP Benthic Cover - USVI
# - CRCP_Benthic_Cover_USVI_a5ca_1402_566a.csv
lines_to_skip <- c(2)
data_lines <- readLines("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/NOAA NCRMP/CRCP_Benthic_Cover_USVI_a5ca_1402_566a.csv")
data_lines <- data_lines[-lines_to_skip]
NOAA.usvi.dat<-read.csv(text = data_lines, header = TRUE)
str(NOAA.usvi.dat)

############################################################
#Formatting Data

#NOAA NCRMP Benthic Cover - Puerto Rico
# - CRCP_Benthic_Cover_Puerto_Rico_1220_55cd_a163.csv
NOAA.pr.pcdat<-NOAA.pr.dat%>%
  group_by(YEAR, latitude, longitude, PRIMARY_SAMPLE_UNIT, STATION_NR)%>%
  mutate(site = paste("PR", YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "_"))%>% #Assigning Unique Values to each Site in Each Year
  ungroup()%>%
  mutate(year = YEAR,
         Parameter = "Percent Cover",
         data = "NOAA NCRMP - Puerto Rico",
         Taxa = "Hard Coral, Soft Coral, Algae",
         Specificity = "Species",
         ecosystem = "Coral Reef",
         std_id = paste0("coral_", "NOAA.NCRMP_", site),
         plot = 999,
         transect = 999,
         month = MONTH,
         month = ifelse(is.na(month), 999, month),
         day = DAY,
         day = ifelse(is.na(day), 999, day),
         spp_full_name = COVER_CAT_NAME,
         percent_cover = rowSums(across(HARDBOTTOM_P:RUBBLE_P), na.rm=T),
         latitude = latitude,
         longitude =  longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Turf Algae Free of Sediment" |
                             spp_full_name == "Turf Algae with Sediment" |
                             spp_full_name == "Encrusting gorgonian" |
                             spp_full_name == "Other species" |
                             spp_full_name == "Other coral" |
                             spp_full_name == "Gorgonians" |
                             spp_full_name == "MacroOtherCalcareous" |
                             spp_full_name == "Bare Substrate" |
                             spp_full_name == "MacroOtherFleshy"~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Turf Algae Free of Sediment" |
                             spp_full_name == "Turf Algae with Sediment" |
                             spp_full_name == "Encrusting gorgonian" |
                             spp_full_name == "Other species" |
                             spp_full_name == "Other coral" |
                             spp_full_name == "Gorgonians" |
                             spp_full_name == "MacroOtherCalcareous" |
                             spp_full_name == "Bare Substrate" |
                             spp_full_name == "MacroOtherFleshy" |
                             spp_full_name == "Rhodophyta cru. spp" |
                             genus == "Peysonnellia" |
                             species == "spp" ~ "998",
                           TRUE ~ species))%>%
  filter(!is.na(percent_cover) & percent_cover >0)%>%
  mutate(notes = "A few sites with 101% cover; aggregatin at site; sites differ between years, will need to spatially aggregate")

str(NOAA.pr.dat)
str(NOAA.pr.pcdat)

spp.check<-NOAA.pr.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


check1<-NOAA.pr.pcdat%>%
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
write.csv(NOAA.pr.pcdat, file="Percent.Cover.Species.CoralReef.NOAA.NCRMP.PR.csv", row.names = F)
############################################################

#NOAA NCRMP Benthic Cover - Florida
# - CRCP_Benthic_Cover_Puerto_Rico_1220_55cd_a163.csv
NOAA.fl.pcdat<-NOAA.fl.dat%>%
  group_by(YEAR, latitude, longitude, PRIMARY_SAMPLE_UNIT, STATION_NR)%>%
  mutate(site = paste("FL", YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "_"))%>% #Assigning Unique Values to each Site in Each Year
  ungroup()%>%
  mutate(year = YEAR,
         Parameter = "Percent Cover",
         data = "NOAA NCRMP - Florida",
         Taxa = "Hard Coral, Soft Coral, Algae",
         Specificity = "Species",
         ecosystem = "Coral Reef",
         std_id = paste0("coral_", "NOAA.NCRMP_", site),
         plot = 999,
         transect = 999,
         month = MONTH,
         month = ifelse(is.na(month), 999, month),
         day = DAY,
         day = ifelse(is.na(day), 999, day),
         spp_full_name = COVER_CAT_NAME,
         percent_cover = rowSums(across(HARDBOTTOM_P:RUBBLE_P), na.rm=T),
         latitude = latitude,
         longitude =  longitude)%>%   
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Turf Algae Free of Sediment" |
                             spp_full_name == "Turf Algae with Sediment" |
                             spp_full_name == "Encrusting gorgonian" |
                             spp_full_name == "Other species" |
                             spp_full_name == "Other coral" |
                             spp_full_name == "Gorgonians" |
                             spp_full_name == "MacroOtherCalcareous" |
                             spp_full_name == "Bare Substrate" |
                             spp_full_name == "MacroOtherFleshy" |
                             is.na(genus) ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Turf Algae Free of Sediment" |
                               spp_full_name == "Turf Algae with Sediment" |
                               spp_full_name == "Encrusting gorgonian" |
                               spp_full_name == "Other species" |
                               spp_full_name == "Other coral" |
                               spp_full_name == "Gorgonians" |
                               spp_full_name == "MacroOtherCalcareous" |
                               spp_full_name == "Bare Substrate" |
                               spp_full_name == "MacroOtherFleshy" |
                               spp_full_name == "Rhodophyta cru. spp" |
                               genus == "Peysonnellia" |
                               species == "spp" |
                               is.na(species) ~ "998",
                             TRUE ~ species),
         spp_full_name = case_when(is.na(spp_full_name) ~ "998",
                                         TRUE ~ spp_full_name))%>%
  filter(!is.na(percent_cover) & percent_cover >0)%>%
  mutate(notes = "aggregation at site; sites differ between years, will need to spatially aggregate")



str(NOAA.fl.dat)
str(NOAA.fl.pcdat)

spp.check<-NOAA.fl.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-NOAA.fl.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

check2<-NOAA.fl.pcdat%>%filter(site=="FL_2016_3119_1" & year == 2016 & month == 8 & day == 8)

#Fixing sites with duplicated values
fl.dups<-NOAA.fl.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  filter(check.full>100)%>%
  distinct()

fl.nodups<-NOAA.fl.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  filter(check.full<=100)

NOAA.fl.pcdat2<-bind_rows(
  fl.nodups,
  fl.dups
)%>%
  select(-check.full)

check3<-NOAA.fl.pcdat2%>%
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
write.csv(NOAA.fl.pcdat2, file="Percent.Cover.Species.CoralReef.NOAA.NCRMP.FL.csv", row.names = F)
############################################################

#NOAA NCRMP Benthic Cover - USVI
# - CRCP_Benthic_Cover_USVI_a5ca_1402_566a.csv
NOAA.usvi.pcdat<-NOAA.usvi.dat%>%
  group_by(YEAR, latitude, longitude, PRIMARY_SAMPLE_UNIT, STATION_NR)%>%
  mutate(site = paste("USVI", YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR, sep = "_"))%>% #Assigning Unique Values to each Site in Each Year
  ungroup()%>%
  mutate(year = YEAR,
         Parameter = "Percent Cover",
         data = "NOAA NCRMP - USVI",
         Taxa = "Hard Coral, Soft Coral, Algae",
         Specificity = "Species",
         ecosystem = "Coral Reef",
         std_id = paste0("coral_", "NOAA.NCRMP_", site),
         plot = 999,
         transect = 999,
         month = MONTH,
         month = ifelse(is.na(month), 999, month),
         day = DAY,
         day = ifelse(is.na(day), 999, day),
         spp_full_name = COVER_CAT_NAME,
         percent_cover = rowSums(across(HARDBOTTOM_P:RUBBLE_P), na.rm=T),
         latitude = latitude,
         longitude =  longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Turf Algae Free of Sediment" |
                             spp_full_name == "Turf Algae with Sediment" |
                             spp_full_name == "Encrusting gorgonian" |
                             spp_full_name == "Other species" |
                             spp_full_name == "Other coral" |
                             spp_full_name == "Gorgonians" |
                             spp_full_name == "MacroOtherCalcareous" |
                             spp_full_name == "Bare Substrate" |
                             spp_full_name == "MacroOtherFleshy" |
                             is.na(genus) ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Turf Algae Free of Sediment" |
                               spp_full_name == "Turf Algae with Sediment" |
                               spp_full_name == "Encrusting gorgonian" |
                               spp_full_name == "Other species" |
                               spp_full_name == "Other coral" |
                               spp_full_name == "Gorgonians" |
                               spp_full_name == "MacroOtherCalcareous" |
                               spp_full_name == "Bare Substrate" |
                               spp_full_name == "MacroOtherFleshy" |
                               spp_full_name == "Rhodophyta cru. spp" |
                               genus == "Peysonnellia" |
                               species == "spp" |
                               is.na(species) ~ "998",
                             TRUE ~ species),
         spp_full_name = case_when(is.na(spp_full_name) ~ "998",
                                   TRUE ~ spp_full_name))%>%
  filter(!is.na(percent_cover) & percent_cover >0)%>%
  mutate(notes = "aggregation at site; a few sites with >100% cover; sites differ between years, will need to spatially aggregate")

str(NOAA.usvi.dat)
str(NOAA.usvi.pcdat)

spp.check<-NOAA.usvi.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-NOAA.usvi.pcdat%>%
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
write.csv(NOAA.usvi.pcdat, file="Percent.Cover.Species.CoralReef.NOAA.NCRMP.USVI.csv", row.names = F)
############################################################
