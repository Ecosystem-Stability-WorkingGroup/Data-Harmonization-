############################################################
#Load Packages
library(tidyverse)
library(lubridate)

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
# _ Vegetation Data - Small (0.25m2) Quadrats
# - UNDERSTORY_SMQUAD_2008-2019.csv
UHURU.smquad.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Savanna/Alston et al 2022/UNDERSTORY_SMQUAD_2008-2019.csv")
str(UHURU.smquad.dat)
#Trimming "Notes" Columns and Rows
UHURU.smquad.dat<-UHURU.smquad.dat[-c(1:3), -1]
#Trimming to Control Sites
UHURU.smquad.dat<-UHURU.smquad.dat%>%
  filter(treatment == "OPEN")

#Alston et al. 2022: 12 years of data from the UHURU experiment
# _ Vegetation Data - Large (1m2) Quadrats
# - UNDERSTORY_LGQUAD_2008-2019.csv
UHURU.lgquad.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Savanna/Alston et al 2022/UNDERSTORY_LGQUAD_2008-2019.csv")
str(UHURU.lgquad.dat)
#Trimming "Notes" Columns and Rows
UHURU.lgquad.dat<-UHURU.lgquad.dat[-c(1:3), -1]
#Trimming to Control Sites
UHURU.lgquad.dat<-UHURU.lgquad.dat%>%
  filter(treatment == "OPEN")



############################################################
#Formatting Data

#Alston et al. 2022: 12 years of data from the UHURU experiment
# _ Vegetation Data - Small (0.25m2) Quadrats
# - UNDERSTORY_SMQUAD_2008-2019.csv
UHURU.smquad.pcdat<-UHURU.smquad.dat%>%
  left_join(UHURU.coords, by = c("site" = "Site2", "block" = "block"))%>%
  mutate(site = plot, 
         Parameter = "Percent Cover, Presence/Absence",
         data = "UHURU - Vegetation Small Quadrats",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Savanna",
         std_id = paste0("grass.sav_", "UHURU_", site),
         plot = 999,
         transect = 999,
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         latitude = Latitude,
         longitude =  Longitude)%>%
  mutate(across(8:330, as.character))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, c(8:330))%>%
  pivot_longer(cols = 12:ncol(.), 
               names_to = "spp_full_name", 
               values_to = "percent_cover")%>%
  filter(percent_cover > 0 & !is.na(percent_cover))%>% #Filter out 0 Values
  mutate(spp_full_name = gsub("_", " ", spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "unknown B3" |
                             spp_full_name == "unknown B4" |
                             spp_full_name == "unknown B5" |
                             spp_full_name == "unknown B8" |
                             spp_full_name == "Brachiaria eruciformis . Brachiaria lachnantha . Brachiaria leersoides . Brachiaria leucracrantha" |
                             spp_full_name == "Bare ground" |
                             spp_full_name == "Pupalia lappacea . Psilotrichum elliotii" |
                             spp_full_name == "Plectranthus montanus . Plectranthus cylindraceus . Plectranthus caninus" |
                             spp_full_name == "Cynodon nlemfuensis . Digitaria macroblephara" |
                             spp_full_name == "Ruellia patula . Ruellia prostrata" |
                             spp_full_name == "Crassula schimperi . Portulaca parensis . Portulaca quadrifida" |
                             spp_full_name == "Digitaria velutina . Digitaria milanjiana" |
                             spp_full_name == "Cynanchum viminale . Cynanchum gerrardii" |
                             spp_full_name == "unknown3B" |
                             spp_full_name == "unknownS3" ~ "998",
                             TRUE ~ genus),
         species = case_when(spp_full_name == "unknown B3" |
                             spp_full_name == "unknown B4" |
                             spp_full_name == "unknown B5" |
                             spp_full_name == "unknown B8" |
                             spp_full_name == "Brachiaria eruciformis . Brachiaria lachnantha . Brachiaria leersoides . Brachiaria leucracrantha" |
                             spp_full_name == "Bare ground" |
                             spp_full_name == "Pupalia lappacea . Psilotrichum elliotii" |
                             spp_full_name == "Plectranthus montanus . Plectranthus cylindraceus . Plectranthus caninus" |
                             spp_full_name == "Cynodon nlemfuensis . Digitaria macroblephara" |
                             spp_full_name == "Ruellia patula . Ruellia prostrata" |
                             spp_full_name == "Crassula schimperi . Portulaca parensis . Portulaca quadrifida" |
                             spp_full_name == "Digitaria velutina . Digitaria milanjiana" |
                             spp_full_name == "Cynanchum viminale . Cynanchum gerrardii" |
                             spp_full_name == "unknown3B" |
                             spp_full_name == "unknownS3" |
                             is.na(species) ~ "998",
                           TRUE ~ species))%>%
  mutate(percent_cover = as.numeric(percent_cover))%>%
  mutate(notes = "aggregation at site level, some extremely high total percent cover values exist, be careful when aggregating with UHURU large quadrats (same site names)")


str(UHURU.smquad.dat)
str(UHURU.smquad.pcdat)

spp.check<-UHURU.smquad.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-UHURU.smquad.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()



#Alston et al. 2022: 12 years of data from the UHURU experiment
# _ Vegetation Data - Large (1m2) Quadrats
# - UNDERSTORY_LGQUAD_2008-2019.csv
UHURU.lgquad.pcdat<-UHURU.lgquad.dat%>%
  left_join(UHURU.coords, by = c("site" = "Site2", "block" = "block"))%>%
  mutate(site = plot, 
         Parameter = "Percent Cover, Presence/Absence",
         data = "UHURU - Vegetation Quadrats [0.25m2 and 1m2]",       #Same as Small Quads - Combining for Mapping
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Savanna",
         std_id = paste0("grass.sav_", "UHURU_", site),
         plot = 999,
         transect = 999,
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         latitude = Latitude,
         longitude =  Longitude)%>%
  mutate(across(8:330, as.character))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, c(8:330))%>%
  pivot_longer(cols = 12:ncol(.), 
               names_to = "spp_full_name", 
               values_to = "percent_cover")%>%
  filter(percent_cover > 0 & !is.na(percent_cover))%>% #Filter out 0 Values
  mutate(spp_full_name = gsub("_", " ", spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "unknown B3" |
                             spp_full_name == "unknown B4" |
                             spp_full_name == "unknown B5" |
                             spp_full_name == "unknown B8" |
                             spp_full_name == "Brachiaria eruciformis . Brachiaria lachnantha . Brachiaria leersoides . Brachiaria leucracrantha" |
                             spp_full_name == "Bare ground" |
                             spp_full_name == "Pupalia lappacea . Psilotrichum elliotii" |
                             spp_full_name == "Plectranthus montanus . Plectranthus cylindraceus . Plectranthus caninus" |
                             spp_full_name == "Cynodon nlemfuensis . Digitaria macroblephara" |
                             spp_full_name == "Ruellia patula . Ruellia prostrata" |
                             spp_full_name == "Crassula schimperi . Portulaca parensis . Portulaca quadrifida" |
                             spp_full_name == "Digitaria velutina . Digitaria milanjiana" |
                             spp_full_name == "Cynanchum viminale . Cynanchum gerrardii" |
                             spp_full_name == "unknown3B" |
                             spp_full_name == "unknown BB" |
                             spp_full_name == "Bothriochloa insculpta . Bothriochloa radicans" |
                             spp_full_name == "unknownS3" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "unknown B3" |
                               spp_full_name == "unknown B4" |
                               spp_full_name == "unknown B5" |
                               spp_full_name == "unknown B8" |
                               spp_full_name == "Brachiaria eruciformis . Brachiaria lachnantha . Brachiaria leersoides . Brachiaria leucracrantha" |
                               spp_full_name == "Bare ground" |
                               spp_full_name == "Pupalia lappacea . Psilotrichum elliotii" |
                               spp_full_name == "Plectranthus montanus . Plectranthus cylindraceus . Plectranthus caninus" |
                               spp_full_name == "Cynodon nlemfuensis . Digitaria macroblephara" |
                               spp_full_name == "Ruellia patula . Ruellia prostrata" |
                               spp_full_name == "Crassula schimperi . Portulaca parensis . Portulaca quadrifida" |
                               spp_full_name == "Digitaria velutina . Digitaria milanjiana" |
                               spp_full_name == "Cynanchum viminale . Cynanchum gerrardii" |
                               spp_full_name == "unknown3B" |
                               spp_full_name == "unknown BB" |
                               spp_full_name == "Bothriochloa insculpta . Bothriochloa radicans" |
                               spp_full_name == "unknownS3" |
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  mutate(percent_cover = as.numeric(percent_cover))%>%
  mutate(notes = "aggregation at site level, some extremely high total percent cover values exist, be careful when aggregating with UHURU large quadrats (same site names)")



str(UHURU.lgquad.dat)
str(UHURU.lgquad.pcdat)

spp.check<-UHURU.lgquad.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-UHURU.lgquad.pcdat%>%
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
#Combine Files

UHURU.all.pcdat<-bind_rows(
  UHURU.smquad.pcdat,
  UHURU.lgquad.pcdat
)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(UHURU.all.pcdat, file="Percent.Cover.Species.GrassSav.UHURU.All.PC.csv", row.names = F)
############################################################

