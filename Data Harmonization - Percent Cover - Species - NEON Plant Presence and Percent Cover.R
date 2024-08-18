############################################################
#Load Packages
library(tidyverse)


############################################################
#Automatic CSV Importing

#NEON - Plant Presence and Percent Cover
# Percent Cover Data
# - div_1m2Data.csv
NEON.plant.pc.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Multiple/NEON/NEON_presence-cover-plant/div_1m2Data.csv")
str(NEON.plant.pc.dat)


#NEON - Unified Coordinates
NEON.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Multiple/NEON/NEON.Unified.Coordinates.RYF.csv")
str(NEON.coords)

############################################################
#Formatting Data
#NEON - Plant Presence and Percent Cover - Keeping This one For Map - See Crosscheck Below
# Percent Cover Data
# - div_1m2Data.csv
NEON.plant.pc.pcdat<-NEON.plant.pc.dat%>%
  filter(divDataType == "plantSpecies")%>%
  mutate(Year = substr(eventID, nchar(eventID) -3, nchar(eventID)), #Pulling Year from EventID
         Year = case_when(Year == "N022" ~ "2022",                  #Fixing Blank and NO22 Years
                          Year == "" ~ NA,
                          TRUE ~ Year),
         year = as.numeric(Year),
         site = plotID, 
         Parameter = "Percent Cover, Presence/Absence",
         data = "NEON Percent Cover (1m2) and Presence/Absence (10+m2) Plots",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Type",    #placeholder
         std_id = paste0("type", "data_", site),  #placeholder
         plot = subplotID,
         transect = 999,
         date = lubridate::parse_date_time(endDate, orders = c("mdy", "dmy", "ymd")),
         year.y = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = scientificName,
         percent_cover = percentCover)%>%
  left_join(NEON.coords, by = c("site" = "plotID"))%>%
  mutate(latitude = Latitude,
         longitude = Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover, nlcdClass)%>%
  mutate(spp_full_name = gsub("/", " /", spp_full_name),
         spp_full_name = case_when(spp_full_name == ""~  "998",
                                   TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(species = case_when(is.na(species) ~ "998",
                             TRUE ~ species))%>%
  filter(!is.na(percent_cover) & percent_cover>0)%>%
  mutate(notes = "plot marks 'subplotID'; aggregation likely site/plot/y/m/d")

str(NEON.plant.pc.dat)
str(NEON.plant.pc.pcdat)

spp.check<-NEON.plant.pc.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

spp.check2<-NEON.plant.pc.pcdat%>%
  select(genus)%>%
  distinct()

spp.check3<-NEON.plant.pc.pcdat%>%
  select(species)%>%
  distinct()

df_with_capitals <- spp.check %>%
  filter(str_detect(species, "[A-Z]"))

spp.check[4126,]

check1<-NEON.plant.pc.pcdat%>%
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
#Separating Files by Ecosystem Type - NOTE: Removing "pastureHay", "cultivatedCrops", and ""

NEON.plant.pc.forest.pcdat<-NEON.plant.pc.pcdat%>%
  filter(nlcdClass == "deciduousForest" |
           nlcdClass == "evergreenForest" |
           nlcdClass == "mixedForest")%>%
  mutate(ecosystem = "Forests",
         std_id = paste0("forest_", "NEON_", site)  
  )%>%
  select(-nlcdClass)

NEON.plant.pc.sme.pcdat<-NEON.plant.pc.pcdat%>%
  filter(nlcdClass == "woodyWetlands" |
           nlcdClass == "emergentHerbaceousWetlands")%>%
  mutate(ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "NEON_", site)
         )%>%
  select(-nlcdClass)

NEON.plant.pc.grassland.pcdat<-NEON.plant.pc.pcdat%>%
  filter(nlcdClass == "shrubScrub" |
           nlcdClass == "grasslandHerbaceous" |
           nlcdClass == "sedgeHerbaceous" |
           nlcdClass == "dwarfScrub")%>%
  mutate(ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "NEON_", site)
         )%>%
  select(-nlcdClass)


############################################################
#Colmbining Files

NEON.plant.pc.all.pcdat<-bind_rows(
  NEON.plant.pc.forest.pcdat,
  NEON.plant.pc.sme.pcdat,
  NEON.plant.pc.grassland.pcdat
)


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(NEON.plant.pc.all.pcdat, file="Percent.Cover.Species.MULT.NEON.PPPC.csv", row.names = F)
############################################################

#duplicate checking

neon.dups<-NEON.plant.pc.all.pcdat%>%
  select(std_id, latitude, longitude, ecosystem, site)%>%
  distinct()%>%
  mutate(duplicated = duplicated(paste(std_id)) | duplicated(paste(std_id), fromLast = TRUE))





