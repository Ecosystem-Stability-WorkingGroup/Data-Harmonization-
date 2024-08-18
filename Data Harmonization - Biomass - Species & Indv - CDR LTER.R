############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#CDR LTER Bounding Box Coordinates - ENTIRE LTER SITE
CDR.coords<-data.frame(
  Site = c("CDR LTER"),
  Latitude = c((45.44138 + 45.384865)/2),
  Longitude = c((-93.16289 + -93.22445)/2)
)

#Cedar Creek (CDR) LTER - e001: Nitrogen Addition to Undisturbed Vegetation - Aboveground Plant Biomass
# - E001_Aboveground_Biomass.csv
CDR.e001.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/CDR LTER/e001/E001_Aboveground_Biomass.csv")
str(CDR.e001.dat)

#Cedar Creek (CDR) LTER - e307: DroughtNet - International Drought Experiment - Aboveground Biomass
# - e307_sIDE_AbovegroundBiomass_2016-2020.csv
CDR.e307.bio.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Savanna/CDR LTER/e307 - Biomass/e307_sIDE_AbovegroundBiomass_2016-2020.csv")
str(CDR.e307.bio.dat)

#Cedar Creek (CDR) LTER - e307: DroughtNet - International Drought Experiment - Percent Cover
# - e307_sIDE_PercentCover_2016-2020.csv
CDR.e307.pc.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Savanna/CDR LTER/e307 - Percent Cover/e307_sIDE_PercentCover_2016-2020.csv")
str(CDR.e307.pc.dat)

#Cedar Creek (CDR) LTER - e014: Successional Dynamics on a Resampled Chronosequence - Percent Cover
# - e014_Plant species percent cover.csv
CDR.e014.pc.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/CDR LTER/e014/e014_Plant species percent cover.csv")
str(CDR.e014.pc.dat)
#e014 Treatment Table:
CDR.e014.treatments<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/CDR LTER/e014/CDRLTERe014PlotTreatments.csv")
#Fixing UnMatched Transect Values
CDR.e014.treatments<-CDR.e014.treatments%>%
  mutate(Transect = case_when(
    Transect == "G (A)" ~ "A",
    Transect == "R (B)" ~ "B",
    Transect == "W (C)" ~ "C",
    Transect == "Y (D)" ~ "D",
    Transect == "(E)" ~ "E",
    Transect == "(F)" ~ "F",
    TRUE ~ Transect
  ))

#Cedar Creek (CDR) LTER - e054: Old-Field Chronosequence: Plant Productivity - Aboveground Plant Biomass
# - e054_Plant aboveground biomass.csv
CDR.e054.bio.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/CDR LTER/e054/e054_Plant aboveground biomass.csv")
str(CDR.e054.bio.dat)

#Cedar Creek (CDR) LTER - e245: Influence of Natural Enemies on Plant Community Composition and Productivity - Aboveground Biomass
# - E245 Enemy Removal Aboveground Biomass.csv
CDR.e245.bio.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/CDR LTER/e245 - Biomass/E245 Enemy Removal Aboveground Biomass.csv")
str(CDR.e245.bio.dat)

#Cedar Creek (CDR) LTER - e245: Influence of Natural Enemies on Plant Community Composition and Productivity - Percent Cover
# - e245_Plant species percent cover data.csv
CDR.e245.pc.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/CDR LTER/e245 - Percent Cover/e245_Plant species percent cover data.csv")
str(CDR.e245.pc.dat)

#Cedar Creek (CDR) LTER - e247: Nutrient Network - Plant Species Composition Percent Cover
# - e247_Plant Species Composition percent cover.csv
CDR.e247.pc.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/CDR LTER/e247/e247_Plant Species Composition percent cover.csv")
str(CDR.e247.pc.dat)

#Cedar Creek (CDR) LTER - e247: Nutrient Network - Plant Biomass
# - e247_Aboveground Standing Crop Biomass.csv
CDR.e247.bio.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/CDR LTER/e247 - Biomass/e247_Aboveground Standing Crop Biomass.csv")
str(CDR.e247.bio.dat)

#Cedar Creek (CDR) LTER - e306: DroughtNet - International Drought Experiment at tIDE - Percent Cover
# - e306_tIDE_PercentCover_2016-2020.csv
CDR.e306.pc.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/CDR LTER/e306/e306_tIDE_PercentCover_2016-2020.csv")
str(CDR.e306.pc.dat)

############################################################
#Trimming Data to Control Sites in Experimental Data

#Cedar Creek (CDR) LTER - e001: Nitrogen Addition to Undisturbed Vegetation - Aboveground Plant Biomass
# - E001_Aboveground_Biomass.csv
CDR.e001.dat<-CDR.e001.dat%>%
  filter(NTrt == 1 | NTrt == 9)%>%
  filter(FencedAfterFall2004 == 0)

#Cedar Creek (CDR) LTER - e307: DroughtNet - International Drought Experiment - Aboveground Biomass
# - e307_sIDE_AbovegroundBiomass_2016-2020.csv
CDR.e307.bio.dat<-CDR.e307.bio.dat%>%
  filter(treatment=="control")

#Cedar Creek (CDR) LTER - e307: DroughtNet - International Drought Experiment - Percent Cover
# - e307_sIDE_PercentCover_2016-2020.csv
CDR.e307.pc.dat<-CDR.e307.pc.dat%>%
  filter(treatment=="control")

#Cedar Creek (CDR) LTER - e014: Successional Dynamics on a Resampled Chronosequence - Percent Cover
# - e014_Plant species percent cover.csv
CDR.e014.pc.dat<-CDR.e014.pc.dat%>%
  mutate(OldField = as.character(OldField))%>%
  left_join(CDR.e014.treatments, by = c("OldField" = "Old.Field", "Transect"))%>%
  filter(Burn.Treatment==0)

#Cedar Creek (CDR) LTER - e054: Old-Field Chronosequence: Plant Productivity - Aboveground Plant Biomass
# - e054_Plant aboveground biomass.csv
CDR.e054.bio.dat<-CDR.e054.bio.dat%>%
  filter(BurnTrt == 0)

#Cedar Creek (CDR) LTER - e245: Influence of Natural Enemies on Plant Community Composition and Productivity - Aboveground Biomass
# - E245 Enemy Removal Aboveground Biomass.csv
CDR.e245.bio.dat<-CDR.e245.bio.dat%>%
  filter(TreatmentCode=="Control")

#Cedar Creek (CDR) LTER - e245: Influence of Natural Enemies on Plant Community Composition and Productivity - Percent Cover
# e245_Plant species percent cover data.csv
CDR.e245.pc.dat<-CDR.e245.pc.dat%>%
  filter(Treatment=="Control")

#Cedar Creek (CDR) LTER - e247: Nutrient Network - Plant Species Composition Percent Cover
# - e247_Plant Species Composition percent cover.csv
CDR.e247.pc.dat<-CDR.e247.pc.dat%>%
  filter(trt == "Control")

#Cedar Creek (CDR) LTER - e247: Nutrient Network - Plant Biomass
# - e247_Aboveground Standing Crop Biomass.csv
CDR.e247.bio.dat<-CDR.e247.bio.dat%>%
  filter(trt == "Control")

#Cedar Creek (CDR) LTER - e306: DroughtNet - International Drought Experiment at tIDE - Percent Cover
# - e306_tIDE_PercentCover_2016-2020.csv
CDR.e306.pc.dat<-CDR.e306.pc.dat%>%
  filter(shelter == "none")%>%
  filter(fertilized == "no")%>%
  filter(fomerly_irrigated == "no")
############################################################
#Formatting Data

#Cedar Creek (CDR) LTER - e001: Nitrogen Addition to Undisturbed Vegetation - Aboveground Plant Biomass
# - E001_Aboveground_Biomass.csv
CDR.e001.biodat<-CDR.e001.dat%>%
  mutate(Site = "CDR LTER")%>%
  left_join(CDR.coords, by = c("Site"))%>%
  mutate(site = Site,
         Parameter = "Biomass",
         data = "CDR LTER - e001 Biomass",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "CDR.LTER_", site),
         plot = Plot,
         transect = Field,
        #date = lubridate::parse_date_time(XXXX, orders = c("mdy", "dmy", "ymd")),
         year = Year,
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         biomass = Biomass.g.m2.,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  mutate(genus = case_when(spp_full_name == "Mosses & lichens" |
                             spp_full_name == "Woody debris" |
                             spp_full_name == "woody debris" |
                             spp_full_name == "Unknown forb 1" |
                             spp_full_name == "Unknown forb 2" |
                             spp_full_name == "miscellaneous forb" |
                             spp_full_name == "Unknown grass 1" |
                             spp_full_name == "Bare ground" |
                             spp_full_name == "ant hill" |
                             spp_full_name == "Miscellaneous litter" |
                             spp_full_name == "Man made object" |
                             spp_full_name == "gopher mound" |
                             spp_full_name == "ant mound" |
                             spp_full_name == "gooher mound" |
                             spp_full_name == "Pine needles" |
                             spp_full_name == "Forb seedlings" |
                             spp_full_name == "Grass seedlings" |
                             spp_full_name == "tree" |
                             spp_full_name == "Fungi" |
                             spp_full_name == "Woody" |
                             spp_full_name == "dung" |
                             spp_full_name == "Leaves" |
                             spp_full_name == "gophermound" |
                             spp_full_name == "anthill" |
                             spp_full_name == "Mosses" |
                             spp_full_name == "Miscellaneous sp." |
                             spp_full_name == "Unknown woody 1" |
                             spp_full_name == "unknown tree 1" |
                             spp_full_name == "Moss" |
                             spp_full_name == "Litter" |
                             spp_full_name == "unknown forb 1" |
                             spp_full_name == "Mosses & lichens 2" |
                             spp_full_name == "Miscellaneous grasses" |
                             spp_full_name == "Miscellaneous herbs" |
                             spp_full_name == "Miscellaneous woody plants" |
                             spp_full_name == "Miscellaneous forb" |
                             spp_full_name == "Miscellaneous legumes" |
                             spp_full_name == "Miscellaneous sedges" |
                             spp_full_name == "Mosses/lichens" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Mosses & lichens" |
                               spp_full_name == "Woody debris" |
                               spp_full_name == "woody debris" |
                               spp_full_name == "Unknown forb 1" |
                               spp_full_name == "Unknown forb 2" |
                               spp_full_name == "miscellaneous forb" |
                               spp_full_name == "Unknown grass 1" |
                               spp_full_name == "Bare ground" |
                               spp_full_name == "ant hill" |
                               spp_full_name == "Miscellaneous litter" |
                               spp_full_name == "Man made object" |
                               spp_full_name == "gopher mound" |
                               spp_full_name == "ant mound" |
                               spp_full_name == "gooher mound" |
                               spp_full_name == "Pine needles" |
                               spp_full_name == "Forb seedlings" |
                               spp_full_name == "Grass seedlings" |
                               spp_full_name == "tree" |
                               spp_full_name == "Fungi" |
                               spp_full_name == "Woody" |
                               spp_full_name == "dung" |
                               spp_full_name == "Leaves" |
                               spp_full_name == "gophermound" |
                               spp_full_name == "anthill" |
                               spp_full_name == "Mosses" |
                               spp_full_name == "Miscellaneous sp." |
                               spp_full_name == "Unknown woody 1" |
                               spp_full_name == "unknown tree 1" |
                               spp_full_name == "Moss" |
                               spp_full_name == "Mosses/lichens" |
                               spp_full_name == "unknown forb 1" |
                               spp_full_name == "Mosses & lichens 2" |
                               spp_full_name == "Miscellaneous herbs" |
                               spp_full_name == "Miscellaneous woody plants" |
                               spp_full_name == "Miscellaneous forb" |
                               spp_full_name == "Miscellaneous legumes" |
                               spp_full_name == "Miscellaneous sedges" |
                               spp_full_name == "Litter" |
                               species == "sp" |
                               species == "sp." |
                               is.na(species) ~ "998",
                             TRUE ~ species),
         species = case_when(spp_full_name == "Artemisia (caudata) campestris" ~ "campestris",
                             spp_full_name == "Plantago (purshii) patagonica" ~ "patagonica",
                             spp_full_name == "Euphorbia (supina) maculata" ~ "maculata",
                             spp_full_name == "Achillea millefolium(lanulosa)" ~ "millefolium",
                             TRUE ~ species),
         genus = str_to_title(genus))%>%
  filter(biomass > 0)%>%
  mutate(notes = "plot marks 'Plot', transect marks 'Field', aggregation likely site/plot/transect/y/m/d")


str(CDR.e001.dat)
str(CDR.e001.biodat)

#Species checking
spp.check<-CDR.e001.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-CDR.e001.biodat%>%
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
write.csv(CDR.e001.biodat, file="Biomass.Species.Indv.GrassSav.CDRLTER.e001.csv", row.names = F)
############################################################


#Cedar Creek (CDR) LTER - e054: Old-Field Chronosequence: Plant Productivity - Aboveground Plant Biomass
# - e054_Plant aboveground biomass.csv
CDR.e054.bio.biodat<-CDR.e054.bio.dat%>%
  mutate(Site = "CDR LTER")%>%
  left_join(CDR.coords, by = c("Site"))%>%
  mutate(site = Site,
         Parameter = "Biomass",
         data = "CDR LTER - e054 Biomass",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "CDR.LTER_", site),
         plot = paste0(OldField, "_", Plot),
         transect = Transect,
         #date = lubridate::parse_date_time(XXXX, orders = c("mdy", "dmy", "ymd")),
         year = Year,
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         biomass = Biomass..g.m.2.,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  mutate(genus = case_when(spp_full_name == "Mosses & lichens" |
                             spp_full_name == "Woody debris" |
                             spp_full_name == "woody debris" |
                             spp_full_name == "Unknown forb 1" |
                             spp_full_name == "Unknown forb 2" |
                             spp_full_name == "miscellaneous forb" |
                             spp_full_name == "Unknown grass 1" |
                             spp_full_name == "Bare ground" |
                             spp_full_name == "ant hill" |
                             spp_full_name == "Miscellaneous litter" |
                             spp_full_name == "Man made object" |
                             spp_full_name == "gopher mound" |
                             spp_full_name == "ant mound" |
                             spp_full_name == "gooher mound" |
                             spp_full_name == "Pine needles" |
                             spp_full_name == "Forb seedlings" |
                             spp_full_name == "Grass seedlings" |
                             spp_full_name == "tree" |
                             spp_full_name == "Fungi" |
                             spp_full_name == "Woody" |
                             spp_full_name == "dung" |
                             spp_full_name == "Leaves" |
                             spp_full_name == "gophermound" |
                             spp_full_name == "anthill" |
                             spp_full_name == "Mosses" |
                             spp_full_name == "Miscellaneous sp." |
                             spp_full_name == "Unknown woody 1" |
                             spp_full_name == "unknown tree 1" |
                             spp_full_name == "Moss" |
                             spp_full_name == "Litter" |
                             spp_full_name == "unknown forb 1" |
                             spp_full_name == "Mosses & lichens 2" |
                             spp_full_name == "Miscellaneous grasses" |
                             spp_full_name == "Miscellaneous herbs" |
                             spp_full_name == "Miscellaneous woody plants" |
                             spp_full_name == "Miscellaneous forb" |
                             spp_full_name == "Miscellaneous legumes" |
                             spp_full_name == "Miscellaneous sedges" |
                             spp_full_name == "Miscellaneous  woody" |
                             spp_full_name == "moses & lichens" |
                             spp_full_name == "Ambro Art" |
                             spp_full_name == "Pine cones" |
                             spp_full_name == "Miscellaneous forb 1" |
                             spp_full_name == "Miscellaneous forb 2" |
                             spp_full_name == "Miscellaneous grass" |
                             spp_full_name == "Miscellaneous herbs 2" |
                             spp_full_name == "Miscellaneous liter" |
                             spp_full_name == "Corn litter" |
                             spp_full_name == "Miscellaneous litter (pine)" |
                             spp_full_name == "pine needles" |
                             spp_full_name == "Pine twigs" |
                             spp_full_name == "Miscellaneous woody tree" |
                             spp_full_name == "Miscellaneous woody" |
                             spp_full_name == "Miscellaneous woody 2" |
                             spp_full_name == "Miscellaneous woody 1" |
                             spp_full_name == "Miscellaneous woody plants 1" |
                             spp_full_name == "Miscellaneous woody plants 2" |
                             spp_full_name == "Miscellaneous woody litter" |
                             spp_full_name == "Mosses/lichens" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Mosses & lichens" |
                               spp_full_name == "Woody debris" |
                               spp_full_name == "woody debris" |
                               spp_full_name == "Unknown forb 1" |
                               spp_full_name == "Unknown forb 2" |
                               spp_full_name == "miscellaneous forb" |
                               spp_full_name == "Unknown grass 1" |
                               spp_full_name == "Bare ground" |
                               spp_full_name == "ant hill" |
                               spp_full_name == "Miscellaneous litter" |
                               spp_full_name == "Man made object" |
                               spp_full_name == "gopher mound" |
                               spp_full_name == "ant mound" |
                               spp_full_name == "gooher mound" |
                               spp_full_name == "Pine needles" |
                               spp_full_name == "Forb seedlings" |
                               spp_full_name == "Grass seedlings" |
                               spp_full_name == "tree" |
                               spp_full_name == "Fungi" |
                               spp_full_name == "Woody" |
                               spp_full_name == "dung" |
                               spp_full_name == "Leaves" |
                               spp_full_name == "gophermound" |
                               spp_full_name == "anthill" |
                               spp_full_name == "Mosses" |
                               spp_full_name == "Miscellaneous sp." |
                               spp_full_name == "Unknown woody 1" |
                               spp_full_name == "unknown tree 1" |
                               spp_full_name == "Moss" |
                               spp_full_name == "Mosses/lichens" |
                               spp_full_name == "unknown forb 1" |
                               spp_full_name == "Mosses & lichens 2" |
                               spp_full_name == "Miscellaneous herbs" |
                               spp_full_name == "Miscellaneous woody plants" |
                               spp_full_name == "Miscellaneous forb" |
                               spp_full_name == "Miscellaneous legumes" |
                               spp_full_name == "Miscellaneous sedges" |
                               spp_full_name == "Miscellaneous  woody" |
                               spp_full_name == "moses & lichens" |
                               spp_full_name == "Ambro Art" |
                               spp_full_name == "Pine cones" |
                               spp_full_name == "Miscellaneous forb 1" |
                               spp_full_name == "Miscellaneous forb 2" |
                               spp_full_name == "Miscellaneous grass" |
                               spp_full_name == "Miscellaneous herbs 2" |
                               spp_full_name == "Miscellaneous liter" |
                               spp_full_name == "Corn litter" |
                               spp_full_name == "Miscellaneous litter (pine)" |
                               spp_full_name == "pine needles" |
                               spp_full_name == "Pine twigs" |
                               spp_full_name == "Miscellaneous woody tree" |
                               spp_full_name == "Miscellaneous woody" |
                               spp_full_name == "Miscellaneous woody 2" |
                               spp_full_name == "Miscellaneous woody 1" |
                               spp_full_name == "Miscellaneous woody plants 1" |
                               spp_full_name == "Miscellaneous woody plants 2" |
                               spp_full_name == "Miscellaneous woody litter" |
                               spp_full_name == "Litter" |
                               species == "sp" |
                               species == "sp." |
                               is.na(species) ~ "998",
                             TRUE ~ species),
         species = case_when(spp_full_name == "Artemisia (caudata) campestris" ~ "campestris",
                             spp_full_name == "Plantago (purshii) patagonica" ~ "patagonica",
                             spp_full_name == "Euphorbia (supina) maculata" ~ "maculata",
                             spp_full_name == "Achillea millefolium(lanulosa)" ~ "millefolium",
                             TRUE ~ species),
         genus = str_to_title(genus))%>%
  filter(biomass > 0)%>%
  mutate(notes = "plot marks 'OldField/Plot', transect marks 'Transect', aggregation likely site/plot/y/m/d")

str(CDR.e054.bio.dat)
str(CDR.e054.bio.biodat)

#Species checking
spp.check<-CDR.e054.bio.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-CDR.e054.bio.biodat%>%
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
write.csv(CDR.e054.bio.biodat, file="Biomass.Species.Indv.GrassSav.CDRLTER.e054.csv", row.names = F)
############################################################


#Cedar Creek (CDR) LTER - e245: Influence of Natural Enemies on Plant Community Composition and Productivity - Aboveground Biomass
# - E245 Enemy Removal Aboveground Biomass.csv
CDR.e245.bio.biodat<-CDR.e245.bio.dat%>%
  mutate(Site = "CDR LTER")%>%
  left_join(CDR.coords, by = c("Site"))%>%
  mutate(site = Site,
         Parameter = "Biomass",
         data = "CDR LTER - e245 Biomass",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "CDR.LTER_", site),
         plot = paste0(Plot, "_", Subplot),
         transect = Strip,
         transect = case_when(is.na(transect) ~ 999,
                              TRUE ~ transect),
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         biomass = Mass..g.m.2.,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  mutate(genus = case_when(spp_full_name == "Mosses & lichens" |
                             spp_full_name == "Woody debris" |
                             spp_full_name == "woody debris" |
                             spp_full_name == "Unknown forb 1" |
                             spp_full_name == "Unknown forb 2" |
                             spp_full_name == "miscellaneous forb" |
                             spp_full_name == "Unknown grass 1" |
                             spp_full_name == "Bare ground" |
                             spp_full_name == "ant hill" |
                             spp_full_name == "Miscellaneous litter" |
                             spp_full_name == "Man made object" |
                             spp_full_name == "gopher mound" |
                             spp_full_name == "ant mound" |
                             spp_full_name == "gooher mound" |
                             spp_full_name == "Pine needles" |
                             spp_full_name == "Forb seedlings" |
                             spp_full_name == "Grass seedlings" |
                             spp_full_name == "tree" |
                             spp_full_name == "Fungi" |
                             spp_full_name == "Woody" |
                             spp_full_name == "dung" |
                             spp_full_name == "Leaves" |
                             spp_full_name == "gophermound" |
                             spp_full_name == "anthill" |
                             spp_full_name == "Mosses" |
                             spp_full_name == "Miscellaneous sp." |
                             spp_full_name == "Unknown woody 1" |
                             spp_full_name == "unknown tree 1" |
                             spp_full_name == "Moss" |
                             spp_full_name == "Litter" |
                             spp_full_name == "unknown forb 1" |
                             spp_full_name == "Mosses & lichens 2" |
                             spp_full_name == "Miscellaneous grasses" |
                             spp_full_name == "Miscellaneous herbs" |
                             spp_full_name == "Miscellaneous woody plants" |
                             spp_full_name == "Miscellaneous forb" |
                             spp_full_name == "Miscellaneous legumes" |
                             spp_full_name == "Miscellaneous sedges" |
                             spp_full_name == "Miscellaneous  woody" |
                             spp_full_name == "moses & lichens" |
                             spp_full_name == "Ambro Art" |
                             spp_full_name == "Pine cones" |
                             spp_full_name == "Miscellaneous forb 1" |
                             spp_full_name == "Miscellaneous forb 2" |
                             spp_full_name == "Miscellaneous grass" |
                             spp_full_name == "Miscellaneous herbs 2" |
                             spp_full_name == "Miscellaneous liter" |
                             spp_full_name == "Corn litter" |
                             spp_full_name == "Miscellaneous litter (pine)" |
                             spp_full_name == "pine needles" |
                             spp_full_name == "Pine twigs" |
                             spp_full_name == "Miscellaneous woody tree" |
                             spp_full_name == "Miscellaneous woody" |
                             spp_full_name == "Miscellaneous woody 2" |
                             spp_full_name == "Miscellaneous woody 1" |
                             spp_full_name == "Miscellaneous woody plants 1" |
                             spp_full_name == "Miscellaneous woody plants 2" |
                             spp_full_name == "Miscellaneous woody litter" |
                             spp_full_name == "Unsorted Biomass" |
                             spp_full_name == "Miscellaneous forbs" |
                             spp_full_name == "C3 grasses" |
                             spp_full_name == "C4 grasses" |
                             spp_full_name == "Miscellaneous grasses" |
                             spp_full_name == "miscellaneous litter" |
                             spp_full_name == "Miscellaneous sp. 2" |
                             spp_full_name == "Mosses/lichens" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Mosses & lichens" |
                               spp_full_name == "Woody debris" |
                               spp_full_name == "woody debris" |
                               spp_full_name == "Unknown forb 1" |
                               spp_full_name == "Unknown forb 2" |
                               spp_full_name == "miscellaneous forb" |
                               spp_full_name == "Unknown grass 1" |
                               spp_full_name == "Bare ground" |
                               spp_full_name == "ant hill" |
                               spp_full_name == "Miscellaneous litter" |
                               spp_full_name == "Man made object" |
                               spp_full_name == "gopher mound" |
                               spp_full_name == "ant mound" |
                               spp_full_name == "gooher mound" |
                               spp_full_name == "Pine needles" |
                               spp_full_name == "Forb seedlings" |
                               spp_full_name == "Grass seedlings" |
                               spp_full_name == "tree" |
                               spp_full_name == "Fungi" |
                               spp_full_name == "Woody" |
                               spp_full_name == "dung" |
                               spp_full_name == "Leaves" |
                               spp_full_name == "gophermound" |
                               spp_full_name == "anthill" |
                               spp_full_name == "Mosses" |
                               spp_full_name == "Miscellaneous sp." |
                               spp_full_name == "Unknown woody 1" |
                               spp_full_name == "unknown tree 1" |
                               spp_full_name == "Moss" |
                               spp_full_name == "Mosses/lichens" |
                               spp_full_name == "unknown forb 1" |
                               spp_full_name == "Mosses & lichens 2" |
                               spp_full_name == "Miscellaneous herbs" |
                               spp_full_name == "Miscellaneous woody plants" |
                               spp_full_name == "Miscellaneous forb" |
                               spp_full_name == "Miscellaneous legumes" |
                               spp_full_name == "Miscellaneous sedges" |
                               spp_full_name == "Miscellaneous  woody" |
                               spp_full_name == "moses & lichens" |
                               spp_full_name == "Ambro Art" |
                               spp_full_name == "Pine cones" |
                               spp_full_name == "Miscellaneous forb 1" |
                               spp_full_name == "Miscellaneous forb 2" |
                               spp_full_name == "Miscellaneous grass" |
                               spp_full_name == "Miscellaneous herbs 2" |
                               spp_full_name == "Miscellaneous liter" |
                               spp_full_name == "Corn litter" |
                               spp_full_name == "Miscellaneous litter (pine)" |
                               spp_full_name == "pine needles" |
                               spp_full_name == "Pine twigs" |
                               spp_full_name == "Miscellaneous woody tree" |
                               spp_full_name == "Miscellaneous woody" |
                               spp_full_name == "Miscellaneous woody 2" |
                               spp_full_name == "Miscellaneous woody 1" |
                               spp_full_name == "Miscellaneous woody plants 1" |
                               spp_full_name == "Miscellaneous woody plants 2" |
                               spp_full_name == "Miscellaneous woody litter" |
                               spp_full_name == "Unsorted Biomass" |
                               spp_full_name == "Miscellaneous forbs" |
                               spp_full_name == "C3 grasses" |
                               spp_full_name == "C4 grasses" |
                               spp_full_name == "Miscellaneous grasses" |
                               spp_full_name == "miscellaneous litter" |
                               spp_full_name == "Miscellaneous sp. 2" |
                               spp_full_name == "Litter" |
                               species == "sp" |
                               species == "sp." |
                               is.na(species) ~ "998",
                             TRUE ~ species),
         species = case_when(spp_full_name == "Artemisia (caudata) campestris" ~ "campestris",
                             spp_full_name == "Plantago (purshii) patagonica" ~ "patagonica",
                             spp_full_name == "Euphorbia (supina) maculata" ~ "maculata",
                             spp_full_name == "Achillea millefolium(lanulosa)" ~ "millefolium",
                             TRUE ~ species),
         genus = str_to_title(genus))%>%
  filter(biomass > 0)%>%
  mutate(notes = "plot marks 'Plot/Subplot', transect marks 'Strip', aggregation likely site/plot/y/m/d")




str(CDR.e245.bio.dat)
str(CDR.e245.bio.biodat)

#Species checking
spp.check<-CDR.e245.bio.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-CDR.e245.bio.biodat%>%
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
write.csv(CDR.e245.bio.biodat, file="Biomass.Species.Indv.GrassSav.CDRLTER.e245.csv", row.names = F)
############################################################
