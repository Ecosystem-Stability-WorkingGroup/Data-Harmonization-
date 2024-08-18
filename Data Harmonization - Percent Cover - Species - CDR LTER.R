############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#CDR LTER Bounding Box Coordinates - ENTIRE LTER SITE
CDR.coords<-data.frame(
  Site = c("CDR.LTER"),
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

#Cedar Creek (CDR) LTER - e014: Successional Dynamics on a Resampled Chronosequence - Percent Cover
# - e014_Plant species percent cover.csv
CDR.e014.pc.pcdat<-CDR.e014.pc.dat%>%
  filter(Percent.Cover > 0)%>%       #Filter out zero cover vals
  mutate(Site = "CDR.LTER")%>%
  left_join(CDR.coords, by = c("Site"))%>%
  mutate(Parameter = "Percent Cover",
         Data = "CDR LTER - e014 Percent Cover",
         Taxa = "Vegetation",
         Specificity = "Species",
         Ecosystem = "Grassland",
         Latitude = Latitude,
         Longitude =  Longitude,
         std_id = paste0("grass.sav_", "CDR.LTER_", Site),
         transect = paste0(Transect, "_", Transect.Number),
         plot = paste0(OldField, "_", Plot),
         month = 999,
         day = 999)%>%
  select(std_id, Data, Ecosystem, Site, plot, transect, Latitude, Longitude, 
         Year, month, day, Species, Percent.Cover)%>%
  rename("data" = "Data",
         "ecosystem" = "Ecosystem",
         "site" = "Site",
         "latitude" = "Latitude",
         "longitude" = "Longitude",
         "year" = "Year",
         "spp_full_name" = "Species",
         "percent_cover" = "Percent.Cover")%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
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
                             spp_full_name == "Mosses/lichens" |
                             species == "sp" |
                             species == "sp." ~ "998",
                           TRUE ~ species),
         species = case_when(spp_full_name == "Artemisia (caudata) campestris" ~ "campestris",
                             spp_full_name == "Plantago (purshii) patagonica" ~ "patagonica",
                             spp_full_name == "Euphorbia (supina) maculata" ~ "maculata",
                             TRUE ~ species),
         genus = str_to_title(genus))%>%
  mutate(notes = "plot marks 'Old Field / Plot', transect marks 'Transect / Transect Number', aggregation likely at site/plot/transect - some individual values with >100% cover")

spp.check<-CDR.e014.pc.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


str(CDR.e014.pc.dat)

check1<-CDR.e014.pc.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()


check2<-CDR.e014.pc.dat%>%group_by(Year, OldField, Transect, Plot, Transect.Number, Burn.Treatment)%>%summarise(check = sum(Percent.Cover))
check3<-CDR.e014.pc.dat%>%
  filter(Year == 2022&
         OldField == "4"&
         Transect == "W"& 
         Plot == 1 &
         Transect.Number == 3 &
         Burn.Treatment ==0)



str(CDR.e014.pc.pcdat)

table(CDR.e014.pc.dat$OldField, CDR.e014.pc.dat$Transect.Number)


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(CDR.e014.pc.pcdat, file="Percent.Cover.Species.GrassSav.CDRLTER.e014.csv", row.names = F)
############################################################


#Cedar Creek (CDR) LTER - e245: Influence of Natural Enemies on Plant Community Composition and Productivity - Percent Cover
# e245_Plant species percent cover data.csv
CDR.e245.pc.pcdat<-CDR.e245.pc.dat%>%
  filter(Percent.Cover > 0)%>%       #Filter out zero cover vals
  mutate(Site = "CDR.LTER")%>%
  left_join(CDR.coords, by = c("Site"))%>%
  mutate(Parameter = "Percent Cover",
         Data = "CDR LTER - e245 Percent Cover",
         Taxa = "Vegetation",
         Specificity = "Species",
         Ecosystem = "Grassland",
         Latitude = Latitude,
         Longitude =  Longitude,
         std_id = paste0("grass.sav_", "CDR.LTER_", Site),
         plot = Plot,
         transect = 999,
         date = as.Date(Date, format = "%m/%d/%Y"),  
         month = as.numeric(format(date, "%m")),
         day = as.numeric(format(date, "%d")))%>%
  select(std_id, Data, Ecosystem, Site, plot, transect, Latitude, Longitude, 
         Year, month, day, Species, Percent.Cover)%>%
  rename("data" = "Data",
         "ecosystem" = "Ecosystem",
         "site" = "Site",
         "latitude" = "Latitude",
         "longitude" = "Longitude",
         "year" = "Year",
         "spp_full_name" = "Species",
         "percent_cover" = "Percent.Cover")%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
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
                             spp_full_name == "unknown forb 1" |
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
                               species == "sp" |
                               species == "sp." |
                               is.na(species) ~ "998",
                             TRUE ~ species),
         species = case_when(spp_full_name == "Artemisia (caudata) campestris" ~ "campestris",
                             spp_full_name == "Plantago (purshii) patagonica" ~ "patagonica",
                             spp_full_name == "Euphorbia (supina) maculata" ~ "maculata",
                             TRUE ~ species),
         genus = str_to_title(genus))%>%
  mutate(notes = "plot marks 'Plot', aggregation at site/pot/y/m/d")


spp.check<-CDR.e245.pc.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


str(CDR.e245.pc.pcdat)
str(CDR.e245.pc.dat)

check1<-CDR.e245.pc.pcdat%>%group_by(site, plot, year, month)%>%summarise(check = sum(percent_cover))
check2<-CDR.e245.pc.dat%>%filter(Plot == 2 & Year == 2009)

check1<-CDR.e245.pc.pcdat%>%
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
write.csv(CDR.e245.pc.pcdat, file="Percent.Cover.Species.GrassSav.CDRLTER.e245.csv", row.names = F)
############################################################


capitalize_first <- function(x) {
  tolower(substring(x, 2)) %>%
    paste0(toupper(substring(x, 1, 1)), .)
}

#Cedar Creek (CDR) LTER - e247: Nutrient Network - Plant Species Composition Percent Cover
# - e247_Plant Species Composition percent cover.csv
CDR.e247.pc.pcdat<-CDR.e247.pc.dat%>%
  filter(max_cover > 0)%>%
  mutate(Site = "CDR.LTER")%>%
  left_join(CDR.coords, by = c("Site"))%>%
  mutate(Parameter = "Percent Cover",
         data = "CDR LTER - e247 NutNet Percent Cover",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "CDR.LTER_", Site),
         plot = paste0(plot, "_", subplot),
         transect = block,
         month = 999,
         day = 999,
         spp_full_name = sapply(Taxon, capitalize_first),
         percent_cover = max_cover,
         latitude = Latitude,
         longitude =  Longitude,
         site = Site)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
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
                             spp_full_name == "Unknown " |
                             spp_full_name == "Bryophyte" |
                             spp_full_name == "Lichen" |
                             spp_full_name == "Other" |
                             spp_full_name == "Ground" |
                             spp_full_name == "Unknown sp." |
                             spp_full_name == "Fungi sp." |
                             spp_full_name == "Other animal diggings" |
                             spp_full_name == "Unknown cupressaceae sp." |
                             spp_full_name == "Unknown fabaceae" |
                             spp_full_name == "Other litter" |
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
                               spp_full_name == "Unknown " |
                               spp_full_name == "Bryophyte" |
                               spp_full_name == "Lichen" |
                               spp_full_name == "Other" |
                               spp_full_name == "Litter" |
                               spp_full_name == "Ground" |
                               spp_full_name == "Unknown sp." |
                               spp_full_name == "Fungi sp." |
                               spp_full_name == "Other animal diggings" |
                               spp_full_name == "Unknown cupressaceae sp." |
                               spp_full_name == "Unknown fabaceae" |
                               spp_full_name == "Other litter" |
                               species == "sp" |
                               species == "sp." |
                               is.na(species) ~ "998",
                             TRUE ~ species),
         species = case_when(spp_full_name == "Artemisia (caudata) campestris" ~ "campestris",
                             spp_full_name == "Plantago (purshii) patagonica" ~ "patagonica",
                             spp_full_name == "Euphorbia (supina) maculata" ~ "maculata",
                             TRUE ~ species),
         genus = str_to_title(genus))%>%
  mutate(notes = "plot marks 'plot/subplot', transect marks 'block', aggregation at site/plot/subplot/y/m/d")





str(CDR.e247.pc.dat)
str(CDR.e247.pc.pcdat)

spp.check<-CDR.e247.pc.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-CDR.e247.pc.pcdat%>%
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
write.csv(CDR.e247.pc.pcdat, file="Percent.Cover.Species.GrassSav.CDRLTER.e247.csv", row.names = F)
############################################################


#Cedar Creek (CDR) LTER - e306: DroughtNet - International Drought Experiment at tIDE - Percent Cover
# - e306_tIDE_PercentCover_2016-2020.csv
CDR.e306.pc.pcdat<-CDR.e306.pc.dat%>%
  filter(cover > 0)%>%       #Filter out zero cover vals
  mutate(Site = "CDR.LTER")%>%
  left_join(CDR.coords, by = c("Site"))%>%
  mutate(Date = as.Date(date, "%Y-%m-%d"),
         year = as.numeric(format(Date, "%Y")),
         month = as.numeric(format(Date, "%m")),
         day = as.numeric(format(Date, "%d")),
         Parameter = "Biomass",
         Data = "CDR LTER - e306 Percent Cover",
         Taxa = "Vegetation",
         Specificity = "Species",
         Ecosystem = "Grassland",
         Latitude = Latitude,
         Longitude =  Longitude,
         std_id = paste0("grass.sav_", "CDR.LTER_", Site),
         plot = plot,
         transect = subplot)%>%
  rename("data" = "Data",
         "ecosystem" = "Ecosystem",
         "site" = "Site",
         "latitude" = "Latitude",
         "longitude" = "Longitude",
         "spp_full_name" = "taxa",
         "percent_cover" = "cover")%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = "_", 
    remove = FALSE)%>%
  mutate(spp_full_name = gsub("_", " ", spp_full_name))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
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
                               spp_full_name == "Litter" |
                               species == "sp" |
                               species == "sp." |
                               is.na(species) ~ "998",
                             TRUE ~ species),
         species = case_when(spp_full_name == "Artemisia (caudata) campestris" ~ "campestris",
                             spp_full_name == "Plantago (purshii) patagonica" ~ "patagonica",
                             spp_full_name == "Euphorbia (supina) maculata" ~ "maculata",
                             TRUE ~ species),
         genus = str_to_title(genus))%>%
  mutate(notes = "plot marks 'plot', transect marks 'subplot', aggregation at site/plot/subplot/y/m/d")
  

spp.check<-CDR.e306.pc.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-CDR.e306.pc.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

  
str(CDR.e306.pc.dat)
str(CDR.e306.pc.pcdat)
check1<-CDR.e306.pc.pcdat%>%group_by(year,  month, site, plot, transect)%>%summarise(check = sum(percent_cover))
check2<-CDR.e306.pc.pcdat%>%filter(year == "2016" & month == 8 & site == "CDR_LTER" & plot == "152_Middle" & transect == 999)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(CDR.e306.pc.pcdat, file="Percent.Cover.Species.GrassSav.CDRLTER.e306.csv", row.names = F)
############################################################


#Cedar Creek (CDR) LTER - e307: DroughtNet - International Drought Experiment - Percent Cover
# - e307_sIDE_PercentCover_2016-2020.csv
CDR.e307.pc.pcdat<-CDR.e307.pc.dat%>%
  filter(cover > 0)%>%
  mutate(Site = "CDR.LTER")%>%
  left_join(CDR.coords, by = c("Site"))%>%
  mutate(Parameter = "Biomass",
         data = "CDR LTER - e307 Percent Cover",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "CDR.LTER_", Site),
         plot = plot,
         transect = subplot,
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = taxa,
         percent_cover = cover,
         site = Site,
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = "_", 
    remove = FALSE)%>%
  mutate(spp_full_name = gsub("_", " ", spp_full_name))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
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
                               spp_full_name == "Litter" |
                               species == "sp" |
                               species == "sp." |
                               is.na(species) ~ "998",
                             TRUE ~ species),
         species = case_when(spp_full_name == "Artemisia (caudata) campestris" ~ "campestris",
                             spp_full_name == "Plantago (purshii) patagonica" ~ "patagonica",
                             spp_full_name == "Euphorbia (supina) maculata" ~ "maculata",
                             TRUE ~ species),
         genus = str_to_title(genus))%>%
  mutate(notes = "plot marks 'plot', transect marks 'subplot', aggregation at site/plot/subplot/y/m/d")

spp.check<-CDR.e307.pc.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-CDR.e307.pc.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()


str(CDR.e307.pc.dat)
str(CDR.e307.pc.mapdat)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(CDR.e307.pc.pcdat, file="Percent.Cover.Species.GrassSav.CDRLTER.e307.csv", row.names = F)
############################################################
