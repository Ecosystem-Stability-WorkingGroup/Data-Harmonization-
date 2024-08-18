############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#Andrews Experimental Forest (AEF) LTER - Site Locations
AEF.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/AEF LTER/locTV010.csv")
str(AEF.coords)
AEF.coords<-AEF.coords%>%
  mutate(LOCATION_CODE = trimws(LOCATION_CODE))%>% 
  mutate(Latitude = (NORTH_BOUND_COORD_decdeg + SOUTH_BOUND_COORD_decdeg)/2,
         Longitude = (WEST_BOUND_COORD_decdeg + EAST_BOUND_COORD_decdeg)/2)%>%
  select(LOCATION_CODE, Latitude, Longitude)

#Species Codes
AEF.sppcodes<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/AEF LTER/aef_spp_codes.csv")
str(AEF.sppcodes)
AEF.sppcodes<-AEF.sppcodes%>%
  select(code, species)

PLANTS.spplist<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/USDA_PLANTS_spp_list_complete.txt")%>%
  select(Symbol, Synonym.Symbol, Scientific.Name.with.Author)%>%
  mutate(code = case_when(Synonym.Symbol == "" ~ Symbol,
                          TRUE ~ Synonym.Symbol))%>%
  separate(
    col = Scientific.Name.with.Author,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(code, genus, species)%>%
  distinct()

  
  str(PLANTS.spplist)

#Andrews Experimental Forest (AEF) LTER - Long-Term Growth, Mortality, and Regeneration of Trees in Permanent Vegetation Plots, 1910 to Present
# - Initial tree conditions with spatial coordinates
# - TV01001_v2.csv
AEF.init.tree.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/AEF LTER/TV01001_v2.csv")
str(AEF.init.tree.dat)

#Andrews Experimental Forest (AEF) LTER - Long-Term Growth, Mortality, and Regeneration of Trees in Permanent Vegetation Plots, 1910 to Present
# - Individual tree remeasurement
# - TV01002_v19.csv
AEF.rem.tree.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/AEF LTER/TV01002_v19.csv")
str(AEF.rem.tree.dat)

############################################################
#Formatting Data

AEF.tree.DIAdat<-AEF.rem.tree.dat%>%
  left_join(AEF.coords, by=c("STANDID" = "LOCATION_CODE"))%>%
  filter(!TREE_STATUS %in% c("6", "8", "9"))%>% #remove dead/missing trees
  mutate(site = STANDID, 
         Parameter = "DBH, Mortality",
         data = "AEF LTER - Tree Growth & Mortality",
         Taxa = "Trees",
         Specificity = "Individuals",
         ecosystem = "Forests",
         std_id = paste0("forest_", "AEF.LTER_", site),
         plot = PLOTNUMBER,
         transect = 999,
         date = lubridate::parse_date_time(SAMPLEDATE, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         tree_id = as.character(TREEID),
         stem_id = as.character(TAG),
         diameter = DBH,
         diam_units = "cm",
         latitude = Latitude,
         longitude =  Longitude)%>%
  left_join(PLANTS.spplist, by = c("SPECIES" = "code"))%>%
  mutate(genus = case_when(SPECIES == "ILAQ" ~ "Ilex",
                           SPECIES == "SEGI" ~ "Sequoiadendron",
                           TRUE ~ genus),
         species = case_when(SPECIES == "ILAQ" ~ "aquifolium",
                           SPECIES == "SEGI" ~ "giganteum",
                           TRUE ~ species))%>%
  mutate(spp_full_name = paste0(genus, " ", species))%>%
  mutate(species = case_when(species == "L." ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%    
  mutate(notes = "plot marks 'PLOTNUMBER', stem_id marks 'TAG'; live trees only")


str(AEF.rem.tree.dat)
str(AEF.tree.DIAdat)

#Species checking
spp.check<-AEF.tree.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(AEF.tree.DIAdat, file="Diameter.Species.Indv.Forest.AEF.LTER.csv", row.names = F)
############################################################

