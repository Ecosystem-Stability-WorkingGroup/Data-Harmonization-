############################################################
#Load Packages
library(tidyverse)


############################################################
#Automatic CSV Importing

#Georgia Coastal Ecosystems (GCE) LTER - Altamaha River Tree Dendrometer Band and DBH - PLT-GCED-1711_3_0.CSV
#Multi-step to remove blank and other superfluous rows
lines_to_skip <- c(1, 2, 4, 5)
data_lines <- readLines("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/GCE LTER/PLT-GCED-1711_3_0.CSV")
data_lines <- data_lines[-lines_to_skip]
GCE.ar.dbh.dat <- read.csv(text = data_lines, header = TRUE)

str(GCE.ar.dbh.dat)

############################################################
#Formatting Data

#Georgia Coastal Ecosystems (GCE) LTER - Altamaha River Tree Dendrometer Band and DBH - PLT-GCED-1711_3_0.CSV
GCE.ar.dbh.DIAdat<-GCE.ar.dbh.dat%>%
  mutate(site = Site_Code, 
         Parameter = "DBH",
         data = "GCE LTER - Dendrometer & DBH",
         Taxa = "Trees (~6 spp)",
         Specificity = "Species",
         ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "GCE.LTER_", site),
         plot = 999,
         transect = 999,
         #date = lubridate::parse_date_time(XXXX, orders = c("mdy", "dmy", "ymd")),
         year = Year,
         month = Month,
         month = ifelse(is.na(month), 999, month),
         day = 999,
         day = ifelse(is.na(day), 999, day),
         tree_id = as.character(Tag),
         stem_id = as.character("no_stem_id"),
         diameter = Diameter_Breast_Height + BandMeasure_Increase,
         diam_units = "cm",
         latitude = 31.378508,                   
         longitude = -81.496112)%>%
  mutate(spp_full_name = case_when(Tree_Species == "AcRu" ~ "Acer rubrum", 
                                    Tree_Species == "FrPe" ~ "Fraxinus pennsylanica", 
                                    Tree_Species == "LiSt" ~ "Liquidambar styraciflua", 
                                    Tree_Species == "NyAq" ~ "Nyssa aquatica", 
                                    Tree_Species == "NyBi" ~ "Nyssa sylvatica var. biflora", 
                                    Tree_Species == "TaDi" ~ "Taxodium distichum",
                                    TRUE ~ NA))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%    
  mutate(notes = "")


str(GCE.ar.dbh.dat)
str(GCE.ar.dbh.DIAdat)

#Species checking
spp.check<-GCE.ar.dbh.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(GCE.ar.dbh.DIAdat, file="Diameter.Species.Indv.SMEW.GCE.LTER.csv", row.names = F)
############################################################

