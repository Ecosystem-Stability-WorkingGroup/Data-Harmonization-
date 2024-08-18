############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#################
#ForestGeo - Dendrometer - North America - SCBI
#ForestGeo_Dendrometer_SCBI_Census_1.csv
ForestGEO.SCBI.den.dat.cen1<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Dendrometer - North America - SCBI/ForestGeo_Dendrometer_SCBI_Census_1.csv")
#ForestGeo_Dendrometer_SCBI_Census_2.csv
ForestGEO.SCBI.den.dat.cen2<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Dendrometer - North America - SCBI/ForestGeo_Dendrometer_SCBI_Census_2.csv")
#ForestGeo_Dendrometer_SCBI_Census_3.csv
ForestGEO.SCBI.den.dat.cen3<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Dendrometer - North America - SCBI/ForestGeo_Dendrometer_SCBI_Census_3.csv")
#ForestGeo_Dendrometer_SCBI_Census_4.csv
ForestGEO.SCBI.den.dat.cen4<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Dendrometer - North America - SCBI/ForestGeo_Dendrometer_SCBI_Census_4.csv")
#ForestGeo_Dendrometer_SCBI_Census_5.csv
ForestGEO.SCBI.den.dat.cen5<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Dendrometer - North America - SCBI/ForestGeo_Dendrometer_SCBI_Census_5.csv")
#ForestGeo_Dendrometer_SCBI_Census_6.csv
ForestGEO.SCBI.den.dat.cen6<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Dendrometer - North America - SCBI/ForestGeo_Dendrometer_SCBI_Census_6.csv")

############################################################
#Combining Censuses and Adding Metadata

#################
#ForestGeo - Tree - North America - SCBI
ForestGeo.SCBI.den.dat<-bind_rows(
  ForestGEO.SCBI.den.dat.cen1,
  ForestGEO.SCBI.den.dat.cen2,
  ForestGEO.SCBI.den.dat.cen3,
  ForestGEO.SCBI.den.dat.cen4,
  ForestGEO.SCBI.den.dat.cen5,
  ForestGEO.SCBI.den.dat.cen6)%>%
  mutate(Site = "ForestGeo.SCBI.Dend",
         Latitude = 38.893500000000,
         Longitude = -78.145400000000)%>%
  filter(Date != "")  #Filtering out Blank Incorrectly Formatted Rows

############################################################
#Formatting Data

#ForestGeo - Dendrometer - North America - SCBI
# - CSV Name
ForestGeo.SCBI.den.DIAdat<-ForestGeo.SCBI.den.dat%>%
  filter(Status == "alive")%>%
  filter(Genus != "")%>%
  mutate(site = Site, 
         Parameter = "DBH, Mortality",
         data = "ForestGEO - Dendrometer - SCBI",
         Taxa = "Trees",
         Specificity = "Individuals",
         ecosystem = "Forests",
         std_id = paste0("forest_", "Forest.GEO_", site),
         plot = as.character(Quadrat),
         transect = 999,
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         genus = Genus,
         species = Species,
         spp_full_name = paste0(genus, " ", species),
         tree_id = as.character(TreeTag),
         stem_id = as.character(StemTag),
         diameter = DendroMeasure,
         diam_units = "unclear",
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%    
  mutate(notes = "plot marks 'Quadrat', diameter from 'DendroMeasure'; unclear if mm or cm")

str(ForestGeo.SCBI.den.dat)
str(ForestGeo.SCBI.den.DIAdat)

#Species checking
spp.check<-ForestGeo.SCBI.den.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(ForestGeo.SCBI.den.DIAdat, file="Diameter.Species.Indv.ForestGEO.SCBI.Dendro.csv", row.names = F)
############################################################
