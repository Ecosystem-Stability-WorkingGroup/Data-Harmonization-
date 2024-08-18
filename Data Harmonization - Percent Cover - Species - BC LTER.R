############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#Site Coordinates
bc.lter.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/BC LTER/BC.LTER.Site.Coords.csv")

#BC LTER Vegetation Percent Cover: Control Plots - 174.VegplotPercentCover_2009.csv
bc.lter.veg.control<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/BC LTER/174_VegplotPercentCover_2009.csv")
str(bc.lter.veg.control)

#BC LTER Shurb, Seedling, Sapling Density - 530_ShrubSeedlingSaplingDensity_1975-2021.csv
bc.lter.sss.den<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/BC LTER/530_ShrubSeedlingSaplingDensity_1975-2021.csv")
str(bc.lter.sss.den)

#BC LTER Vegetation Cover: Wichersham Fire Sites - 219_WickVegCoverLAVplots.csv
bc.lter.veg.fire<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/BC LTER/219_WickVegCoverLAVplots.csv")
str(bc.lter.veg.fire)

#Species Code List - SSS
bc.lter.spp.codes<-read.table("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/BC LTER/PlantSpeciesList_Export.txt", 
                              sep = ",", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)
str(bc.lter.spp.codes)

#Species Code List - Wickersham
bc.lter.wick.spp.codes<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/BC LTER/WickTaxaListLAVplots.csv")
str(bc.lter.wick.spp.codes)

############################################################
#Formatting Data

#BC LTER Vegetation Percent Cover: Control Plots - 174.VegplotPercentCover_2009.csv
bc.lter.veg.control.pcdat<-bc.lter.veg.control%>%
  left_join(bc.lter.coords, by=c("site" = "Site"))%>%
  mutate(Date = as.Date(date),
         Year = as.numeric(format(Date, "%Y")),
         Site = site,  
         Parameter = "Percent Cover",
         Data = "BC LTER - Control Sites",
         Taxa = "Mosses, Lichens, Forbs, Low Shrubs",
         Specificity = "Species",
         Ecosystem = "Forests",
         STD_ID = paste0("forest_", "BC.LTER_", Site))%>%
  select(STD_ID, Data, Ecosystem, Site, Latitude, Longitude, 
         Year, c(3:25))%>%
  pivot_longer(cols = 11:ncol(.), 
               names_to = "Plot", 
               values_to = "percent_cover")%>%
  left_join(bc.lter.spp.codes[,c("LTER_code", "genus", "species")], by=c("SpecCode" = "LTER_code"))

spp.check<-bc.lter.veg.control.pcdat%>%
  select(SpecCode, genus, species)%>%
  distinct()
  

  
  #BC LTER Vegetation Cover: Wichersham Fire Sites - 219_WickVegCoverLAVplots.csv
  bc.lter.veg.fire.pcdat<-bc.lter.veg.fire%>%
    mutate(Site = as.character(SITE))%>%                 #Sites are Numbers, Changing Variable Type to chr
    left_join(bc.lter.coords, by=c("Site"))%>%
    mutate(Date = as.Date(strptime(DATE, format = "%m/%d/%Y")),
           Year = as.numeric(format(Date, "%Y")),
           Parameter = "Percent Cover",
           Data = "BC LTER - Wichersham Fire Sites",
           Taxa = "Fungi, Lichens, Moss, Low SHrub, Herbaceous, Tall Shrub, Seedling, Trees",
           Specificity = "Species",
           Ecosystem = "Forests",
           STD_ID = paste0("forest_", "BC.LTER_", Site))%>%
    select(STD_ID, Data, Ecosystem, Site, Latitude, Longitude, 
           Year, c(6:16))%>%
    pivot_longer(cols = 9:ncol(.), 
                 names_to = "Plot", 
                 values_to = "percent_cover")%>%
    left_join(bc.lter.wick.spp.codes[,c("SPCODE", "Genus", "Species")], by=c("SPCODE"))
  
  
  str(bc.lter.veg.fire.pcdat)
  