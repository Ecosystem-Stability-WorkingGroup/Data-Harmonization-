############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing
fce.mang<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Mangroves/FCE LTER/LT_PP_Rivera_002.csv")
str(fce.mang)
############################################################
#Creating GPS Coordinate Dataframe

fce.gps<- data.frame(
  Site = c("SRS4", "SRS5", "SRS6", "TS/Ph8"),
  Latitude = c(25.410, 25.377, 25.365, 25.233),
  Longitude = c(-80.964, -81.032, -81.078, -80.525)
)

############################################################
#Formatting Data

fce.mang.DIAdat<-fce.mang%>%
  left_join(fce.gps, by=c("SITENAME" = "Site"))%>%
  mutate(site = SITENAME,
         Parameter = "DBH",
         data = "FCE LTER Mangrove DBH",
         Taxa = "Mangroves",
         Specificity = "Individuals",
         ecosystem = "Mangrove Forest",
         std_id = paste0("mang_", "FCE.LTER_", site),
         plot = Plot_ID,
         transect = 999,
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         tree_id = as.character(Tree_TagNumber),
         stem_id = as.character("no_stem_id"),
         diameter = Tree_DBH,
         diam_units = "cm",
         latitude = Latitude,
         longitude = Longitude)%>%
  mutate(spp_full_name = case_when(Species_Tree == "A" ~ "Avicennia germinans",
                                    Species_Tree == "C" ~ "Conacarpus erectus",
                                    Species_Tree == "L" ~ "Laguncularia racemosa",
                                    Species_Tree == "R" ~ "Rhizophora mangle",
                                    TRUE ~ NA))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%
  filter(!is.na(spp_full_name))%>%
  mutate(notes = "plot marks 'PlotID'")



str(fce.mang)
str(fce.mang.DIAdat)

#Species checking
spp.check<-fce.mang.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(fce.mang.DIAdat, file="Diameter.Species.Indv.Mangrove.FCE.LTER.csv", row.names = F)
############################################################




