############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#################
#ForestGeo - Tree - North America - SCBI
# - ForestGeo_Tree_SCBI_Census_1.csv
ForestGEO.SCBI.dat.cen1<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - SCBI/ForestGeo_Tree_SCBI_Census_1.csv")
# - ForestGeo_Tree_SCBI_Census_2.csv
ForestGEO.SCBI.dat.cen2<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - SCBI/ForestGeo_Tree_SCBI_Census_2.csv")
# - ForestGeo_Tree_SCBI_Census_3.csv
ForestGEO.SCBI.dat.cen3<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - SCBI/ForestGeo_Tree_SCBI_Census_3.csv")


#################
#ForestGeo - Tree - North America - UCSC
# - ForestGeo_Tree_UCSC_Census_1.csv
ForestGEO.UCSC.dat.cen1<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - UCSC/ForestGeo_Tree_UCSC_Census_1.csv")
# - ForestGeo_Tree_UCSC_Census_2.csv
ForestGEO.UCSC.dat.cen2<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - UCSC/ForestGeo_Tree_UCSC_Census_2.csv")
# - ForestGeo_Tree_UCSC_Census_3.csv
ForestGEO.UCSC.dat.cen3<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - UCSC/ForestGeo_Tree_UCSC_Census_3.csv")


#################
#ForestGeo - Tree - North America - SERC
# - ForestGeo_Tree_SERC_Census_1.csv
ForestGEO.SERC.dat.cen1<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - SERC/ForestGeo_Tree_SERC_Census_1.csv")
# - ForestGeo_Tree_SERC_Census_2.csv
ForestGEO.SERC.dat.cen2<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - SERC/ForestGeo_Tree_SERC_Census_2.csv")
# - ForestGeo_Tree_SERC_Census_3.csv
ForestGEO.SERC.dat.cen3<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - SERC/ForestGeo_Tree_SERC_Census_3.csv")

#################
#ForestGeo - Tree - North America - Luquillo
# - ForestGeo_Tree_Luquillo_Census_1.csv
ForestGEO.Luquillo.dat.cen1<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - Luquillo/ForestGeo_Tree_Luquillo_Census_1.csv")
# - ForestGeo_Tree_Luquillo_Census_2.csv
ForestGEO.Luquillo.dat.cen2<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - Luquillo/ForestGeo_Tree_Luquillo_Census_2.csv")
# - ForestGeo_Tree_Luquillo_Census_3.csv
ForestGEO.Luquillo.dat.cen3<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - Luquillo/ForestGeo_Tree_Luquillo_Census_3.csv")
# - ForestGeo_Tree_Luquillo_Census_4.csv
ForestGEO.Luquillo.dat.cen4<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - Luquillo/ForestGeo_Tree_Luquillo_Census_4.csv")
# - ForestGeo_Tree_Luquillo_Census_5.csv
ForestGEO.Luquillo.dat.cen5<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - Luquillo/ForestGeo_Tree_Luquillo_Census_5.csv")
# - ForestGeo_Tree_Luquillo_Census_6.csv
ForestGEO.Luquillo.dat.cen6<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - North America - Luquillo/ForestGeo_Tree_Luquillo_Census_6.csv")


############################################################
#Combining Censuses and Adding Metadata

#################
#ForestGeo - Tree - North America - SCBI
ForestGeo.SCBI.dat<-bind_rows(
  ForestGEO.SCBI.dat.cen1,
  ForestGEO.SCBI.dat.cen2,
  ForestGEO.SCBI.dat.cen3)%>%
  mutate(Site = "ForestGeo.SCBI",
         Latitude = 38.893500000000,
         Longitude = -78.145400000000)%>%
  filter(Date != "")  #Filtering out Blank Incorrectly Formatted Rows

#################
#ForestGeo - Tree - North America - UCSC
ForestGeo.UCSC.dat<-bind_rows(
  ForestGEO.UCSC.dat.cen1,
  ForestGEO.UCSC.dat.cen2,
  ForestGEO.UCSC.dat.cen3)%>%
  mutate(Site = "ForestGeo.UCSC",
         Latitude = 37.012400000000,
         Longitude = -122.075000000000)%>%
  filter(Date != "")  #Filtering out Blank Incorrectly Formatted Rows

#################
#ForestGeo - Tree - North America - SERC
ForestGeo.SERC.dat<-bind_rows(
  ForestGEO.SERC.dat.cen1,
  ForestGEO.SERC.dat.cen2,
  ForestGEO.SERC.dat.cen3)%>%
  mutate(Site = "ForestGeo.SERC",
         Latitude = 38.889100000000,
         Longitude = -76.559400000000)%>%
  filter(Date != "")  #Filtering out Blank Incorrectly Formatted Rows

#################
#ForestGeo - Tree - North America - Luquillo
ForestGeo.Luquillo.dat<-bind_rows(
  ForestGEO.Luquillo.dat.cen1,
  ForestGEO.Luquillo.dat.cen2,
  ForestGEO.Luquillo.dat.cen3,
  ForestGEO.Luquillo.dat.cen4,
  ForestGEO.Luquillo.dat.cen5,
  ForestGEO.Luquillo.dat.cen6)%>%
  mutate(Site = "ForestGeo.Luquillo",
         Latitude = 18.326200000000,
         Longitude = -65.816000000000)%>%
  filter(Date != "")  #Filtering out Blank Incorrectly Formatted Rows

############################################################
#Formatting Data

#################
#ForestGeo - Tree - North America - SCBI
ForestGeo.SCBI.DIAdat<-ForestGeo.SCBI.dat%>%
  filter(Status == "alive")%>%
  mutate(site = Site, 
         Parameter = "DBH, Mortality",
         data = "ForestGEO - Tree - SCBI",
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
         spp_full_name = Latin,
         tree_id = as.character(TreeID),
         stem_id = as.character(StemID),
         diameter = DBH,
         diam_units = "unclear",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Unidentified unk" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Unidentified unk" |
                               species == "sp" ~ "998",
                           TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter))%>%    
  mutate(notes = "plot marks 'Quadrat', live trees only; unclear if DBH is mm or cm")


str(ForestGeo.SCBI.dat)
str(ForestGeo.SCBI.DIAdat)

#Species checking
spp.check<-ForestGeo.SCBI.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#################
#ForestGeo - Tree - North America - UCSC
ForestGeo.UCSC.DIAdat<-ForestGeo.UCSC.dat%>%
  filter(Status == "alive")%>%
  filter(StemTag == "1")%>%
  mutate(site = Site, 
         Parameter = "DBH, Mortality",
         data = "ForestGEO - Tree - UCSC",
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
         spp_full_name = Latin,
         tree_id = as.character(TreeID),
         stem_id = as.character(StemID),
         diameter = DBH,
         diam_units = "unclear",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Unidentified unk" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Unidentified unk" |
                               species == "sp" ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter))%>%    
  mutate(notes = "plot marks 'Quadrat', live trees only; unclear if DBH is mm or cm")

str(ForestGeo.UCSC.dat)
str(ForestGeo.UCSC.DIAdat)

#Species checking
spp.check<-ForestGeo.UCSC.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

stem.check1<-ForestGeo.UCSC.dat%>%
  filter(StemTag == 1)
length(unique(stem.check1$TreeID))
length(unique(ForestGeo.UCSC.dat$TreeID))

stem.check2<-ForestGeo.UCSC.dat%>%
  select(TreeID, StemTag)%>%
  distinct()


#ForestGeo - Tree - North America - SERC
ForestGeo.SERC.DIAdat<-ForestGeo.SERC.dat%>%
  filter(Status == "alive")%>%
  mutate(site = Site, 
         Parameter = "DBH, Mortality",
         data = "ForestGEO - Tree - SERC",
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
         spp_full_name = Latin,
         tree_id = as.character(TreeID),
         stem_id = as.character(StemID),
         diameter = DBH,
         diam_units = "unclear",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Unidentified unk" |
                             spp_full_name == "Unidentified Unknown"|
                             spp_full_name == "Unidentified unknown 3" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Unidentified unk" |
                               spp_full_name == "Unidentified Unknown"|
                               spp_full_name == "Unidentified unknown 3" |
                               species == "sp" |
                               species == "sp." ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter))%>%    
  mutate(notes = "plot marks 'Quadrat', live trees only; unclear if DBH is mm or cm")

str(ForestGeo.SERC.dat)
str(ForestGeo.SERC.DIAdat)

#Species checking
spp.check<-ForestGeo.SERC.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()




# Function to fix invalid UTF-8 strings
fix_invalid_utf8 <- function(x) {
  # Replace invalid UTF-8 characters
  iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
}

#ForestGeo - Tree - North America - Luquillo
ForestGeo.Luquillo.DIAdat<-ForestGeo.Luquillo.dat%>%
  filter(Status == "alive")%>%
  mutate(site = Site, 
         Parameter = "DBH, Mortality",
         data = "ForestGEO - Tree - Luquillo",
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
         spp_full_name = Latin,
         tree_id = as.character(TreeID),
         stem_id = as.character(StemID),
         diameter = DBH,
         diam_units = "unclear",
         latitude = Latitude,
         longitude =  Longitude)%>%
  mutate(spp_full_name = fix_invalid_utf8(spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Unidentified unk" |
                             spp_full_name == "Unidentified Unknown" |
                             spp_full_name == "Unidentified SPECIES" |
                             spp_full_name == "Unidentified unknown 3" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Unidentified unk" |
                               spp_full_name == "Unidentified Unknown"|
                               spp_full_name == "Unidentified SPECIES" |
                               spp_full_name == "Unidentified unknown 3" |
                               species == "sp" |
                               species == "sp." |
                               species == "spp" ~ "998",
                               spp_full_name == "Citrus X aurantium" ~ "X aurantium",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter))%>%
  mutate(notes = "plot marks 'Quadrat', live trees only; unclear if DBH is mm or cm")

str(ForestGeo.Luquillo.dat)
str(ForestGeo.Luquillo.DIAdat)

#Species checking
spp.check<-ForestGeo.Luquillo.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()



############################################################
#Combine Files

ForestGeo.NorthAmerica.All.DIAdat<-bind_rows(
  ForestGeo.SCBI.DIAdat,
  ForestGeo.UCSC.DIAdat,
  ForestGeo.SERC.DIAdat,
  ForestGeo.Luquillo.DIAdat
)%>%
  mutate(day = as.numeric(day))%>%
  filter(diameter >0)

sapply(ForestGeo.NorthAmerica.All.DIAdat, function(x) sum(is.na(x)))

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(ForestGeo.NorthAmerica.All.DIAdat, file="Diameter.Species.Indv.Forest.ForestGEO.NorthAmerica.csv", row.names = F)
############################################################
