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
ForestGeo.SCBI.mortdat<-ForestGeo.SCBI.dat%>%
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
         mort_year = case_when(Status == "dead" ~ year,
                               TRUE ~ NA),
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
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, Status, mort_year)%>%
  mutate(notes = "plot marks 'Quadrat'")%>%
  arrange(tree_id, stem_id, year)

ForestGeo.SCBI.mortdat<-as.data.frame(ForestGeo.SCBI.mortdat)
str(ForestGeo.SCBI.dat)
str(ForestGeo.SCBI.mortdat)

#Checking Mortality
filtered_df <- ForestGeo.SCBI.mortdat %>%
  filter(!is.na(mort_year))%>%  
  select(tree_id, stem_id, mort_year)

result_df <- ForestGeo.SCBI.mortdat %>%
  semi_join(filtered_df, by =c("tree_id", "stem_id")) #Trees listed as dead become re-alive, can't use



#################
#ForestGeo - Tree - North America - UCSC   ##NO DEAD
ForestGeo.UCSC.mortdat<-ForestGeo.UCSC.dat%>%
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
         mort_year = case_when(Status == "dead" ~ year,
                               TRUE ~ NA),
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
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, Status, mort_year)%>%
  mutate(notes = "plot marks 'Quadrat'")%>%
  arrange(tree_id, stem_id, year)

#Checking Mortality
filtered_df <- ForestGeo.UCSC.mortdat %>%
  filter(!is.na(mort_year))%>%  
  select(tree_id, stem_id, mort_year)

result_df <- ForestGeo.UCSC.mortdat %>%
  semi_join(filtered_df, by =c("tree_id", "stem_id")) #No dead trees



#################
#ForestGeo - Tree - North America - SERC
ForestGeo.SERC.mortdat<-ForestGeo.SERC.dat%>%
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
         mort_year = case_when(Status == "dead" ~ year,
                               TRUE ~ NA),
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
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, Status, mort_year)%>%
  mutate(notes = "plot marks 'Quadrat'")%>%
  arrange(tree_id, stem_id, year)

#Checking Mortality
filtered_df <- ForestGeo.SERC.mortdat %>%
  filter(!is.na(mort_year))%>%  
  select(tree_id, stem_id, mort_year)

result_df <- ForestGeo.SERC.mortdat %>%
  semi_join(filtered_df, by =c("tree_id", "stem_id")) #Trees listed as dead become re-alive, can't use

#################
#ForestGeo - Tree - North America - Luquillo
ForestGeo.Luquillo.mortdat<-ForestGeo.Luquillo.dat%>%
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
         mort_year = case_when(Status == "dead" ~ year,
                               TRUE ~ NA),
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
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, Status, mort_year)%>%
  mutate(notes = "plot marks 'Quadrat'")%>%
  arrange(tree_id, stem_id, year)

#Checking Mortality
filtered_df <- ForestGeo.Luquillo.mortdat %>%
  filter(!is.na(mort_year))%>%  
  select(tree_id, stem_id, mort_year)

result_df <- ForestGeo.Luquillo.mortdat %>%
  semi_join(filtered_df, by =c("tree_id", "stem_id")) #USABLE!!

ForestGeo.Luquillo.mortdat.fin<-ForestGeo.Luquillo.mortdat%>%
  filter(Status == "dead")%>%
  select(-Status)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Mortality - Species & Individuals")
#Save Files
write.csv(ForestGeo.Luquillo.mortdat.fin, file="Mortality.Species.Indv.Forest.ForestGEO.NorthAmerica.csv", row.names = F)
############################################################
