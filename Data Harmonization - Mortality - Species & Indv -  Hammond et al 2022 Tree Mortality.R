############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#Hammond et al Tree Mortality 
# - GTM_full_database_download_20240626-221932.csv
GTM.full.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/Hammond et al 2022/GTM_full_database_download_20240626-221932.csv")
str(GTM.full.dat)

############################################################
#Formatting Data

#Hammond et al Tree Mortality 
# - GTM_full_database_download_20240626-221932.csv
GTM.full.mortdat<-GTM.full.dat%>%
  mutate(test = row_number(),
         Site = paste0(Ref_ID, "-", test))%>%
  mutate(year = event.start,
         site = Site, 
         Parameter = "Mortality",
         data = "Hammond et al 2022 - Tree Mortality",
         Taxa = "Trees",
         Specificity = "Species",
         ecosystem = "Forests",
         std_id = paste0("forest_", "Hammond22_", site),
         plot = 999,
         transect = 999,
         #date = lubridate::parse_date_time(XXXX, orders = c("mdy", "dmy", "ymd")),
         #year = year(date),
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         spp_full_name = species,
         stem_id = as.character("no_stem_id"),
         mort_year = year,
         latitude = lat,
         longitude =  long)%>%
  mutate(spp_full_name = strsplit(spp_full_name, ";\\s*")) %>%  #Splitting species
  unnest(spp_full_name)%>%
  mutate(spp_full_name = gsub("  ", " ", spp_full_name),
         spp_full_name = case_when(spp_full_name == "J. excelsa" ~ "Juniperus excelsa",
                                   spp_full_name == "Juniper foetidissima" ~ "Juniperus foetidissima",
                                   Ref_ID == "69" ~ "Euphorbia ingens",
                                   TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "many others" |
                           spp_full_name == "(many spp.)" |
                           spp_full_name == "Fig tree (ficus)" ~ "998",
                           genus == "Euphorbia ingens " ~ "Euphorbia",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "many others" |
                             spp_full_name == "(many spp.)" |
                             spp_full_name == "Fig tree (ficus)" |
                             species == "spp." ~ "998",
                             genus == "Euphorbia" ~ "ingens",
                             is.na(species) ~ "998",
                             species == "Ponderosa" ~ "ponderosa",
                           TRUE ~ species))%>%
  mutate(test2 = row_number(),
         tree_id = paste0(Ref_ID, "_", test, "_", test2))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, mort_year)%>%
  mutate(notes = "")

huh<-GTM.full.mortdat%>%
  filter(spp_full_name=="Euphorbia ingens")

str(GTM.full.dat)
str(GTM.full.mortdat)

#Species checking
spp.check<-GTM.full.mortdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

#Duplicate Checking
duplicates<- GTM.full.mortdat%>%
  mutate(duplicate = duplicated(paste(site, plot, transect, 
                                      year, month, day,
                                      spp_full_name, genus, species, tree_id)) | 
           duplicated(paste(site, plot, transect, 
                            year, month, day,
                            spp_full_name, genus, species, tree_id), fromLast = TRUE))%>%
  filter(duplicate == "TRUE")%>%
  arrange(desc(duplicate), site, plot, transect, year, month, day, spp_full_name, tree_id)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Mortality - Species & Individuals")
#Save Files
write.csv(GTM.full.mortdat, file="Mortality.Species.Indv.Forest.Hammond.Mort.csv", row.names = F)
############################################################

