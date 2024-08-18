############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#Dataset Name 
# - CSV Name
predicts.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Multiple/PREDICTS Database/resource.csv")
str(predicts.dat)

############################################################
#Trimming Classes

predicts.trimmed.dat<-predicts.dat%>%
  filter(Class %in% c("Magnoliopsida",
                      "Liliopsida",
                      "Polypodiopsida",
                      "Lycopodiopsida",
                      "Gnetopsida",
                      "Marattiopsida",
                      "Pinopsida",
                      "Bryopsida",
                      "Jungermanniopsida",
                      "Equisetopsida",
                      "Psilotopsida",
                      "Anthocerotopsida",
                      "Marchantiopsida",
                      "Sphagnopsida",
                      "Cycadopsida",
                      "Andreaeopsida",
                      "Haplomitriopsida"))

############################################################
#Rolling Up Plot-Level Obs into Sites and Assigning Unique Site Names

predicts.trimmed.dat<-predicts.trimmed.dat%>%
  group_by(Latitude, Longitude) %>%
  mutate(Site = cur_group_id()) %>%
  ungroup()


sitecheck<-predicts.trimmed.dat%>%
  select(c(Site, Latitude, Longitude))%>%
  distinct()

############################################################
#Breaking Apart by Metric

#Percent Cover
predicts.trimmed.pcov.dat<-predicts.trimmed.dat%>%
  filter(Diversity_metric == "percent cover")

############################################################
#Breaking Apart by Ecosystem

#Percent Cover - Forests
predicts.trimmed.pcov.forest.dat<-predicts.trimmed.pcov.dat%>%
  filter(!Ecoregion %in% c("Red Sea Nubo-Sindian Tropical Desert And Semi-Desert",
                           "Arabian Desert And East Sahero-Arabian Xeric Shrublands",
                           "Highveld Grasslands",
                           "Maputaland-Pondoland Bushland And Thickets",
                           "Ethiopian Montane Grasslands And Woodlands",
                           "Succulent Karoo",
                           "West Sudanian Savanna",
                           "Southern Acacia-Commiphora Bushlands And Thickets",
                           "Serengeti Volcanic Grasslands"))

############################################################
#Formatting Data

#PREDICTS Database - Percent Cover - Forests
# - predicts.trimmed.pcov.forest.dat
predicts.trimmed.pcov.forest.pcdat<-predicts.trimmed.pcov.forest.dat%>%
  filter(Rank == "Species" | Rank == "Genus")%>% #trimming to genus and lower specificity
  filter(Measurement > 0)%>%                     #pulling zero data out
  mutate(Date = as.Date(Sample_midpoint),
         site = Site, 
         Parameter = "Percent Cover",
         data = "PREDICTS Database - % Cover - Forests",
         Taxa = "Trees",
         Specificity = "Species",
         ecosystem = "Forests",
         std_id = paste0("forest_", "PREDICTS.DB_", site),
         plot = Block,
         plot = ifelse(plot =="", 999, plot),
         transect = 999,
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = case_when (Sample_date_resolution == "year" ~ 999,
                            TRUE ~ month),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = case_when (Sample_date_resolution == "year" |
                            Sample_date_resolution =="month" ~ 999,
                            TRUE ~ day),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = Best_guess_binomial,
         spp_full_name = ifelse(spp_full_name == "", Taxon_name_entered, spp_full_name),
         spp_full_name = gsub(" species", " sp.", spp_full_name),
         percent_cover = Measurement,
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  mutate(spp_full_name = case_when(spp_full_name == "Euryops (Rooi)" ~ "Euryops sp.",
                                   spp_full_name == "Erica (Wol)" ~ "Erica sp.",
                                   spp_full_name == "Aster ???" ~ "Aster sp.",
                                   TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(species = case_when(is.na(species) |
                               species == "sp." |
                               species == "spp" |
                               species == "sp.1" |
                               species == "sp.2" |
                               species == "sp1" |
                               species == "sp2" |
                               species == "sp3" |
                               species == "sp4" |
                               species == "sp5" |
                               species == "sp6" |
                               species == "sp7" |
                               species == "sp8" |
                               species == "spp." ~ "998",
                               TRUE ~ species))%>%
  mutate(notes = "plot marks 'Block'; aggregation at site/plot/y/m/d")


str(predicts.trimmed.pcov.forest.dat)
str(predicts.trimmed.pcov.forest.pcdat)

predicts.spp.check<-predicts.trimmed.pcov.forest.dat%>%
  select(Best_guess_binomial,  Parsed_name, Taxon, Taxon_name_entered, Genus, Species)%>%
  distinct()

predicts.cleaned.spp.check<-predicts.trimmed.pcov.forest.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


check1<-predicts.trimmed.pcov.forest.pcdat%>%
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
write.csv(predicts.trimmed.pcov.forest.pcdat, file="Percent.Cover.Species.Forest.PREDICTS.PCov.csv", row.names = F)
############################################################