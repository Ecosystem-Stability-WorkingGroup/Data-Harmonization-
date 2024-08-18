############################################################
#Load Packages
library(tidyverse)

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


#Abundance
predicts.trimmed.abun.dat<-predicts.trimmed.dat%>%
  filter(Diversity_metric == "abundance")

#Density
predicts.trimmed.dens.dat<-predicts.trimmed.dat%>%
  filter(Diversity_metric == "density")


############################################################
#Breaking Apart by Ecosystem

#Abundance - Forests
predicts.trimmed.abun.forest.dat<-predicts.trimmed.abun.dat%>%
  filter(!Ecoregion %in% c("Red Sea Nubo-Sindian Tropical Desert And Semi-Desert",
                           "Arabian Desert And East Sahero-Arabian Xeric Shrublands",
                           "Highveld Grasslands",
                           "Maputaland-Pondoland Bushland And Thickets",
                           "Ethiopian Montane Grasslands And Woodlands",
                           "Succulent Karoo",
                           "West Sudanian Savanna",
                           "Southern Acacia-Commiphora Bushlands And Thickets",
                           "Serengeti Volcanic Grasslands"))

#Density - Forests
predicts.trimmed.dens.forest.dat<-predicts.trimmed.dens.dat%>%
  filter(!Ecoregion %in% c("Red Sea Nubo-Sindian Tropical Desert And Semi-Desert",
                           "Arabian Desert And East Sahero-Arabian Xeric Shrublands",
                           "Highveld Grasslands",
                           "Maputaland-Pondoland Bushland And Thickets",
                           "Ethiopian Montane Grasslands And Woodlands",
                           "Succulent Karoo",
                           "West Sudanian Savanna",
                           "Southern Acacia-Commiphora Bushlands And Thickets",
                           "Serengeti Volcanic Grasslands"))

#Abundance - Grasslands
predicts.trimmed.abun.grassland.dat<-predicts.trimmed.abun.dat%>%
  filter(Ecoregion %in% c("Red Sea Nubo-Sindian Tropical Desert And Semi-Desert",
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


#PREDICTS Database - Abundance - Forests
# - predicts.trimmed.abun.forest.dat
predicts.trimmed.abun.forest.ADdat<-predicts.trimmed.abun.forest.dat%>%
  mutate(Date = as.Date(Sample_midpoint),
         site = Site, 
         Parameter = "Abundance",
         data = "PREDICTS Database - Abundance - Forests",
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
         abundance = Measurement,
         density = -9999,
         dens_units = "",
         latitude = Latitude,
         longitude =  Longitude)%>%
  mutate(spp_full_name = case_when(spp_full_name == "Euryops (Rooi)" ~ "Euryops sp.",
                                   spp_full_name == "Erica (Wol)" ~ "Erica sp.",
                                   spp_full_name == "Aster ???" ~ "Aster sp.",
                                   TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(spp_full_name = case_when(is.na(spp_full_name)~"998",
                                   TRUE ~ spp_full_name),
         genus = case_when(spp_full_name == "998" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "998" ~ "998",
                             TRUE ~ species))%>%
  mutate(species = case_when(species == "cf" |
                               is.na(species) |
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
                               species == "sp9" |
                               species == "sp10" |
                               species == "sp01" |
                               species == "sp02" |
                               species == "sp03" |
                               species == "sp04" |
                               species == "sp05" |
                               species == "sp06" |
                               species == "sp07" |
                               species == "sp08" |
                               species == "sp09" ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units, Sampling_effort, Sampling_effort_unit, Sampling_method)%>%
  mutate(density = case_when(Sampling_effort_unit == "square m" ~ abundance,
                             Sampling_effort_unit == "square km" ~ abundance/1000000,
                               Sampling_effort_unit== "hectare" ~ abundance/10000,
                               TRUE ~ density),
         dens_units = case_when(density != "-9999" ~ "# per m2",
                                TRUE ~ "no density measure"))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
  mutate(notes = "plot marks 'Block'; aggregation at site/plot/y/m/d")

str(predicts.trimmed.abun.forest.dat)
str(predicts.trimmed.abun.forest.ADdat)

#Species checking
spp.check<-predicts.trimmed.abun.forest.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

dens.check<-predicts.trimmed.abun.forest.ADdat%>%
  select(Sampling_effort, Sampling_effort_unit, Sampling_method)%>%
  distinct()

dens.check2<-predicts.trimmed.abun.forest.ADdat%>%
  select(density, dens_units)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(predicts.trimmed.abun.forest.ADdat, file="AbunDens.Species.Indv.Forest.PREDICTS.AD.Abun.csv", row.names = F)
############################################################


#PREDICTS Database - Density - Forests
# - predicts.trimmed.dens.forest.dat
predicts.trimmed.dens.forest.ADdat<-predicts.trimmed.dens.forest.dat%>%
  mutate(Date = as.Date(Sample_midpoint),
         site = Site, 
         Parameter = "Density",
         data = "PREDICTS Database - Density - Forests",
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
         abundance = Measurement,
         density = -9999,
         dens_units = "",
         latitude = Latitude,
         longitude =  Longitude)%>%
  mutate(spp_full_name = case_when(spp_full_name == "Euryops (Rooi)" ~ "Euryops sp.",
                                   spp_full_name == "Erica (Wol)" ~ "Erica sp.",
                                   spp_full_name == "Aster ???" ~ "Aster sp.",
                                   TRUE ~ spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(spp_full_name = case_when(is.na(spp_full_name) |
                                     spp_full_name == "" ~"998",
                                   TRUE ~ spp_full_name),
         genus = case_when(spp_full_name == "998" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "998" ~ "998",
                             TRUE ~ species))%>%
  mutate(species = case_when(species == "cf" |
                               is.na(species) |
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
                               species == "sp9" |
                               species == "sp10" |
                               species == "sp01" |
                               species == "sp02" |
                               species == "sp03" |
                               species == "sp04" |
                               species == "sp05" |
                               species == "sp06" |
                               species == "sp07" |
                               species == "sp08" |
                               species == "sp09" ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units, Sampling_effort, Sampling_effort_unit, Sampling_method)%>%
  mutate(density = case_when(Sampling_effort_unit == "square m" ~ abundance,
                             Sampling_effort_unit == "square km" ~ abundance/1000000,
                             Sampling_effort_unit== "hectare" ~ abundance/10000,
                             TRUE ~ density),
         dens_units = case_when(density != "-9999" ~ "# per m2",
                                TRUE ~ "no density measure"))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
  mutate(notes = "plot marks 'Block'; aggregation at site/plot/y/m/d")


str(predicts.trimmed.dens.forest.dat)
str(predicts.trimmed.dens.forest.ADdat)

#Species checking
spp.check<-predicts.trimmed.dens.forest.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

dens.check<-predicts.trimmed.dens.forest.ADdat%>%
  select(Sampling_effort, Sampling_effort_unit, Sampling_method)%>%
  distinct()

dens.check2<-predicts.trimmed.abun.forest.ADdat%>%
  select(density, dens_units)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(predicts.trimmed.dens.forest.ADdat, file="AbunDens.Species.Indv.Forest.PREDICTS.AD.Dens.csv", row.names = F)
############################################################


#PREDICTS Database - Abundance - Grassland
# - predicts.trimmed.abun.grassland.dat
predicts.trimmed.abun.grassland.ADdat<-predicts.trimmed.abun.grassland.dat%>%
  mutate(Date = as.Date(Sample_midpoint),
         site = Site, 
         Parameter = "Abundance",
         data = "PREDICTS Database - Abundance - Grasslands",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "PREDICTS.DB_", site),
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
         abundance = Measurement,
         density = -9999,
         dens_units = "",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(spp_full_name = case_when(is.na(spp_full_name) |
                                     spp_full_name == "" ~"998",
                                   TRUE ~ spp_full_name),
         genus = case_when(spp_full_name == "998" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "998" ~ "998",
                             TRUE ~ species))%>%
  mutate(species = case_when(species == "cf" |
                               is.na(species) |
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
                               species == "sp9" |
                               species == "sp10" |
                               species == "sp01" |
                               species == "sp02" |
                               species == "sp03" |
                               species == "sp04" |
                               species == "sp05" |
                               species == "sp06" |
                               species == "sp07" |
                               species == "sp08" |
                               species == "sp09" |
                               species == "spec"~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units, Sampling_effort, Sampling_effort_unit, Sampling_method)%>%
  mutate(density = case_when(Sampling_effort_unit == "square m" ~ abundance,
                             Sampling_effort_unit == "square km" ~ abundance/1000000,
                             Sampling_effort_unit== "hectare" ~ abundance/10000,
                             TRUE ~ density),
         dens_units = case_when(density != "-9999" ~ "# per m2",
                                TRUE ~ "no density measure"))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
  mutate(notes = "plot marks 'Block'; aggregation at site/plot/y/m/d")


str(predicts.trimmed.abun.grassland.dat)
str(predicts.trimmed.abun.grassland.ADdat)

#Species checking
spp.check<-predicts.trimmed.abun.grassland.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

dens.check<-predicts.trimmed.abun.grassland.ADdat%>%
  select(Sampling_effort, Sampling_effort_unit, Sampling_method)%>%
  distinct()

dens.check2<-predicts.trimmed.abun.grassland.ADdat%>%
  select(density, dens_units)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(predicts.trimmed.abun.grassland.ADdat, file="AbunDens.Species.Indv.GrassSav.PREDICTS.AD.Abun.csv", row.names = F)
############################################################
