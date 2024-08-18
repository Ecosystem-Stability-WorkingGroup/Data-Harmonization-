############################################################
#Load Packages
library(tidyverse)
library(lubridate)


############################################################
#Automatic CSV Importing

#Sevilleta (SEV) LTER - Site Coordinates
SEV.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/SEV LTER/SEV.LTER.Coordinates.csv")
str(SEV.coords)

#Sevilleta (SEV) LTER - Core Research Site Web Quadrat Data for the Net Primary Production Study 
# - sev129_NPP_quad_core_cover_height.csv
SEV.quad.cover.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/SEV LTER/sev129_NPP_quad_core_cover_height.csv")
str(SEV.quad.cover.dat)

#Sevilleta (SEV) LTER - Quadrat Plant Species All Sites and Experiments
# - sev331_quadrat_plant_species_biomass.csv
SEV.quad.cover.ALL.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/SEV LTER/sev331_quadrat_plant_species_biomass.csv")
str(SEV.quad.cover.ALL.dat)

#Sevilleta (SEV) LTER - Core Research Site Web Seasonal Biomass and Seasonal and Annual NPP Data for the Net Primary Production Study
# - sev182_NPP_core_biomass.csv
SEV.bio.npp.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/SEV LTER/sev182_NPP_core_biomass.csv")
str(SEV.bio.npp.dat)

#Sevilleta (SEV) LTER - Fall Season Aboveground NPP in the Monsoon Rainfall Manipulation Experiment (MRME)
# - rfbrown-mrme-anpp-20231011.csv
SEV.monsoon.npp.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/SEV LTER/rfbrown-mrme-anpp-20231011.csv")
str(SEV.monsoon.npp.dat)

#Sevilleta (SEV) LTER - Biome Transition Along Elevational Gradients in New Mexico (SEON) Study
# - sev292_flux_biomass.csv
SEV.flux.seon.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/SEV LTER/sev292_flux_biomass.csv")
str(SEV.flux.seon.dat)

############################################################
#Trimming to Control Sites  where Necessary

#Sevilleta (SEV) LTER - Quadrat Plant Species All Sites and Experiments
# - sev331_quadrat_plant_species_biomass.csv
SEV.quad.cover.ALL.dat<-SEV.quad.cover.ALL.dat%>%
  filter(treatment %in% c("AA", "C"))

#Sevilleta (SEV) LTER - Fall Season Aboveground NPP in the Monsoon Rainfall Manipulation Experiment (MRME)
# - rfbrown-mrme-anpp-20231011.csv
SEV.monsoon.npp.dat<-SEV.monsoon.npp.dat%>%
  filter(h2o == "A")

#Sevilleta (SEV) LTER - Biome Transition Along Elevational Gradients in New Mexico (SEON) Study
# - sev292_flux_biomass.csv
SEV.flux.seon.dat<-SEV.flux.seon.dat%>%
  filter(treatment=="C")


############################################################
#Formatting Data

#Sevilleta (SEV) LTER - Quadrat Plant Species All Sites and Experiments
# - sev331_quadrat_plant_species_biomass.csv
#NOTE - MISSING TWO SITES GPS
SEV.quad.cover.ALL.biodat<-SEV.quad.cover.ALL.dat%>%
  left_join(SEV.coords, by = c("site" = "Site"))%>%
  mutate(site = site, 
         Parameter = "Percent Cover, Biomass",
         data = "SEV LTER - All Quadrat Data",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "SEV.LTER_", site),
         plot = paste0(web, "_", plot),
         transect = quad,
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = paste0(genus, " ", sp.epithet),
         species = sp.epithet,
         biomass = biomass.BM,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  mutate(spp_full_name = case_when(
    kartez == "UNKNOWN" ~ "998",
    kartez == "EMPTY" ~ "998",
    kartez == "NYCT" ~ "Nyctaginia sp.",
    kartez == "ASTER" ~ "Aster sp.",
    kartez == "CACTA" ~ "998",
    kartez == "DELPH" ~ "Delphinium sp.",
    TRUE ~ spp_full_name),
    genus = case_when(
      kartez == "UNKNOWN" ~ "998",
      kartez == "EMPTY" ~ "998",
      kartez == "NYCT" ~ "Nyctaginia",
      kartez == "ASTER" ~ "Aster",
      kartez == "CACTA" ~ "998",
      kartez == "DELPH" ~ "Delphinium",
      TRUE ~ genus),
    species = case_when(
      kartez == "UNKNOWN" ~ "998",
      kartez == "EMPTY" ~ "998",
      kartez == "NYCT" ~ "sp.",
      kartez == "ASTER" ~ "sp.",
      kartez == "CACTA" ~"998",
      kartez == "DELPH" ~ "sp.",
      TRUE ~ species),
    species = ifelse(is.na(species), "sp.", species))%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  filter(!is.na(latitude))%>%
  mutate(notes = "plot marks 'web/plot', transect marks 'quadrat', aggregation likely site/plot/transect/y/m/d")

str(SEV.quad.cover.ALL.dat)
str(SEV.quad.cover.ALL.biodat)

#Species checking
spp.check<-SEV.quad.cover.ALL.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-SEV.quad.cover.ALL.biodat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(biomass))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()


#Sevilleta (SEV) LTER - Biome Transition Along Elevational Gradients in New Mexico (SEON) Study
# - sev292_flux_biomass.csv
SEV.flux.seon.biodat<-SEV.flux.seon.dat%>%
  left_join(SEV.coords, by = c("site" = "Site"))%>%
  mutate(site = site, 
         Parameter = "Percent Cover, Biomass",
         data = "SEV LTER - SEON Study",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "SEV.LTER_", site),
         plot = "999",
         transect = as.character(quad),
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = paste0(genus, " ", sp.epithet),
         species = sp.epithet,
         biomass = biomass.BM,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  mutate(spp_full_name = case_when(
    kartez == "DELPH" ~ "Delphinium sp.",
    TRUE ~ spp_full_name),
    genus = case_when(
      kartez == "DELPH" ~ "Delphinium",
      TRUE ~ genus),
    species = case_when(
      kartez == "DELPH" ~ "sp.",
      TRUE ~ species),
    species = ifelse(is.na(species), "sp.", species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "transect marks 'quadrat', aggregation likely site/plot/transect/y/m/d")


str(SEV.flux.seon.dat)
str(SEV.flux.seon.biodat)

#Species checking
spp.check<-SEV.flux.seon.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

#Backcheck for percent cover aggregation levels
check1<-SEV.flux.seon.biodat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(biomass))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()


#Sevilleta (SEV) LTER - Core Research Site Web Seasonal Biomass and Seasonal and Annual NPP Data for the Net Primary Production Study
# - sev182_NPP_core_biomass.csv
SEV.bio.npp.biodat<-SEV.bio.npp.dat%>%
  filter(cover > 0)%>%
  left_join(SEV.coords, by = c("site" = "Site"))%>%
  mutate(site = site, 
         Parameter = "Percent Cover, Biomass",
         data = "SEV LTER - Seasonal Biomass & NPP",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "SEV.LTER_", site),
         plot = paste0(web, "_", plot),
         transect = as.character(quad),
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = paste0(genus, " ", sp.epithet),
         species = sp.epithet,
         biomass = biomass.BM,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             is.na(species) ~ "998",
                             TRUE ~ species))%>%
  mutate(spp_full_name = case_when(spp_full_name == "NA NA" ~ "998",
                                    TRUE ~ spp_full_name),
         genus = case_when(spp_full_name == "998" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "998" ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'web/plot', transect marks 'quadrat', aggregation likely site/plot/transect/y/m/d")


str(SEV.bio.npp.dat)
str(SEV.bio.npp.biodat)

spp.check<-SEV.bio.npp.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-SEV.bio.npp.biodat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(biomass))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()


############################################################
#Crosschecking Sites

all.quads<-SEV.quad.cover.ALL.biodat[,c("site", "plot", "transect", "year" , "month", "day")]%>%
  mutate(all = "X")%>%
  distinct()

bio.npp.quads<-SEV.bio.npp.biodat[,c("site", "plot", "transect", "year" , "month", "day")]%>%
  mutate(bio.npp = "X")%>%
  distinct()

bio.seon<-SEV.flux.seon.biodat[,c("site", "plot", "transect", "year" , "month", "day")]%>%
  mutate(seon = "X")%>%
  distinct()

sev.cross<-all.quads%>%
  full_join(bio.npp.quads, by = c("site", "plot", "transect", "year" , "month", "day"))%>%
  full_join(bio.seon, by = c("site", "plot", "transect", "year" , "month", "day"))%>%
  mutate(all.only = case_when(all=="X" & is.na(bio.npp) & is.na(seon)   ~ "X" , 
                              TRUE ~ NA),
         bio.npp.only = case_when(bio.npp=="X" & is.na(all) & is.na(seon)  ~ "X" ,  #All Bio.NPP sites are in other data
                                  TRUE ~ NA),
         bio.seon.only = case_when(seon=="X" & is.na(all) & is.na(bio.npp)  ~ "X" , #All SEON sites are unique - exist only in SEON
                                   TRUE ~ NA))
############################################################
#Reformatting ALL Quadrat Data to Match Percent Cover - Adding Transect

#Sevilleta (SEV) LTER - Quadrat Plant Species All Sites and Experiments
# - sev331_quadrat_plant_species_biomass.csv
#NOTE - MISSING TWO SITES GPS
SEV.quad.cover.ALL.biodat<-SEV.quad.cover.ALL.dat%>%
  left_join(SEV.coords, by = c("site" = "Site"))%>%
  mutate(site = site, 
         Parameter = "Percent Cover, Biomass",
         data = "SEV LTER - All Quadrat Data",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "SEV.LTER_", site),
         plot = paste0(web, "_", plot),
         transect = paste0(quad, "_", transect),
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = paste0(genus, " ", sp.epithet),
         species = sp.epithet,
         biomass = biomass.BM,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  mutate(spp_full_name = case_when(
    kartez == "UNKNOWN" ~ "998",
    kartez == "EMPTY" ~ "998",
    kartez == "NYCT" ~ "Nyctaginia sp.",
    kartez == "ASTER" ~ "Aster sp.",
    kartez == "CACTA" ~ "998",
    kartez == "DELPH" ~ "Delphinium sp.",
    TRUE ~ spp_full_name),
    genus = case_when(
      kartez == "UNKNOWN" ~ "998",
      kartez == "EMPTY" ~ "998",
      kartez == "NYCT" ~ "Nyctaginia",
      kartez == "ASTER" ~ "Aster",
      kartez == "CACTA" ~ "998",
      kartez == "DELPH" ~ "Delphinium",
      TRUE ~ genus),
    species = case_when(
      kartez == "UNKNOWN" ~ "998",
      kartez == "EMPTY" ~ "998",
      kartez == "NYCT" ~ "sp.",
      kartez == "ASTER" ~ "sp.",
      kartez == "CACTA" ~"998",
      kartez == "DELPH" ~ "sp.",
      TRUE ~ species),
    species = ifelse(is.na(species), "sp.", species))%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  filter(!is.na(latitude))%>%
  mutate(notes = "plot marks 'web/plot', transect marks 'quadrat/transect', aggregation likely site/plot/transect/y/m/d")

str(SEV.quad.cover.ALL.dat)
str(SEV.quad.cover.ALL.biodat)
############################################################
#Combining Final Dataframe

sev.bio.all.dat<-bind_rows(
  SEV.quad.cover.ALL.biodat,
  SEV.flux.seon.biodat
)


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Biomass - Species & Individuals")
#Save Files
write.csv(sev.bio.all.dat, file="Biomass.Species.Indv.GrassSav.SEV.LTER.AllBMass.csv", row.names = F)
############################################################

