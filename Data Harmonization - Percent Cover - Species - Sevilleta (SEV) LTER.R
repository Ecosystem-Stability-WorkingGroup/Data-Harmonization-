############################################################
#Load Packages
library(tidyverse)


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
#Pulling Species Code Lists
SEV.spp.list<-SEV.quad.cover.ALL.dat%>%
  select(kartez, genus, sp.epithet)%>%
  bind_rows(SEV.bio.npp.dat[,c("kartez", "genus", "sp.epithet")],
            SEV.monsoon.npp.dat[,c("kartez", "genus", "species")],
            SEV.flux.seon.dat[,c("kartez", "genus", "sp.epithet")])%>%
  mutate(species = ifelse(is.na(species), sp.epithet, species))%>%
  rename("code" = "kartez")%>%
  select(code, genus, species)%>%
  filter(code != "")%>%
  mutate(species = case_when(!is.na(genus) & is.na(species) ~ "sp.",
                             TRUE ~ species),
         genus = case_when(is.na(genus) ~ "Unknown", 
                           TRUE ~ genus),
         species = case_when(is.na(species) ~ "unknown",
                             TRUE ~ species))%>%
  distinct()

duplicated(SEV.spp.list$code)


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

#Sevilleta (SEV) LTER - Core Research Site Web Quadrat Data for the Net Primary Production Study 
# - sev129_NPP_quad_core_cover_height.csv
SEV.quad.cover.pcdat<-SEV.quad.cover.dat%>%
  filter(cover > 0)%>%
  left_join(SEV.coords, by = c("site" = "Site"))%>%
  mutate(site = site, 
         Parameter = "Percent Cover",
         data = "SEV LTER - Core Quadrat Data",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "SEV.LTER_", site),
         plot = paste0(web, "_", plot),
         transect = as.character(quad),
         date = lubridate::parse_date_time(collection_date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = species,
         percent_cover = cover,
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  left_join(SEV.spp.list, by = c("spp_full_name" = "code"))%>%
  mutate(genus = case_when(spp_full_name == "STEM" ~ "Stachys",
                           spp_full_name == "PRGLT" ~ "Prosopis",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "STEM" ~"mexicana",
                             spp_full_name == "PRGLT" ~ "glandulosa",
                             TRUE ~ species))%>%
  mutate(spp_full_name = paste0(genus, " ", species))%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'web/plot', transect marks 'quadrat', aggregation likely site/plot/transect/y/m/d")

spp.check<-SEV.quad.cover.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


check1<-SEV.quad.cover.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

str(SEV.quad.cover.dat)
str(SEV.quad.cover.pcdat)

#Sevilleta (SEV) LTER - Quadrat Plant Species All Sites and Experiments
# - sev331_quadrat_plant_species_biomass.csv
#NOTE - MISSING TWO SITES GPS
SEV.quad.cover.ALL.pcdat<-SEV.quad.cover.ALL.dat%>%
  filter(cover > 0)%>%
  left_join(SEV.coords, by = c("site" = "Site"))%>%
  filter(!is.na(Latitude))%>%
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
         percent_cover = cover,
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
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(notes = "plot marks 'web/plot', transect marks 'quadrat', aggregation likely site/plot/transect/y/m/d")

str(SEV.quad.cover.ALL.dat)
str(SEV.quad.cover.ALL.pcdat)

spp.check<-SEV.quad.cover.ALL.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-SEV.quad.cover.ALL.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

#Sevilleta (SEV) LTER - Core Research Site Web Seasonal Biomass and Seasonal and Annual NPP Data for the Net Primary Production Study
# - sev182_NPP_core_biomass.csv
SEV.bio.npp.pcdat<-SEV.bio.npp.dat%>%
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
         percent_cover = cover,
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             is.na(species) ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'web/plot', transect marks 'quadrat', aggregation likely site/plot/transect/y/m/d")


str(SEV.bio.npp.dat)
str(SEV.bio.npp.pcdat)

spp.check<-SEV.bio.npp.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-SEV.bio.npp.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

#Sevilleta (SEV) LTER - Biome Transition Along Elevational Gradients in New Mexico (SEON) Study
# - sev292_flux_biomass.csv
SEV.flux.seon.pcdat<-SEV.flux.seon.dat%>%
  filter(cover > 0)%>%
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
         percent_cover = cover,
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
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(species = case_when(species == "sp." ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "transect marks 'quadrat', aggregation likely site/plot/transect/y/m/d")

str(SEV.flux.seon.dat)
str(SEV.flux.seon.pcdat)

spp.check<-SEV.flux.seon.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


check1<-SEV.flux.seon.pcdat%>%
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
#Crosschecking Sites
core.quads<-SEV.quad.cover.pcdat[,c("site", "plot", "transect", "year" , "month", "day")]%>%
  mutate(core = "X")%>%
  distinct()

all.quads<-SEV.quad.cover.ALL.pcdat[,c("site", "plot", "transect", "year" , "month", "day")]%>%
  mutate(all = "X")%>%
  distinct()

bio.npp.quads<-SEV.bio.npp.pcdat[,c("site", "plot", "transect", "year" , "month", "day")]%>%
  mutate(bio.npp = "X")%>%
  distinct()

bio.seon<-SEV.flux.seon.pcdat[,c("site", "plot", "transect", "year" , "month", "day")]%>%
  mutate(seon = "X")%>%
  distinct()

sev.cross<-core.quads%>%
  full_join(all.quads, by = c("site", "plot", "transect", "year" , "month", "day"))%>%
  full_join(bio.npp.quads, by = c("site", "plot", "transect", "year" , "month", "day"))%>%
  full_join(bio.seon, by = c("site", "plot", "transect", "year" , "month", "day"))%>%
  mutate(cross.1.2 = case_when(core == "X" & all == "X" ~ "X",
                           TRUE ~ NA),
         core.only = case_when(core=="X" & is.na(all) & is.na(bio.npp) & is.na(seon)  ~ "X" , #Some Cross
                               TRUE ~ NA),
         all.only = case_when(all=="X" & is.na(core) & is.na(bio.npp) & is.na(seon)   ~ "X" , 
                               TRUE ~ NA),
         bio.npp.only = case_when(bio.npp=="X" & is.na(core) & is.na(all) & is.na(seon)  ~ "X" ,  #All Bio.NPP sites are in other data
                                  TRUE ~ NA),
         bio.seon.only = case_when(seon=="X" & is.na(core) & is.na(all) & is.na(bio.npp)  ~ "X" , #All SEON sites are unique - exist only in SEON
                                  TRUE ~ NA))


sev.backcheck1<-SEV.quad.cover.pcdat%>%
  filter(site == "core_black" & plot == "1_E" & transect == "1" & year == "2000" & month == "5" & day == "16")

sev.backcheck2<-SEV.quad.cover.ALL.pcdat%>%
  filter(site == "core_black" & plot == "1_E" & transect == "1" & year == "2000" & month == "5" & day == "16")

sev.backcheck3<-SEV.bio.npp.pcdat%>%
  filter(site == "core_black" & plot == "1_E" & transect == "1" & year == "2000" & month == "5" & day == "16")

sev.backcheck4<-bind_rows(sev.backcheck1, sev.backcheck2, sev.backcheck3)%>%
  group_by(data, spp_full_name)%>%
  summarise(cov = sum(percent_cover))  #Same Data

nrow(sev.cross%>%filter(!is.na(core.only)))
nrow(sev.cross%>%filter(!is.na(all.only)))

############################################################
#Trimming Data Based on Overlap

# Identify the rows in SEV.quad.cover.pcdat that match the specified columns in SEV.quad.cover.ALL.pcdat
rows_to_remove <- SEV.quad.cover.pcdat %>%
  semi_join(SEV.quad.cover.ALL.pcdat, by = c("site", "plot", "transect", "year", "month", "day"))

# Remove the identified rows from SEV.quad.cover.pcdat
SEV.quad.cover.pcdat_filtered <- SEV.quad.cover.pcdat %>%
  anti_join(rows_to_remove, by = c("site", "plot", "transect", "year", "month", "day"))



############################################################
#ReCalculating ALL Quadrat Sites to Match with Biomass - Adding Transect Info

#Sevilleta (SEV) LTER - Quadrat Plant Species All Sites and Experiments
# - sev331_quadrat_plant_species_biomass.csv
#NOTE - MISSING TWO SITES GPS
SEV.quad.cover.ALL.pcdat<-SEV.quad.cover.ALL.dat%>%
  filter(cover > 0)%>%
  left_join(SEV.coords, by = c("site" = "Site"))%>%
  filter(!is.na(Latitude))%>%
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
         percent_cover = cover,
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
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(notes = "plot marks 'web/plot', transect marks 'quadrat/transect', aggregation likely site/plot/transect/y/m/d")

str(SEV.quad.cover.ALL.dat)
str(SEV.quad.cover.ALL.pcdat)







# Combine the filtered SEV.quad.cover.pcdat with SEV.quad.cover.ALL.pcdat
SEV_combined_df <- bind_rows(SEV.quad.cover.ALL.pcdat, SEV.quad.cover.pcdat_filtered)

sev.backcheck5<-SEV_combined_df%>%
  filter(site == "core_black" & plot == "1_E" & transect == "1" & year == "2000" & month == "5" & day == "16")

nrow(SEV_combined_df%>%
       select(site, plot, transect, year, month, day, data)%>%
       filter(data=="SEV LTER - Core Quadrat Data")%>%
       distinct())

nrow(sev.cross%>%filter(!is.na(core.only)))

############################################################
#Combining Final Dataframe

SEV.all.pcdata<-bind_rows(
  SEV_combined_df,
  SEV.flux.seon.pcdat
  
)%>%
  mutate(spp_full_name = gsub(" NA", " sp.", spp_full_name))

unique(SEV.all.pcdata$spp_full_name)
spp.check<-SEV.all.pcdata%>%
  select(spp_full_name, genus, species, data)%>%
  distinct()

spp.check.allquad<-SEV.quad.cover.ALL.dat%>%
  select(kartez, genus, sp.epithet)%>%
  distinct()

spp.check.seon<-SEV.flux.seon.dat%>%
  select(kartez, genus, sp.epithet)%>%
  distinct()

str(SEV.all.pcdata)
############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(SEV.all.pcdata, file="Percent.Cover.Species.GrassSav.SEV.LTER.All.csv", row.names = F)
############################################################
