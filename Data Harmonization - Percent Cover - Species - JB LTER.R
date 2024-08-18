############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#Jornada Basin (JB) LTER - Transect Plant Line-Intercept Data
# - Transect_Plant_Line_Intercept_Cover.csv
JB.tran.li.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/JB LTER/Transect_Plant_Line_Intercept_Cover.csv")
str(JB.tran.li.dat)

#Jornada Basin (JB) LTER - Permanent Quadrat Vegetation Cover
# - Jornada_permanent_quads_cover.csv
JB.quad.cov.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/JB LTER/Jornada_permanent_quads_cover.csv")
str(JB.quad.cov.dat)

#Jornada Basin (JB) LTER - Aboveground Net Primary Production
# - JRN011001_NPP_quadrat_estimates_SppSiteSeas.csv
JB.ag.npp.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/JB LTER/JRN011001_NPP_quadrat_estimates_SppSiteSeas.csv")
str(JB.ag.npp.dat)

#USDA Species Codes
JB.spp<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/USDA.clean.species.codes.csv")

jb.spp2<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/JB LTER/JRN vascular plant species list.csv")%>%
  select(LTER_code, Genus_USDA, Species_USDA)%>%
  distinct()

############################################################
#Trimming to Control Sites where Necessary

#Jornada Basin (JB) LTER - Transect Plant Line-Intercept Data
# - Transect_Plant_Line_Intercept_Cover.csv
JB.tran.li.dat<-JB.tran.li.dat%>%
  filter(transect != "T")

############################################################
#Formatting Data

#Jornada Basin (JB) LTER - Transect Plant Line-Intercept Data
# - Transect_Plant_Line_Intercept_Cover.csv
JB.tran.li.pcdat<-JB.tran.li.dat%>%
  filter(!is.na(cover_percent))%>%   #Filter out obs with no data
  mutate(date = as.Date(date),
         year = as.numeric(format(date, "%Y")),
         site = "JB.TPLID", 
         Parameter = "Percent Cover",
         data = "JB LTER - Line Intercept Transects",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "JB.LTER_", site),
         plot = station,
         transect = transect,
         month = as.numeric(format(date, "%m")),
         day = as.numeric(format(date, "%d")),
         spp_full_name = Species_binomial,
         percent_cover = cover_percent,
         latitude = (32.7494871 + 32.47317259)/2,
         longitude =  (-106.8728831 + -106.6927163)/2)%>%       
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "road; no vegetation" |
                             spp_full_name == "rock; no vegetation" |
                             spp_full_name == "soil pit; vegetation not sampled" |
                             spp_full_name == "unidentified plant" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "road; no vegetation" |
                             spp_full_name == "rock; no vegetation" |
                             spp_full_name == "soil pit; vegetation not sampled" |
                             spp_full_name == "unidentified plant" |
                             species =="species" ~ "998",
                           TRUE ~ genus))%>%
  mutate(notes = "plot marks 'station'; transects mark controls (C) and alternative controls (X), see metadata for usage, aggragation appears to be at site/plot/transect/y/m/d")

str(JB.tran.li.dat)
str(JB.tran.li.pcdat)
JB.tran.li.pcdat[26133,]

spp.check<-JB.tran.li.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()



check1<-JB.tran.li.pcdat%>%
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
write.csv(JB.tran.li.pcdat, file="Percent.Cover.Species.GrassSav.JBLTER.TPLID.csv", row.names = F)
############################################################


#Jornada Basin (JB) LTER - Permanent Quadrat Vegetation Cover
# - Jornada_permanent_quads_cover.csv
JB.quad.cov.pcdat<-JB.quad.cov.dat%>%
  filter(area != ".")%>%                      #Filter out obs with no data
  mutate(site = "JB Quadrats (122)", 
         Parameter = "Percent Cover",
         data = "JB LTER - Permanent Quadrats",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "JB.LTER_", site),
         plot = quadrat,
         transect = 999,
         spp_full_name = scientific_name,
         month = as.numeric(month),
         day = ifelse(day == ".", 999, day),
         day = as.numeric(day),
         percent_cover = as.numeric(area)*100,
         latitude = (32.737108 + 32.466879)/2,
         longitude =  (-106.926435 + -106.528942)/2)%>%       
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(spp_full_name = case_when(spp_full_name == "." ~ "998",
                                   TRUE ~ spp_full_name),
         genus = case_when(genus == "." ~ "998",
                           TRUE ~ genus),
         species = case_when(species == "sp." |
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'quadrat', aggregation at site/plot")


str(JB.quad.cov.dat)
str(JB.quad.cov.pcdat)
unique(JB.quad.cov.dat$perimeter)

spp.check<-JB.quad.cov.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-JB.quad.cov.pcdat%>%
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
write.csv(JB.quad.cov.pcdat, file="Percent.Cover.Species.GrassSav.JBLTER.Quads.csv", row.names = F)
############################################################


#Jornada Basin (JB) LTER - Aboveground Net Primary Production
# - JRN011001_NPP_quadrat_estimates_SppSiteSeas.csv
JB.ag.npp.pcdat<-JB.ag.npp.dat%>%
  mutate(xsite = site,
         site = "JB NPP (15 sites)", 
         Parameter = "NPP",
         data = "JB LTER - Aboveground NPP",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "JB.LTER_", site),
         plot = quad,
         transect = paste0(xsite, "_", zone),
         date = lubridate::parse_date_time(date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         percent_cover = cum_cover,
         latitude = (32.669000 + 32.488000)/2,
         longitude =  (-106.865000 + -106.713000)/2)%>%
  left_join(JB.spp, by = c("USDA_code" = "USDA_code"))%>%
  mutate(spp_full_name = species_name)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(spp_full_name = case_when(is.na(spp_full_name) ~ "998",
                                   TRUE ~ spp_full_name),
         genus = case_when(is.na(genus) ~ "998",
                                   TRUE ~ genus),
         species = case_when(is.na(species) |
                               species == "Ruiz" |
                               species == "R." |
                               species == "P." |
                               species == "Nutt." |
                               species == "Mill." |
                               species == "Lehm." |
                               species == "Lag." |
                               species == "L." |
                               species == "Cav." |
                               species == "Adans." |
                               species == "A." |
                               species == "(Nutt." ~ "998",
                                   TRUE ~ species))%>%
  group_by(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day, spp_full_name, genus, species)%>%
  summarise(percent_cover = sum(percent_cover, na.rm=T))%>%
  filter(!is.na(percent_cover) & percent_cover>0)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(notes = "plot marks 'quad'; transect marks (site - within data)/zone; species codes incomplete: missing ~9% of species (998s in spp_full_name)")

str(JB.ag.npp.dat)
str(JB.ag.npp.pcdat)

#Species checking
spp.check<-JB.ag.npp.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-JB.ag.npp.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

JB.ag.npp.pcdat<-as.data.frame(JB.ag.npp.pcdat)
############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(JB.ag.npp.pcdat, file="Percent.Cover.Species.GrassSav.JBLTER.NPP.csv", row.names = F)
############################################################

                                                        