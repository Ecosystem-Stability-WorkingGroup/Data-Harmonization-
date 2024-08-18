############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#Krumhansl et al. 2016 - Global patterns of kelp forest change over the past half-century
# - krumhansl_kelp_timeseries_raw.csv
Krum.16.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Krumhansl et al 2016/krumhansl_kelp_timeseries_raw.csv")
str(Krum.16.dat)

############################################################
#Formatting Data

#Krumhansl et al. 2016 - Global patterns of kelp forest change over the past half-century
# - krumhansl_kelp_timeseries_raw.csv
Krum.16.biodat<-Krum.16.dat%>%
  mutate(SiteNum = dense_rank(interaction(Latitude, Longitude)),
         NewSite = paste0(Study, "_", SiteNum))%>%  #Making Actual Unique Site Names
  filter(!is.na(Biomass.kg.wet.per.sq.m))%>%                  #Filter to sites with biomass
  filter(Biomass.kg.wet.per.sq.m > 0)%>%                      #Filter 0 biomass values
  mutate(site = NewSite, 
         Parameter = "Percent Cover, Density, Biomass",
         data = "Krumhansl et al 2016 - Global Kelp",
         Taxa = "Kelp",
         Specificity = "Species",
         ecosystem = "Kelp",
         std_id = paste0("kelp_", "Krumhansl_", site),
         plot = Sample.ID,
         transect = as.character(Depth.m),
         transect = case_when(is.na(transect) ~ "999",
                              TRUE ~ transect),
         year = Sample.Year,
         month = Sample.Month,
         month = ifelse(is.na(month), 999, month),
         day = Sample.Day,
         day = ifelse(is.na(day), 999, day),
         spp_full_name = Taxon,
         biomass = Biomass.kg.wet.per.sq.m*1000,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  filter(str_count(spp_full_name, "\\s+") == 1)%>%  #filtering to sites with single species listed
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
       year, month, day, spp_full_name, biomass, bmass_units)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(notes = "plot marks 'SampleID'; transect marks 'depth(m)'; Trimmed to sites with single species listed (others aggregate multiple species into single measurement), biomass is wet weight")

str(Krum.16.dat)
str(Krum.16.biodat)

spp.check<-Krum.16.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


check1<-Krum.16.biodat%>%
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
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Biomass - Species & Individuals")
#Save Files
write.csv(Krum.16.biodat, file="Biomass.Species.Indv.Kelp.Krum16.BMass.csv", row.names = F)
############################################################
