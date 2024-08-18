############################################################
#Load Packages
library(tidyverse)

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
Krum.16.ADdat<-Krum.16.dat%>%
  mutate(SiteNum = dense_rank(interaction(Latitude, Longitude)),
         NewSite = paste0(Study, "_", SiteNum))%>%  #Making Actual Unique Site Names
  filter(!is.na(Individual.Density.num.per.sq.m))%>%                  #Filter to sites with density
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
         abundance = -9999,
         density = Individual.Density.num.per.sq.m,
         dens_units = "# per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  filter(str_count(spp_full_name, "\\s+") == 1)%>%  #filtering to sites with single species listed
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
  mutate(genus = case_when(spp_full_name == "Juv Laminariales.5" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Juv Laminariales.5" ~ "998",
                           TRUE ~ species))%>%
mutate(notes = "plot marks 'SampleID'; transect marks 'depth(m)'; Trimmed to sites with single species listed (others aggregate multiple species into single measurement)")

spp.check<-Krum.16.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(Krum.16.ADdat, file="AbunDens.Species.Indv.Kelp.Krumhansl.Dens.csv", row.names = F)
############################################################

  