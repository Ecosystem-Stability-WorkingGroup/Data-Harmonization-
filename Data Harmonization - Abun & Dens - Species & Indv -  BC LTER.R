############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#BC LTER Shurb, Seedling, Sapling Density - 530_ShrubSeedlingSaplingDensity_1975-2021.csv
bc.lter.sss.den<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/BC LTER/530_ShrubSeedlingSaplingDensity_1975-2021.csv")
str(bc.lter.sss.den)

#Species Code List - SSS
bc.lter.spp.codes<-read.table("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/BC LTER/PlantSpeciesList_Export.txt", 
                              sep = ",", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)
str(bc.lter.spp.codes)

#Site Coordinates
bc.lter.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/BC LTER/BC.LTER.Site.Coords.csv")

############################################################
#Formatting Data4

#BC LTER Shurb, Seedling, Sapling Density - 530_ShrubSeedlingSaplingDensity_1975-2021.csv
bc.lter.sss.ADdat<-bc.lter.sss.den%>%
  mutate(Site = recode(Site, "BDI8" = "BDi8",       #Site alternatively named BDI8 and BDi8, fixing.
                       "FPOA" = "FP0A",       #Site alternatively named FPOA and FP0A, fixing.
                       "FPOC" = "FP0C"))%>%   #Site alternatively named FPOC and FP0C, fixing. 
  filter(Sample.Date != "")%>%                      #Removing site (UP4C) with no Date Info
  left_join(bc.lter.coords, by=c("Site"))%>%
  mutate(site = Site,
         Parameter = "Density",
         data = "BC LTER - SSS Density",
         Taxa = "Shrubs, Seedlings, Saplings",
         Specificity = "Species",
         ecosystem = "Forests",
         std_id = paste0("forest_", "BC.LTER_", site),
         plot = 999,
         transect = 999,
         date = lubridate::parse_date_time(Sample.Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         abundance = Count,
         density = X..ha,
         dens_units = "number per ha",
         latitude = Latitude,
         longitude = Longitude)%>%
  left_join(bc.lter.spp.codes%>%select(LTER_code, genus, species),
            by = c("spp_full_name" = "LTER_code"))


str(bc.lter.sss.den)
str(bc.lter.sss.ADdat)

#Species checking
spp.check<-bc.lter.sss.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

#Backcheck for aggregation levels
check1<-XXXX%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(biomass))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(biomass))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

check2<-XXXX%>%
  filter(site == "" & plot == "" & transect == "", & year == "" & month == "" & day == "")

