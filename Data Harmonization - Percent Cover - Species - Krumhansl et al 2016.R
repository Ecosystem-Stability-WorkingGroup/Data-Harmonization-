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
Krum.16.pcdat<-Krum.16.dat%>%
  mutate(SiteNum = dense_rank(interaction(Latitude, Longitude)),
         NewSite = paste0(Study, "_", SiteNum))%>%  #Making Actual Unique Site Names
  filter(!is.na(Percent.Cover))%>%                  #Filter to sites with percent cover
  filter(Percent.Cover > 0)%>%                      #Filter 0 percent cover values
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
         percent_cover = Percent.Cover,
         latitude = Latitude,
         longitude =  Longitude)%>%
  filter(str_count(spp_full_name, "\\s+") == 1)%>%  #filtering to sites with single species listed
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
       year, month, day, spp_full_name, percent_cover)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(notes = "plot marks 'SampleID'; transect marks 'depth(m)' aggregation likely site/plot/y/m/d; Trimmed to sites with single species listed (others aggregate multiple species into single measurement), *Some sites have >100% cover listed in raw data, may need to trim")

str(Krum.16.dat)
str(Krum.16.pcdat)

spp.check<-Krum.16.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


check1<-Krum.16.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  arrange(site, plot, year)

check2<-Krum.16.pcdat%>%
  filter(site == "Norway_monitoring_kelp_data_1221" & plot =="B11.KYO Hardbunn.1990.5.24.4" & transect == "999" & year == "1990" & month == "5" & day == "24")

check2.2<-Krum.16.pcdat%>%
  filter(site == "Norway_monitoring_kelp_data_1221" & plot =="B11.KYO Hardbunn.1990.5.24.3" & transect == "999" & year == "1990" & month == "5" & day == "24")

check2.3<-check1%>%
  filter(site == "Norway_monitoring_kelp_data_1221" & plot =="B11.KYO Hardbunn.1990.5.24.3" & transect == "999" & year == "1990" & month == "5" & day == "24")

check3<-Krum.16.dat%>%
  mutate(SiteNum = dense_rank(interaction(Latitude, Longitude)),
         site = paste0(Study, "_", SiteNum))%>% 
  filter(site == "Norway_monitoring_kelp_data_1221"  & Sample.Year == "1990" & Sample.Month == "5" & Sample.Day == "24")


############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(Krum.16.pcdat, file="Percent.Cover.Species.Kelp.Krumhansl.2016.csv", row.names = F)