############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#SBC Site Coordinates
SBC.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Santa Barbara Coastal LTER/SBC_LTER_Site_Coords.csv")
str(SBC.coords)

#Santa Barbara Coastal (SBC) LTER - Giant Kelp Forest Community Dynamis (KFCD) - Biomass of Algae, Invertebrates, and Fish
# - Annual_All_Species_Biomass_at_transect_20240501.csv
SBC.all.bmass.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Santa Barbara Coastal LTER/Annual_All_Species_Biomass_at_transect_20240501.csv")
str(SBC.all.bmass.dat)


############################################################
#Formatting Data

#Santa Barbara Coastal (SBC) LTER - Giant Kelp Forest Community Dynamis (KFCD) - Biomass of Algae, Invertebrates, and Fish
# - Annual_All_Species_Biomass_at_transect_20240501.csv
SBC.all.bmass.biodat<-SBC.all.bmass.dat%>%
  left_join(SBC.coords, by=c("SITE" = "Site"))%>%
  filter(TAXON_KINGDOM == "Plantae")%>%  #Filtering to algae
  mutate(site = SITE, 
         Parameter = "Percent Cover",
         data = "SBC LTER - Algae, Invert, Fish Bio",
         Taxa = "Kelp, Algae",
         Specificity = "Species",
         ecosystem = "Kelp",
         std_id = paste0("kelp_", "SBC.LTER_", site),
         plot = "999",
         transect = TRANSECT ,
         date = lubridate::parse_date_time(DATE, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = SCIENTIFIC_NAME,
         biomass = WM_GM2,
         bmass_units = "g per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, biomass, bmass_units)%>%
  filter(!is.na(biomass) & biomass > 0)%>%
  mutate(genus = case_when(spp_full_name == "Neoptilota spp.; Ptilota spp.; Rhodoptilum spp." |
                             spp_full_name == "Ulva spp.; Sponogomorpha spp." |
                             spp_full_name == "Chondracanthus corymbiferus; Chondracanthus exasperatus" |
                             spp_full_name == "crustose coralline algae spp." |
                             spp_full_name == "Halymenia spp.; Schizymenia pacifica" |
                             spp_full_name == "Unidentifiable Branching Red Alga" |
                             spp_full_name == "Unidentified Erect Coralline spp." ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Neoptilota spp.; Ptilota spp.; Rhodoptilum spp." |
                               spp_full_name == "Ulva spp.; Sponogomorpha spp." |
                               spp_full_name == "Chondracanthus corymbiferus; Chondracanthus exasperatus" |
                               spp_full_name == "crustose coralline algae spp." |
                               spp_full_name == "Halymenia spp.; Schizymenia pacifica" |
                               spp_full_name == "Unidentifiable Branching Red Alga" |
                               spp_full_name == "Unidentified Erect Coralline spp." ~ "998",
                             TRUE ~ species),
         spp_full_name = case_when(spp_full_name == "small Ceramiaceae spp." ~ "Ceramiaceae spp.",
                                   TRUE ~ spp_full_name))%>%
  mutate(species = case_when(species == "spp." |
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  mutate(genus = case_when(spp_full_name == "Ceramiaceae spp." ~ "Ceramiaceae",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Ceramiaceae spp." ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "transect marks TRANSECT, aggregation likely site/transect/y/m/d, mass is 'areal wet mass'")


str(SBC.all.bmass.dat)
str(SBC.all.bmass.biodat)

sbc.spp.check<-SBC.all.bmass.biodat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-SBC.all.bmass.biodat%>%
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
write.csv(SBC.all.bmass.biodat, file="Percent.Cover.Species.Kelp.SBC.LTER.AlgBio.BMass.csv", row.names = F)
############################################################