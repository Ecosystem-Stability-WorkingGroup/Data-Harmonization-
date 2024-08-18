############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#SBC Site Coordinates
SBC.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Santa Barbara Coastal LTER/SBC_LTER_Site_Coords.csv")
str(SBC.coords)

#Santa Barbara Coastal (SBC) LTER - Giant Kelp Forest Community Dynamis (KFCD) - Percent Cover Algae and Sessile Invertebrates	
# - Annual_Cover_All_Years_20240501.csv
SBC.alg.invert.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Santa Barbara Coastal LTER/Annual_Cover_All_Years_20240501.csv")
str(SBC.alg.invert.dat)

#Santa Barbara Coastal (SBC) LTER - Giant Kelp Forest Community Dynamis (KFCD) - Biomass of Algae, Invertebrates, and Fish
# - Annual_All_Species_Biomass_at_transect_20240501.csv
SBC.all.bmass.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Santa Barbara Coastal LTER/Annual_All_Species_Biomass_at_transect_20240501.csv")
str(SBC.all.bmass.dat)


############################################################
#Formatting Data

#Santa Barbara Coastal (SBC) LTER - Giant Kelp Forest Community Dynamis (KFCD) - Percent Cover Algae and Sessile Invertebrates	
# - Annual_Cover_All_Years_20240501.csv
SBC.alg.invert.pcdat<-SBC.alg.invert.dat%>%
  left_join(SBC.coords, by=c("SITE" = "Site"))%>%
  filter(TAXON_KINGDOM == "Plantae")%>%  #Filtering to algae
  filter(PERCENT_COVER > 0)%>%           #Filtering out 0 Values
  mutate(site = SITE, 
         Parameter = "Percent Cover",
         data = "SBC LTER - Algae & Sessile Inverts PC",
         Taxa = "Kelp, Algae",
         Specificity = "Species",
         ecosystem = "Kelp",
         std_id = paste0("kelp_", "SBC.LTER_", site),
         plot = QUAD,
         transect = paste0(TRANSECT , "_", SIDE),
         date = lubridate::parse_date_time(DATE, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = SCIENTIFIC_NAME,
         percent_cover = PERCENT_COVER,
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
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
  mutate(notes = "plot marks QUAD, transect marks TRANSECT/SIDE, aggregation likely site/plot/transect/y/m/d, **TWO different percent cover values available between this and SBC biomass data, be careful to not duplicate")

sbc.spp.check<-SBC.alg.invert.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-SBC.alg.invert.pcdat%>%
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
write.csv(SBC.alg.invert.pcdat, file="Percent.Cover.Species.Kelp.SBC.LTER.AlgaePC.csv", row.names = F)
############################################################

#Santa Barbara Coastal (SBC) LTER - Giant Kelp Forest Community Dynamis (KFCD) - Biomass of Algae, Invertebrates, and Fish
# - Annual_All_Species_Biomass_at_transect_20240501.csv
SBC.all.bmass.pcdat<-SBC.all.bmass.dat%>%
  left_join(SBC.coords, by=c("SITE" = "Site"))%>%
  filter(TAXON_KINGDOM == "Plantae")%>%  #Filtering to algae
  filter(PERCENT_COVER > 0)%>%           #Filtering out 0 Values
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
         percent_cover = PERCENT_COVER,
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
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
  mutate(notes = "transect marks TRANSECT, aggregation likely site/transect/y/m/d - **TWO different percent cover values available between this and SBC quadrat data, be careful to not duplicate")


str(SBC.all.bmass.dat)
str(SBC.all.bmass.pcdat)

sbc.spp.check<-SBC.all.bmass.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-SBC.all.bmass.pcdat%>%
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
write.csv(SBC.all.bmass.pcdat, file="Percent.Cover.Species.Kelp.SBC.LTER.AlgBio.PC.csv", row.names = F)
############################################################



############################################################
#Crosschecking Sites

sbc.quads<-SBC.alg.invert.pcdat[,c("site",  "year" , "month", "day")]%>%
  mutate(quads = "X")%>%
  distinct()

sbc.trans<-SBC.all.bmass.pcdat[,c("site",  "year" , "month", "day")]%>%
  mutate(trans = "X")%>%
  distinct()

sbc.cross<-sbc.quads%>%
  full_join(sbc.trans, by = c("site", "year" , "month", "day"))%>%
  mutate(quads.conly = case_when(quads=="X" & is.na(trans)  ~ "X" ,  #All Bio.NPP sites are in other data
                                  TRUE ~ NA),
         trans.only = case_when(trans=="X" & is.na(quads)  ~ "X" , #All SEON sites are unique - exist only in SEON
                                   TRUE ~ NA))


sbc.quads<-SBC.alg.invert.pcdat[,c("site",  "year" , "month", "day", "genus", "species", "percent_cover")]%>%
  group_by(site, year, month, day, genus, species)%>%
  summarise(pc.quads = sum(percent_cover))%>%
  distinct()

sbc.trans<-SBC.all.bmass.pcdat[,c("site",  "year" , "month", "day", "genus", "species", "percent_cover")]%>%
  group_by(site, year, month, day, genus, species)%>%
  summarise(pc.trans = sum(percent_cover))%>%
  distinct()

sbc.cross<-sbc.quads%>%
  full_join(sbc.trans, by = c("site", "year" , "month", "day", "genus", "species"))
