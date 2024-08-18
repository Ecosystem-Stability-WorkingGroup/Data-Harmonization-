############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#SBC Site Coordinates
SBC.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Santa Barbara Coastal LTER/SBC_LTER_Site_Coords.csv")
str(SBC.coords)

#Santa Barbara Coastal (SBC) LTER - Giant Kelp Forest Community Dynamis (KFCD) - Abundance of Algae and Invertebrates
# - Annual_Quad_Swath_All_Years_20230814.csv
SBC.alg.invert.dens.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Santa Barbara Coastal LTER/Annual_Quad_Swath_All_Years_20230814.csv")
str(SBC.alg.invert.dens.dat)

#Santa Barbara Coastal (SBC) LTER - Giant Kelp Forest Community Dynamis (KFCD) - Abundance and Size of Giant Kelp
# - Annual_Kelp_All_Years_20240305.csv
SBC.kelp.abunsize.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Kelp/Santa Barbara Coastal LTER/Annual_Kelp_All_Years_20240305.csv")
str(SBC.kelp.abunsize.dat)

############################################################
#Formatting Data

#Santa Barbara Coastal (SBC) LTER - Giant Kelp Forest Community Dynamis (KFCD) - Abundance and Size of Giant Kelp
# - Annual_Kelp_All_Years_20240305.csv
SBC.kelp.abunsize.ADdat<-SBC.kelp.abunsize.dat%>%
  left_join(SBC.coords, by=c("SITE" = "Site"))%>%
  mutate(site = SITE, 
         Parameter = "Abundance",
         data = "SBC LTER - Kelp Abundance & Size",
         Taxa = "Kelp",
         Specificity = "Species",
         ecosystem = "Kelp",
         std_id = paste0("kelp_", "SBC.LTER_", site),
         plot = "999",
         transect = paste0(TRANSECT , "_", SIDE),
         date = lubridate::parse_date_time(DATE, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = SCIENTIFIC_NAME,
         latitude = Latitude,
         longitude =  Longitude)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, AREA)%>%
  group_by(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day, spp_full_name, AREA)%>%
  mutate(abundance = n(),
         density = abundance/AREA)%>%
  ungroup()%>%
  distinct()%>%
  mutate(dens_units = "# per m2")%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
  mutate(notes = "transect marks TRANSECT/SIDE, aggregation site/transect/y/m/d")



str(SBC.kelp.abunsize.dat)
str(SBC.kelp.abunsize.ADdat)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(SBC.kelp.abunsize.ADdat, file="AbunDens.Species.Indv.Kelp.SBC.LTER.KelpDens.csv", row.names = F)
############################################################

#Santa Barbara Coastal (SBC) LTER - Giant Kelp Forest Community Dynamis (KFCD) - Abundance of Algae and Invertebrates
# - Annual_Quad_Swath_All_Years_20230814.csv
SBC.alg.invert.dens.ADdat<-SBC.alg.invert.dens.dat%>%
  left_join(SBC.coords, by=c("SITE" = "Site"))%>%
  filter(TAXON_KINGDOM == "Plantae")%>%  #Filtering to algae
  mutate(site = SITE, 
         Parameter = "Density",
         data = "SBC LTER - Algae & Sessile Inverts Dens",
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
         abundance = COUNT,
         density = COUNT/AREA,
         dens_units = "# per m2",
         latitude = Latitude,
         longitude =  Longitude)%>%
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
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  filter(!is.na(abundance) & !is.na(density))%>%
  filter(abundance != 0 & density != 0)%>%
  mutate(notes = "plot marks QUAD, transect marks TRANSECT/SIDE, aggregation likely site/plot/transect/y/m/d")


str(SBC.alg.invert.dens.dat)
str(SBC.alg.invert.dens.ADdat)

#Species checking
spp.check<-SBC.alg.invert.dens.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


############################################################
#Save File Appropriate Folders

#Set Working Directory
#setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
#write.csv(SBC.alg.invert.dens.ADdat, file="AbunDens.Species.Indv.Kelp.SBC.LTER.AlgInvDens.csv", row.names = F)
############################################################