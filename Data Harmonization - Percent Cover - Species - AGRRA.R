############################################################
#Load Packages
library(tidyverse)
library(readxl)


############################################################
#Automatic CSV Importing

#Atlantic and Gulf Rapid Reef Assessment (AGRRA) Benthic Data
# - Coral Composition by Site 4cm
# - CoralCompositionBySite-4cm.xlsx
AGRRA.corcomp.dat<-read_excel("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/AGRRA/CoralCompositionBySite-4cm.xlsx",
                              sheet =3)
str(AGRRA.corcomp.dat)

#Unified Coordinates
AGRRA.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/AGRRA/AGGRA.Unified.Site.Coords.RYF.csv")

############################################################
#Formatting Data

#Atlantic and Gulf Rapid Reef Assessment (AGRRA) Benthic Data
# - Coral Composition by Site 4cm
# - CoralCompositionBySite-4cm.xlsx
AGRRA.corcomp.pc.dat<-AGRRA.corcomp.dat%>%
  mutate(Code = case_when(is.na(Code) ~ Site,      #Accounting for NAs in Site Codes
                          TRUE ~ Code))%>%
  filter(!is.na(Latitude))%>%
  mutate(Date = as.Date(Date),
         Year = as.numeric(format(Date, "%Y")),  
         Year = case_when(Year == 2104 ~ 2014,     #Accounting for Year Typos
                          TRUE ~ Year),
         Site = Code, 
         Parameter = "Percent Cover",
         Data = "AGRRA - Coral Composition",
         Taxa = "Hard Coral",
         Specificity = "Species",
         Ecosystem = "Coral Reef",
         std_id = paste0("coral_", "AGRRA_", Site),
         plot = 999,
         transect = 999,
         date = as.Date(Date, format = "%m/%d/%Y"),  
         month = as.numeric(format(Date, "%m")),
         day = as.numeric(format(Date, "%d")))%>%  
  left_join(AGRRA.coords, by =c("Site"))%>%
  mutate(Latitude = Latitude.y,
         Longitude = Longitude.y)%>%
  select(std_id, Data, Ecosystem, Site, plot, transect, Latitude, Longitude, 
         Year, month, day, c(34:53))%>%
  rename( "Acropora cervicornis" = "%ACER",
          "Acropora palmata" = "%APAL",
          "Agaricia spp" = "%AGAR",
          "Colpophyllia natans" = "%CNAT",
          "Diploria labrynthiformis" = "%DLAB",
          "Millepora alcicornis" = "%MALC",
          "Millepora complanata" = "%MCOM",
          "Montastraea cavernosa" = "%MCAV",
          "Orbicella annularis" = "%OANN",
          "Orbicella faveolata" = "%OFAV",
          "Orbicella franksi" = "%OFRA",
          "Porites astreoides" = "%PAST", 
          "Porites furcata" = "%PFUR",
          "Porites porites" = "%PPOR",
          "Pseudodiploria clivosa" = "%PCLI",
          "Pseudodiploria strigosa" = "%PSTR",
          "Siderastrea siderea" = "%SSID",
          "Stephanocoenia intersepta" = "%SINT",
          "Undaria agaricites" = "%UAGA",
          "Undaria tenuifolia" = "%UTEN")%>%
  pivot_longer(cols = 12:ncol(.), 
               names_to = "spp_full_name", 
               values_to = "percent_cover" ) %>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  filter(!is.na(percent_cover))%>%   #Filter out 0 cover values
  mutate(species = case_when(species == "spp" ~ "998",
                             TRUE ~ species))%>%
  rename("data" = "Data",
         "ecosystem" = "Ecosystem",
         "site" = "Site",
         "latitude" = "Latitude",
         "longitude" = "Longitude",
         "year" = "Year")%>%
  mutate(notes = "aggregation at site/plot/transect/y/m/d")
#%>%
#  group_by(site, year)
#%>%
#  mutate(check = sum(percent_cover))%>%
#  ungroup()%>%
#  filter(check<101)%>%
#  select(-c(check))

str(AGRRA.corcomp.dat)
str(AGRRA.corcomp.pc.dat)
colnames(AGRRA.corcomp.pc.dat)

check1<-AGRRA.corcomp.pc.dat%>%group_by(site, year, month, day)%>%summarise(check = sum(percent_cover))
check2<-AGRRA.corcomp.pc.dat%>%filter(site=="COAL-04")
check3<-AGRRA.corcomp.dat%>%filter(Code=="COAL-04")
check4<-AGRRA.corcomp.pc.dat%>%filter(site=="COAL-05")
check5<-AGRRA.corcomp.dat%>%filter(Code=="COAL-05")
check6<-AGRRA.corcomp.pc.dat%>%filter(site=="HNROA001")
check7<-AGRRA.corcomp.dat%>%filter(Code=="HNROA001")

check1<-AGRRA.corcomp.pc.dat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

spp.check<-AGRRA.corcomp.pc.dat%>%
  select(spp_full_name, genus, species)%>%
  distinct()



############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(AGRRA.corcomp.pc.dat, file="Percent.Cover.Species.CoralReef.AGRRA.csv", row.names = F)
