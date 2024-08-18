############################################################
#Load Packages
library(tidyverse)


############################################################
#Automatic CSV Importing

#Konza Prairie (KP) LTER - Site Coordinates
KP.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/KP LTER/KP.LTER.Site.Coords.csv")
str(KP.coords)
KP.coords<-KP.coords%>%
  mutate(Latitude = (Northern + Southern)/2,
         Longitude = (Western + Eastern)/2)

#Konza Prairie (KP) LTER - NUT01- Nutrient Network: Investigating the Roles of Nutrient Availability and Vertebrate Herbivory on Grassland Structure
#Plant Species Composition
# - NUT011.csv
KP.nut01.spcomp.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/KP LTER/NUT01/NUT011.csv")
str(KP.nut01.spcomp.dat)

#Konza Prairie (KP) LTER - NUT01- Treatment Table
KP.nut01.treatments<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/KP LTER/NUT01/NUT01_Treatment_Table.csv")
str(KP.nut01.treatments)

#Konza Prairie (KP) LTER - WAT01 - Konza Prairie Long-Term Irrigation Transect Study
# - Species binned percent cover
# - WAT012.csv
KP.wat01.pc.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/KP LTER/WAT01/WAT012.csv")
str(KP.wat01.pc.dat)

#Konza Prairie (KP) LTER - PPL01 - Konza Prairie Long-Term Phosphorous Plots Experiment
# - Species-specific abundance data
# - PPL011.csv
KP.ppl01.abun.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Grasslands/KP LTER/PPL01/PPL011.csv")
str(KP.ppl01.abun.dat)
############################################################
#Trimming to Control Sites for Experimental Plots

#Konza Prairie (KP) LTER - NUT01- Nutrient Network: Investigating the Roles of Nutrient Availability and Vertebrate Herbivory on Grassland Structure
#Plant Species Composition
# - NUT011.csv
KP.nut01.spcomp.dat<-KP.nut01.spcomp.dat%>%
  left_join(KP.nut01.treatments, by=c("Block", "Plot"))%>% #Need to Crosscheck this is correct matching (subplots dont fully join)
  filter(treat_other_name == "control")

table(KP.nut01.spcomp.dat$Subplot.x, KP.nut01.spcomp.dat$Subplot.y)

#Konza Prairie (KP) LTER - WAT01 - Konza Prairie Long-Term Irrigation Transect Study
# - Species binned percent cover
# - WAT012.csv
KP.wat01.pc.dat<-KP.wat01.pc.dat%>%
  filter(Trt != "i")

#Konza Prairie (KP) LTER - PPL01 - Konza Prairie Long-Term Phosphorous Plots Experiment
# - Species-specific abundance data
# - PPL011.csv
KP.ppl01.abun.dat<-KP.ppl01.abun.dat%>%
  filter(Treatment == "N1P0")

############################################################
#Formatting Data

#Konza Prairie (KP) LTER - NUT01- Nutrient Network: Investigating the Roles of Nutrient Availability and Vertebrate Herbivory on Grassland Structure
#Plant Species Composition
# - NUT011.csv
KP.nut01.spcomp.pcdat<-KP.nut01.spcomp.dat%>%
  mutate(Site = "2C")%>%
  left_join(KP.coords, by=c("Site"))%>%
  mutate(site = Site,
         Parameter = "Percent Cover",
         data = "KP LTER - NutNet Percent Cover",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "KP.LTER_", site),
         plot = Block,
         transect = paste0(Plot, "_", Subplot.x),
         Date = if_else(Date == "", NA_character_, Date),
         date = parse_date_time(Date, orders = c("mdy", "dmy")),
         year = year(date),
         year = ifelse(is.na(year), RecYear, year),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = Taxa,
         spp_full_name = str_replace(spp_full_name, "^([a-z])", toupper),
         percent_cover = Cover,
         latitude = Latitude,
         longitude =  Longitude)%>%       
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, percent_cover)%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  filter(percent_cover > 0)%>%
  mutate(genus = case_when(spp_full_name == "Bare_ground "|
                             spp_full_name == "Unk_1" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Bare_ground "|
                             spp_full_name == "Unk_1" ~ "998",
                             spp_full_name == "Symphyotrichym (Aster) ericoides" ~ "ericoides",
                             spp_full_name == "Symphyotrichym (Aster) oblongifolium" ~ "oblongifolium",
                             spp_full_name == "Sporobolus (compositus) asper" ~ "asper",
                             spp_full_name == "Brickellia (Kuhnia) eupatoriodes" ~ "eupatoriodes",
                           TRUE ~ species))%>%
  mutate(notes = "plot marks 'block'; transect marks 'plot_subplot'; aggregation site/plot/y/m/dr; control sites only")

str(KP.nut01.spcomp.dat)
str(KP.nut01.spcomp.pcdat)


#Species checking
spp.check<-KP.nut01.spcomp.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-KP.nut01.spcomp.pcdat%>%
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
write.csv(KP.nut01.spcomp.pcdat, file="Percent.Cover.Species.GrassSav.KPLTER.NUT01.csv", row.names = F)
############################################################

#Konza Prairie (KP) LTER - PPL01 - Konza Prairie Long-Term Phosphorous Plots Experiment
# - Species-specific abundance data
# - PPL011.csv
KP.ppl01.abun.pcdat<-KP.ppl01.abun.dat%>%
  mutate(WATERSHED = "002c")%>%
  left_join(KP.coords, by=c("WATERSHED"))%>%                    #Matching Site Names
  mutate(site = current.name,
         year = RecYear,
         Parameter = "Percent Cover",
         data = "KP LTER - PPL01 Percent Cover",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         std_id = paste0("grass.sav_", "KP.LTER_", site),
         plot = PlotID,
         transect = 999,
         #Date = if_else(Date == "", NA_character_, Date),
         #date = parse_date_time(Date, orders = c("mdy", "dmy")),
         #year = year(date),
         #year = ifelse(is.na(year), RecYear, year),
         month = 999,
         #month = ifelse(is.na(month), 999, month),
         day = 999,
         #day = ifelse(is.na(day), 999, day),
         genus = str_to_title(Genus),
         species = Species,
         spp_full_name = paste0(genus, " ", species),
         percent_cover = Abundance,
         latitude = Latitude,
         longitude =  Longitude)%>%       
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(species = case_when(species == "spp." ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'plotID', aggregation at site/plot/y/m/d")




str(KP.ppl01.abun.dat)
str(KP.ppl01.abun.pcdat)


#Species checking
spp.check<-KP.ppl01.abun.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check1<-KP.ppl01.abun.pcdat%>%
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
write.csv(KP.ppl01.abun.pcdat, file="Percent.Cover.Species.GrassSav.KPLTER.PPL01.csv", row.names = F)
############################################################


#Konza Prairie (KP) LTER - WAT01 - Konza Prairie Long-Term Irrigation Transect Study
# - Species binned percent cover
# - WAT012.csv
KP.wat01.pc.pcdat<-KP.wat01.pc.dat%>%
  mutate(WATERSHED = "0hqb")%>%
  left_join(KP.coords, by=c("WATERSHED"))%>%                    #Matching Site Names
  mutate(site = current.name,
         year = RecYear,
         month = RecMonth,
         day = RecDay,
         Parameter = "Percent Cover",
         data = "KP LTER - WAT01 Percent Cover",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Grassland",
         latitude = Latitude,
         longitude =  Longitude,
         std_id = paste0("grass.sav_", "KP.LTER_", site),
         plot = Plot,
         transect = Transect,
         genus = str_to_title(Genus),
         species = Species,
         spp_full_name = paste0(genus, " ", species),
         percent_cover = Cover)%>%
  mutate(percent_cover = case_when(percent_cover == 1 ~ mean(c(0,1)),
                                   percent_cover == 2 ~ mean(c(1,5)),
                                   percent_cover == 3 ~ mean(c(5,25)),
                                   percent_cover == 4 ~ mean(c(25,50)),
                                   percent_cover == 5 ~ mean(c(50,75)),
                                   percent_cover == 6 ~ mean(c(75,95)),
                                   percent_cover == 7 ~ mean(c(95,100)),
                                   TRUE ~ NA))
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(notes = "control sites only; plot marks 'plot'; transect marks 'trasnect'; percent cover is midpoint of bins (5-25% groups, see metadata)")


str(KP.wat01.pc.dat)
str(KP.wat01.pc.pcdat)

#Species checking
spp.check<-KP.wat01.pc.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-KP.wat01.pc.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

