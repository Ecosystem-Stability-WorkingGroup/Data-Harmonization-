############################################################
#Load Packages
library(tidyverse)
library(lubridate)

############################################################
#Automatic CSV Importing

#Caribbean Coastal Marine Productivity (CARICOMP) Program - Site Coordinates
# - Coral_Station.csv
CARICOMP.coral.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/CARICOMP/Coral_Station.csv",
                                fileEncoding = "Latin1")
str(CARICOMP.coral.coords)

#Caribbean Coastal Marine Productivity (CARICOMP) Program - Coral Links
# - Coral_Links.csv
CARICOMP.coral.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/CARICOMP/Coral_Links.csv")
str(CARICOMP.coral.dat)

#Species Codes
CARICOMP.spp.codes<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/CARICOMP/caricomp_spp_codes.csv")
str(CARICOMP.spp.codes)

#Category Codes
CARICOMP.cat.codes<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Coral Reefs/CARICOMP/caricomp_cat_codes.csv")
str(CARICOMP.cat.codes)


############################################################
#Converting to Decimal Degrees

# Function to convert DMS to decimal degrees
dms_to_decimal <- function(dms) {
  # Extract degrees, minutes, and seconds using regular expressions
  dms <- gsub("[^0-9°'\".]", "", dms) # Remove any non-numeric and non-DMS characters
  parts <- unlist(regmatches(dms, gregexpr("[0-9]+", dms)))
  
  # Convert parts to numeric
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- ifelse(length(parts) == 3, as.numeric(parts[3]), 0)
  
  # Calculate decimal degrees
  decimal_degrees <- degrees + minutes / 60 + seconds / 3600
  return(decimal_degrees)
}


# Apply the conversion function to the dataframe
CARICOMP.coral.coords <- CARICOMP.coral.coords %>%
  mutate(
    latitude_dd = sapply(lat.ryf, dms_to_decimal),
    longitude_dd = sapply(lon.ryf, dms_to_decimal)*-1
  )


############################################################
#Formatting Data

#Caribbean Coastal Marine Productivity (CARICOMP) Program - Coral Links
# - Coral_Links.csv
CARICOMP.coral.pcdat<-CARICOMP.coral.dat%>%
  mutate(Station = case_when(Station == "El Pe  n" ~ "El Peñón", 
                             Station == "Playa Caim n 1" ~ "Playa Caimán 1",
                             Station == "Playa Caim n 2" ~ "Playa Caimán 2",
                             TRUE ~ Station))%>%
  left_join(CARICOMP.coral.coords, by = c("Station"))%>%
  mutate(Date_notime = substr(Date, 1, nchar(Date) -5),
         site = Station, 
         Parameter = "Abundance",
         data = "CARICOMP - Substrate Data",
         Taxa = "Hard Coral, Soft Coral, Algae",
         Specificity = "Species",
         ecosystem = "Coral Reef",
         std_id = paste0("coral_", "CARICOMP_", site),
         plot = 999,
         transect = Transect,
         date = lubridate::parse_date_time(Date_notime, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         latitude = latitude_dd,
         longitude =  longitude_dd)%>%
  mutate(Species.Code = str_to_upper(Species.Code))%>%
  mutate(Category.Code = str_to_upper(Category.Code))%>%
  left_join(CARICOMP.spp.codes%>%
              mutate(Species.Code = str_to_upper(Species.Code)), by=c("Species.Code"))%>%
  left_join(CARICOMP.cat.codes%>%
              mutate(Category.Code = str_to_upper(Category.Code)), by =c("Category.Code"))%>%
  mutate(spp_full_name = Species.Name)%>%
  mutate(spp_full_name = case_when(is.na(spp_full_name) ~ Name,
                                   TRUE ~ spp_full_name))%>%
  mutate(spp_full_name = gsub("  ", " ", spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Anemones" |
                           spp_full_name == "Bare Boulder" |
                           spp_full_name == "Branching Corals" |
                           spp_full_name == "Calcareous Algae" |
                           spp_full_name == "Corallimorpharians" |
                           spp_full_name == "Dead Corals" |
                           spp_full_name == "Encrusting calcareous algae" |
                           spp_full_name == "Encrusting Corals" |
                           spp_full_name == "Encrusting Gorgonians" |
                           spp_full_name == "Encrusting Sponges" |
                           spp_full_name == "Erect Sponges" |
                           spp_full_name == "Fleshy Algae" |
                           spp_full_name == "Foliaceous Corals" |
                           spp_full_name == "Holes" |
                           spp_full_name == "Gorgonians" |
                           spp_full_name == "Massive Corals" |
                           spp_full_name == "Milleporines" |
                           spp_full_name == "Other" |
                           spp_full_name == "Bare Rock" |
                           spp_full_name == "Bare Rubble" |
                           spp_full_name == "Bare Sediment" |
                           spp_full_name == "Turf Algae" |
                           spp_full_name == "Zoanthids" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Anemones" |
                             spp_full_name == "Bare Boulder" |
                             spp_full_name == "Branching Corals" |
                             spp_full_name == "Calcareous Algae" |
                             spp_full_name == "Corallimorpharians" |
                             spp_full_name == "Dead Corals" |
                             spp_full_name == "Encrusting calcareous algae" |
                             spp_full_name == "Encrusting Corals" |
                             spp_full_name == "Encrusting Gorgonians" |
                             spp_full_name == "Encrusting Sponges" |
                             spp_full_name == "Erect Sponges" |
                             spp_full_name == "Fleshy Algae" |
                             spp_full_name == "Foliaceous Corals" |
                             spp_full_name == "Holes" |
                             spp_full_name == "Gorgonians" |
                             spp_full_name == "Massive Corals" |
                             spp_full_name == "Milleporines" |
                             spp_full_name == "Other" |
                             spp_full_name == "Bare Rock" |
                             spp_full_name == "Bare Rubble" |
                             spp_full_name == "Bare Sediment" |
                             spp_full_name == "Turf Algae" |
                             spp_full_name == "Zoanthids" |
                             species == "sp." ~ "998",
                           TRUE ~ species))%>%
  group_by(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day, spp_full_name, genus, species)%>%
  summarise(count = n())%>%
  ungroup()%>%
  group_by(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day)%>%
  mutate(points = sum(count))%>%
  ungroup()%>%
  mutate(percent_cover = (count/points)*100)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(notes = "transect marks 'Transect'; aggregation site/transect/y/m/d")


str(CARICOMP.coral.dat)
str(CARICOMP.coral.pcdat)

#Species checking
spp.check<-CARICOMP.coral.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

spp.check2<-CARICOMP.coral.pcdat%>%
  select(Category.Code, Species.Code, spp_full_name, genus, species)%>%
  distinct()

#Backcheck for aggregation levels
check1<-CARICOMP.coral.pcdat%>%
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
write.csv(CARICOMP.coral.pcdat, file="Percent.Cover.Species.CoralReef.CARICOMP.csv", row.names = F)
############################################################
