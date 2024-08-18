############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

#FIA - PLOT Table - Coordinates and Other Metadata
# - ENTIRE_PLOT.csv
FIA.coords<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/FIA/ENTIRE_PLOT.csv")
str(FIA.coords)
FIA.coords.trim<-FIA.coords%>%
  select(CN, STATECD, UNITCD, COUNTYCD, INVYR, PLOT, LAT, LON, MEASYEAR, MEASMON, MEASDAY, P2VEG_SAMPLING_STATUS_CD, P2VEG_SAMPLING_LEVEL_DETAIL_CD)
str(FIA.coords.trim)

#FIA Understory Vegetation
# - ENTIRE_P2VEG_SUBPLOT_SPP.csv
FIA.veg<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/FIA/ENTIRE_P2VEG_SUBPLOT_SPP.csv")
str(FIA.veg)

#FIA Species Code List
FIA.spp.codes<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/FIA/ENTIRE_REF_PLANT_DICTIONARY.csv")
str(FIA.spp.codes)
FIA.spp.codes1<-FIA.spp.codes%>%
  filter(SYMBOL_TYPE =="Species")%>%
  select(SYMBOL, SCIENTIFIC_NAME)

FIA.spp.codes2<-FIA.spp.codes%>%
  filter(SYMBOL_TYPE == "Old")%>%
  select(NEW_SYMBOL, NEW_SCIENTIFIC_NAME)%>%
  rename("SYMBOL" = "NEW_SYMBOL",
         "SCIENTIFIC_NAME" = "NEW_SCIENTIFIC_NAME")

FIA.spp.codes3<-FIA.spp.codes%>%
  filter(SYMBOL_TYPE =="Genus")%>%
  select(SYMBOL, SCIENTIFIC_NAME)

FIA.spp.codes4<-FIA.spp.codes%>%
  filter(SYMBOL_TYPE =="Unknown")%>%
  select(SYMBOL, COMMON_NAME)%>%
  mutate(SCIENTIFIC_NAME = gsub(" ", "_", COMMON_NAME))%>%
  select(SYMBOL, SCIENTIFIC_NAME)

FIA.spp.codes.final<-bind_rows(
  FIA.spp.codes1,
  FIA.spp.codes2,
  FIA.spp.codes3,
  FIA.spp.codes4
)%>%
  distinct()

df_duplicates <- FIA.spp.codes.final%>%
  group_by(!!sym("SYMBOL")) %>%
  filter(n() > 1) %>%
  ungroup()

############################################################
#Pulling in Tree Data for Coord Cross

#Pulling in Data by State:
# Specify the folder containing the .csv files
folder_path <- "C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/FIA/State Tree Files"

# List all .csv files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Columns to select
tree_cols <- c("CN", "PLT_CN", "STATECD", "UNITCD", "COUNTYCD", "STATUSCD", "INVYR", "PLOT", "SUBP", "TREE", "SPCD", "TPA_UNADJ", "DIA", "MORTYR")

# Function to read and select specific columns
read_and_select <- function(file) {
  read_csv(file, col_select = tree_cols)
}

# Import and stack all .csv files
FIA.tree.dat <- csv_files %>%
  set_names(gsub("\\.csv$", "", basename(.))) %>%
  map_dfr(read_and_select, .id = "source")


############################################################
#Foramtting GPS Points to Match Other FIA Data

FIA.tree.pc.coords<-FIA.tree.dat%>%
  filter(STATUSCD == 1)%>%       #Live Trees Only
  left_join(FIA.coords.trim, by = c("PLT_CN" = "CN", 
                                    "STATECD" = "STATECD", 
                                    "UNITCD"="UNITCD", 
                                    "COUNTYCD" = "COUNTYCD", 
                                    "INVYR" = "INVYR",
                                    "PLOT" = "PLOT"))%>%
  mutate(site = paste0(STATECD, "_", UNITCD, "_", COUNTYCD, "_", PLOT, "_"))%>%
  select(site, LAT, LON)%>%
  group_by(site)%>%
  mutate(latitude = mean(LAT, na.rm = T),
         longitude = mean(LON, na.rm=T))%>%
  ungroup()%>%
  select(-c(LAT, LON))%>%
  distinct()

FIA.tree.mortdat.coords%>%filter(site == "17_1_127_13_")%>% 
  mutate(across(c(latitude, longitude), ~ sprintf("%.4f", .)))

FIA.tree.mortdat.coords%>%filter(site == "17_1_127_38_")%>% 
  mutate(across(c(latitude, longitude), ~ sprintf("%.4f", .)))

############################################################
#Formatting Data

#FIA Understory Vegetation
# - ENTIRE_P2VEG_SUBPLOT_SPP.csv

FIA.coords.veg.pcdat<-FIA.veg%>%
  left_join(FIA.coords.trim, by = c("PLT_CN" = "CN", 
                                    "STATECD" = "STATECD", 
                                    "UNITCD"="UNITCD", 
                                    "COUNTYCD" = "COUNTYCD", 
                                    "INVYR" = "INVYR",
                                    "PLOT" = "PLOT"))%>%
  filter(P2VEG_SAMPLING_STATUS_CD %in% c("1", "2"))%>%
  mutate(year = MEASYEAR,
         site = paste0(STATECD, "_", UNITCD, "_", COUNTYCD, "_", PLOT, "_"),                                 
         Parameter = "Percent Cover",
         data = "FIA - Understory Vegetation Data",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Forests",
         percent_cover = COVER_PCT,
         std_id = paste0("forest_", "FIA_", site),
         plot = PLOT,
         transect = SUBP,
         month = MEASMON,
         day = MEASDAY)%>%
  left_join(FIA.tree.mortdat.coords, by = "site")%>%
  group_by(site)%>%
  mutate(lat2 = mean(LAT, na.rm = T),
         lon2 = mean(LON, na.rm=T))%>%
  ungroup()%>%
  mutate(latitude = case_when(is.na(latitude) ~ lat2,
                              TRUE ~ latitude),
         longitude = case_when(is.na(longitude) ~ lon2,
                               TRUE ~ longitude))%>%
  left_join(FIA.spp.codes.final[,c("SYMBOL", "SCIENTIFIC_NAME")], by=c("VEG_FLDSPCD" = "SYMBOL"))%>%
  mutate(spp_full_name = SCIENTIFIC_NAME)%>%
  mutate(spp_full_name = gsub(" x ", " x", spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(spp_full_name = case_when(is.na(spp_full_name) ~ "998",
                                     TRUE ~ spp_full_name))%>%
  mutate(genus = case_when(grepl("_", spp_full_name) |
                             is.na(genus) ~ "998",
                           TRUE ~ genus),
         species = case_when(grepl("_", spp_full_name) |
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'plot', transect marks 'subplot', aggregation at site/plot/transect/y/m/d")
  
  
str(FIA.veg)
str(FIA.coords.veg.pcdat)
  
spp.check<-FIA.coords.veg.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()
  
spp.check%>%filter(grepl(" x", spp_full_name))
spp.check%>%filter(grepl("_", spp_full_name))

check1<-FIA.coords.veg.pcdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(percent_cover))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

sapply(FIA.coords.veg.pcdat, function(x) sum(is.na(x)))

  
############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")
#Save Files
write.csv(FIA.coords.veg.pcdat, file="Percent.Cover.Species.Forest.FIA.Vegetation.csv", row.names = F)
############################################################

  