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
  select(CN, STATECD, UNITCD, COUNTYCD, INVYR, PLOT, LAT, LON, MEASYEAR, MEASMON, MEASDAY, DESIGNCD)

#FIA Species List
FIA.spp.list<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/FIA/fia_spp_list_2.csv")
str(FIA.spp.list)

#Pulling in Data by State:
# Specify the folder containing the .csv files
folder_path <- "C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/FIA/State Tree Files"

# List all .csv files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Columns to select
tree_cols <- c("PLT_CN", "STATECD", "UNITCD", "COUNTYCD", "STATUSCD", "INVYR", "PLOT", "SUBP", "TREE", "SPCD", "TPA_UNADJ", "DIA")

# Function to read and select specific columns
read_and_select <- function(file) {
  read_csv(file, col_select = tree_cols)
}

# Import and stack all .csv files
FIA.tree.dat <- csv_files %>%
  set_names(gsub("\\.csv$", "", basename(.))) %>%
  map_dfr(read_and_select, .id = "source")

# 'source' column will contain the original file name without the ".csv"

############################################################
#Formatting Data

FIA.tree.DIAdat<-FIA.tree.dat%>%
  filter(STATUSCD == 1)%>%       #Live Trees Only
  left_join(FIA.coords.trim, by = c("PLT_CN" = "CN", 
                                    "STATECD" = "STATECD", 
                                    "UNITCD"="UNITCD", 
                                    "COUNTYCD" = "COUNTYCD", 
                                    "INVYR" = "INVYR",
                                    "PLOT" = "PLOT"))%>%
  mutate(year = MEASYEAR,
         site = paste0(STATECD, "_", UNITCD, "_", COUNTYCD, "_", PLOT, "_"),                                 
         Parameter = "Growth, Density, Mortality",
         data = "FIA - Tree Data",
         Taxa = "Trees",
         Specificity = "Individuals",
         ecosystem = "Forests",
         std_id = paste0("forest_", "FIA_", site),
         plot = PLOT,
         transect = SUBP,
         month = MEASMON,
         month = ifelse(is.na(month), 999, month),
         day = MEASDAY,
         day = ifelse(is.na(day), 999, day),
         tree_id = paste0(STATECD, "_", UNITCD, "_", COUNTYCD, "_", PLOT, "_", SUBP, "_", DESIGNCD, "_", TREE),
         stem_id = as.character("no_stem_id"),
         diameter = DIA*2.54,
         diam_units = "cm")%>%
  group_by(site)%>%
  mutate(latitude = mean(LAT, na.rm = T),
         longitude = mean(LON, na.rm=T))%>%
  ungroup()%>%
  left_join(FIA.spp.list, by = c("SPCD"))%>%
  mutate(spp_full_name = paste0(genus, " ", species))%>%
  mutate(species = case_when(species == "spp." ~ "998", 
                             TRUE ~ species))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(species = case_when(species == "spp." ~ "998",
                               spp_full_name == "Musa x paradisiaca" ~ "xparadisiaca",
                               spp_full_name == "Salix x sepulcralis" ~ "xsepulcralis",
                               spp_full_name == "Citrus x sinensis" ~ "xsinensis",
                               spp_full_name == "Citrus x aurantiifolia" ~ "xaurantiifolia",
                               spp_full_name == "Citrus x paradisi" ~ "xparadisi",
                               spp_full_name == "Citrus x limon" ~ "xlimon",
                               TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter) & diameter >0)%>%    
  mutate(notes = "plot marks 'plot', transect marks 'subplot', tree_id marks statecd/unitcd/countycd/plot/subplot/designcd/tree, diameter converted from in to cm")




str(FIA.tree.dat)
str(FIA.tree.DIAdat)

#Species checking
spp.check<-FIA.tree.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

FIA.tree.DIAdat.fin<-FIA.tree.DIAdat%>%
  filter(!is.na(year))



############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(FIA.tree.DIAdat.fin, file="Diameter.Species.Indv.Forest.FIA.DHB.csv", row.names = F)
############################################################

