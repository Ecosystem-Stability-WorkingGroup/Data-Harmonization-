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
tree_cols <- c("CN", "PLT_CN", "STATECD", "UNITCD", "COUNTYCD", "STATUSCD", "INVYR", "PLOT", "SUBP", "TREE", "SPCD", "TPA_UNADJ", "DIA")

# Function to read and select specific columns
read_and_select <- function(file) {
  read_csv(file, col_select = tree_cols)
}

# Import and stack all .csv files
FIA.tree.dat <- csv_files %>%
  set_names(gsub("\\.csv$", "", basename(.))) %>%
  map_dfr(read_and_select, .id = "source")

# 'source' column will contain the original file name without the ".csv"

#Growth Columns
FIA.abun<-read_csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/FIA/ENTIRE_TREE_GRM_COMPONENT.csv",
                     col_select = c("PLT_CN", "STATECD", "TRE_CN", "SUBP_TPAGROW_UNADJ_AL_FOREST"))

############################################################
#Formatting Data

FIA.tree.ADdat<-FIA.tree.dat%>%
  filter(STATUSCD == 1)%>%       #Live Trees Only
  left_join(FIA.coords.trim, by = c("PLT_CN" = "CN", 
                                    "STATECD" = "STATECD", 
                                    "UNITCD"="UNITCD", 
                                    "COUNTYCD" = "COUNTYCD", 
                                    "INVYR" = "INVYR",
                                    "PLOT" = "PLOT"))%>%
  left_join(FIA.abun, by = c("PLT_CN" = "PLT_CN",
                               "STATECD" = "STATECD",
                               "CN" = "TRE_CN"))%>%
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
         day = ifelse(is.na(day), 999, day))%>%
  group_by(site)%>%
  mutate(latitude = mean(LAT, na.rm = T),
         longitude = mean(LON, na.rm=T))%>%
  ungroup()%>%
  left_join(FIA.spp.list, by = c("SPCD"))%>%
  mutate(spp_full_name = paste0(genus, " ", species))%>%
  mutate(species = case_when(species == "spp." ~ "998", 
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, TREE, DIA, DESIGNCD, SUBP_TPAGROW_UNADJ_AL_FOREST)%>%
  group_by(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
           year, month, day, spp_full_name, genus, species)%>%
  summarise(abundance = n(),
            density = sum(SUBP_TPAGROW_UNADJ_AL_FOREST))%>%
  mutate(density = density/4046.85642,
         dens_units = "# per m2")%>%
  mutate(notes = "plot marks 'plot', transect marks 'subplot', aggregation at site/plot/transect/y/m/d - density values calculated from SUBP_TPAGROW_UNADJ_AL_FOREST in TREE_GRM_COMPONENT table")


str(FIA.tree.dat)
str(FIA.tree.ADdat)

#Species checking
spp.check<-FIA.tree.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

backcheck<-FIA.tree.ADdat%>%
  filter(std_id=="forest_FIA_10669179010497" & plot =="2368" & transect=="1" & year=="1995" & month == "7" & day == "26")

xxx<-as.data.frame(FIA.tree.ADdat)

#Duplicate Checking
duplicates<- xxx%>%
  mutate(duplicate = duplicated(paste(site, plot, transect, 
                                      year, month, day,
                                      spp_full_name, genus, species)) | 
           duplicated(paste(site, plot, transect, 
                            year, month, day,
                            spp_full_name, genus, species), fromLast = TRUE))%>%
  filter(duplicate == "TRUE")%>%
  arrange(desc(duplicate), site, plot, transect, year, month, day, spp_full_name)

sapply(FIA.tree.ADdat, function(x) sum(is.na(x)))

FIA.tree.ADdat.final<-as.data.frame(FIA.tree.ADdat)%>%
  filter(!is.na(latitude))%>%             #removing sites with no gps data
  filter(!is.na(year))%>%                 #removing sites with no year data
  mutate(density = case_when(is.na(density) |
                               density == 0 ~ -9999,
                             TRUE ~ density),
         dens_units = case_when(density == -9999 ~ "no density measure",
                                TRUE ~ dens_units))
  
  
str(FIA.tree.ADdat.final)
sapply(FIA.tree.ADdat.final, function(x) sum(is.na(x)))

backcheck2<-FIA.tree.ADdat.final%>%
  filter(density ==0)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(FIA.tree.ADdat.final, file="AbunDens.Species.Indv.Forest.FIA.TREE.csv", row.names = F)
############################################################

