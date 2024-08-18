############################################################
#Load Packages
library(tidyverse)

############################################################
#Automatic CSV Importing

# Define the path to the directory containing the .csv files
csv_directory <- "C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/NOAA NERR/NOAA NERR - Marsh Vegetation/Data/CSVs"

# List all .csv files in the directory
csv_files <- list.files(csv_directory, pattern = "\\.csv$", full.names = TRUE)

# Print the list of .csv files found
print("CSV files found:")
print(csv_files)

# Function to read a .csv file and remove the second row
read_and_clean_csv <- function(file) {
  # Read the .csv file
  df <- read.csv(file, stringsAsFactors = FALSE, colClasses = "character")
  
  # Remove the second row
  if (nrow(df) > 1) {
    df <- df[-1, ]
  }
  
  return(df)
}

# Loop through each .csv file, read it, clean it, and assign it to a dataframe
for (csv_file in csv_files) {
  # Read and clean the .csv file
  df <- read_and_clean_csv(csv_file)
  
  # Create a variable name based on the file name without extension
  file_name <- basename(csv_file)
  variable_name <- gsub("\\.csv$", "", file_name)
  
  # Assign the dataframe to a variable with the file name
  assign(variable_name, df, envir = .GlobalEnv)
  
  # Print the name of the dataframe being created
  print(paste("Created dataframe:", variable_name))
}

############################################################
#Fixing Extra Metadata Row in CBMVEG2011

CBMVEG2011<-CBMVEG2011[-1,]


############################################################
#Examining Non-Standard Dataframes

colnames(GTMVEG2014)


colnames(CBMVEG2011)
colnames(CBMVEG2012)
colnames(JACVEG2011)
colnames(JACVEG2013)
colnames(JACVEG2017)
colnames(KACVEG2010)
colnames(KACVEG2011)
colnames(KACVEG2012)
colnames(KACVEG2013)
colnames(MARVEG2011)
colnames(NARVEG2012)
colnames(NOCVEG2010)
colnames(NOCVEG2011)
colnames(NOCVEG2012)
colnames(TJRVEG2012)
colnames(WQBVEG2011)
colnames(WQBVEG2012)
colnames(WQBVEG2013)
colnames(WQBVEG2016)

############################################################
#Standardizing Dataframes

NERR.standards<-bind_rows(
  CBMVEG2011,
  CBMVEG2012,
  JACVEG2011,
  JACVEG2013,
  JACVEG2017,
  KACVEG2010,
  KACVEG2011,
  KACVEG2012,
  KACVEG2013,
  MARVEG2011,
  NARVEG2012,
  NOCVEG2010,
  NOCVEG2011,
  NOCVEG2012,
  TJRVEG2012,
  WQBVEG2011,
  WQBVEG2012,
  WQBVEG2013,
  WQBVEG2016
)%>%
  mutate(Orthometric.Height = NA,
         Height.Relative.to.MLLW = NA, 
         Maximum.Canopy.Height = Canopy.Height,
         Average.Canopy.Height = NA,
         Cover = X..Cover,
         QAQC = NA,
         SSAM.1 = NA)%>%
  select(Reserve, Type, Date, SiteID, TransectID, PlotID,
         Subplot, Rep, SSAM.1, Lat, Long, Distance, 
         Orthometric.Height, Height.Relative.to.MLLW, Species,
         Cover, Density, Maximum.Canopy.Height, Average.Canopy.Height,
         Diameter, Height, QAQC)

############################################################
#Combining Dataframes
NERR.MV.dat<-bind_rows(
  NERR.standards,
  APAVEG2014,
  APAVEG2015,
  APAVEG2016,
  APAVEG2017,
  APAVEG2018,
  APAVEG2019,
  APAVEG2020,
  APAVEG2021,
  APAVEG2022,
  APAVEG2023,
  DELVEG2012,
  ELKVEG2016,
  GRBVEG2010,
  GRBVEG2011,
  GRBVEG2013,
  GRBVEG2014,
  GRBVEG2016,
  GTMVEG2012,
  GTMVEG2013,
  GTMVEG2014,
  GTMVEG2015,
  GTMVEG2016,
  GTMVEG2017,
  GTMVEG2018,
  GTMVEG2019,
  GTMVEG2020,
  GTMVEG2021,
  GTMVEG2022,
  GTMVEG2023,
  HUDVEG2011,
  HUDVEG2012,
  HUDVEG2013,
  HUDVEG2014,
  HUDVEG2015,
  HUDVEG2016,
  HUDVEG2017,
  HUDVEG2018,
  HUDVEG2019,
  HUDVEG2020,
  lksveg2014,
  lksveg2015,
  lksveg2016,
  lksveg2017,
  LKSVEG2020,
  LKSVEG2021,
  LKSVEG2022,
  lksveg2023,
  NIWVEG2006,
  NIWVEG2007,
  NIWVEG2008,
  NIWVEG2009,
  NIWVEG2010,
  NIWVEG2011,
  NIWVEG2012,
  NIWVEG2013,
  NIWVEG2014,
  NIWVEG2015,
  NIWVEG2016,
  NIWVEG2017,
  NIWVEG2018,
  NIWVEG2019,
  NIWVEG2020,
  sfbveg2013,
  sfbveg2014,
  sfbveg2015,
  sfbveg2016,
  sfbveg2017,
  sfbveg2018,
  sfbveg2019,
  sfbveg2020,
  sfbveg2021,
  sfbveg2022,
  WELVEG2011,
  WELVEG2019,
  WELVEG2020,
  WELVEG2021,
  WELVEG2022,
  WELVEG2023
)

############################################################
#Fixing Reverse Lat/Long in WQB 
flipped.gps<-NERR.MV.dat%>%
  filter(Long > 0 & Lat <0)

NERR.MV.dat2<-NERR.MV.dat%>%
  mutate(lat2 = Long,
         switch = case_when(Long > 0 & Lat < 0 ~ "X",
                            TRUE ~ NA),
         Long = case_when(switch == "X" ~ Lat,
                          TRUE ~ Long),
         Lat = case_when(switch == "X" ~ lat2,
                         TRUE ~ Lat))

flipped.gps2<-NERR.MV.dat2%>%
  filter(Long > 0 & Lat <0)


############################################################
#Fixing Date Column from CSV Conversion

NERR.MV.dat2<-NERR.MV.dat2%>%
  mutate(Date2 = as.numeric(Date),
         Date2 = as.Date(Date2, origin = "1899-12-30"),
         Date2 = case_when(is.na(Date2) ~ as.character(Date),
                           TRUE ~ as.character(Date2)))

############################################################
#Cleaning Denisty Column

unique(NERR.MV.dat2$Density)

NERR.MV.dat2<-NERR.MV.dat2%>%
  mutate(Density = case_when(Density=="dominant or codominant species" |
                               Density=="Density" |
                               Density=="per m2, whole number" |
                               Density == "ND" ~ NA,
                             TRUE ~ Density),
         Density2 = as.numeric(Density),
         Density2 = round(Density2))

nerr.denscheck<-NERR.MV.dat2%>%
  select(Density, Density2)%>%
  distinct()

############################################################
#Filtering Data and Standardizing GPS
NERR.MV.dat2<-NERR.MV.dat2%>%
  filter(Reserve != "3 letter code")%>%
  filter(Reserve != "Reserve")

NERR.MV.dat3<-NERR.MV.dat2%>%
  group_by(Reserve, SiteID)%>%
  mutate(latitude = mean(as.numeric(Lat), na.rm = T),
         longitude =  mean(as.numeric(Long), na.rm = T))%>%
  ungroup()

############################################################
#Formatting Data

#Dataset Name 
# - CSV Name
NERR.MV.ABdat<-NERR.MV.dat3%>%
  mutate(site = paste0(toupper(Reserve), "_", toupper(SiteID)), 
         Parameter = "Percent Cover, Density",
         data = "NOAA NERR - Marsh Vegetation",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "NOAA.NERR_", site),
         subp = ifelse(is.na(Subplot), 999, Subplot),
         plot = paste0(PlotID, "_", subp),
         transect = paste0(TransectID, "_", Rep),
         date = lubridate::parse_date_time(Date2, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = Species,
         abundance = -9999,
         density = Density2,
         dens_units = "# per m2",
         latitude = latitude,
         longitude = longitude)%>%
  filter(!is.na(year))%>%            #Filtering out sites with no Date info
  filter(!is.na(Lat))%>%             #Filtering out sites with no GPS
  mutate(longitude = case_when(site == "CBM_IP" |
                                 site == "CBM_MP" |
                                 site == "CBM_RR" |
                                 site == "CBM_MCMS" |
                                 site == "CBM_MCHS" |
                                 site == "CBM_HAHA BRANCH" |
                                 site == "CBM_WOOD DUCK COVE" |
                                 site == "CBM_WINTERS RUN"~ longitude*-1,
                               TRUE ~ longitude))%>%
  mutate(spp_full_name = gsub("  ", " ", spp_full_name))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "H. wrightii" ~ "Halodule",
                           spp_full_name == "T. testudinum" ~ "Thalassia",
                           spp_full_name == "R. maritima" ~ "Ruppia",
                           spp_full_name == "S. filiforme" ~ "Syringodium",
                           spp_full_name == "H. englemanii" ~ "Halophila",
                           spp_full_name == "Bryophyta (aquatic)" |
                             spp_full_name == "unvegetated" |
                             spp_full_name == "Unvegetated" |
                             spp_full_name == "Algae" |
                             spp_full_name == "Bryophyta" |
                             spp_full_name == "Bare" |
                             spp_full_name == "Unknown 1" |
                             spp_full_name == "Unknown 2" |
                             spp_full_name == "Unknown 3" |
                             spp_full_name == "upland grass" |
                             spp_full_name == "Dead Red Cedar stump" |
                             spp_full_name == "unidentified grass" |
                             spp_full_name == "Plantae" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Typha x glauca" ~ "glauca",
                             spp_full_name == "Bryophyta (aquatic)" |
                               spp_full_name == "unvegetated" |
                               spp_full_name == "Unvegetated" |
                               spp_full_name == "Algae" |
                               spp_full_name == "Bryophyta" |
                               spp_full_name == "Bare" |
                               spp_full_name == "Unknown 1" |
                               spp_full_name == "Unknown 2" |
                               spp_full_name == "Unknown 3" |
                               spp_full_name == "upland grass" |
                               spp_full_name == "Dead Red Cedar stump" |
                               spp_full_name == "unidentified grass" |
                               spp_full_name == "Plantae" | 
                               species == "sp." |
                               species == "spp." |
                               species == "spp" |
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  filter(!is.na(density) & density > 0)%>%
  distinct()%>%
  mutate(notes = "plot marks 'Plot/Subplot', transect marks 'Transect/Rep', aggregation is at plot/subplot/transect/y/m/d")%>%
  mutate(dupes = case_when(std_id == "smew_NOAA.NERR_JAC_1" & plot == "1013-8-1_1" & transect == "8_1" & spp_full_name=="Spartina alterniflora" |
                             std_id == "smew_NOAA.NERR_LKS_PO" & plot == "4_999" & transect == "6.3_NA" & spp_full_name=="Cornus racemosa" ~ "X",
                           TRUE ~ NA))%>%
  filter(is.na(dupes))%>%
  select(-dupes)


str(NERR.MV.dat3)
str(NERR.MV.ABdat)

#Species checking
spp.check<-NERR.MV.ABdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

check2<-NERR.MV.ABdat%>%
  group_by(site, plot, transect, year, month, day)%>%mutate(check.full = sum(density))%>%
  ungroup()%>%
  group_by(site, plot, year, month, day)%>%mutate(check.site.plot = sum(density))%>%
  ungroup()%>%
  group_by(site, year, month, day)%>%mutate(check.site = sum(density))%>%
  ungroup()%>%
  group_by(site, plot, transect, year, month, day)%>%
  select(site, plot, transect, year, month, day, check.full, check.site.plot, check.site)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(NERR.MV.ABdat, file="AbunDens.Species.Indv.SMEW.NOAA.NERR.MV.csv", row.names = F)
############################################################
