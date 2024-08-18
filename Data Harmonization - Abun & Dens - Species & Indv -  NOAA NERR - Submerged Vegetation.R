############################################################
#Load Packages
library(tidyverse)
library(readxl)

############################################################
#Automatic CSV Importing

# Define the path to the directory containing the .csv files
csv_directory <- "C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/NOAA NERR/NOAA NERR - Submerged Vegetation/Data/CSVs"

# List all .csv files in the directory
csv_files <- list.files(csv_directory, pattern = "\\.csv$", full.names = TRUE)

# Print the list of .csv files found
print("CSV files found:")
print(csv_files)

# Function to read a .csv file and remove the second row
read_and_clean_csv <- function(file) {
  # Read the .csv file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
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
#Examining Non-Standard Dataframes

str(CBVVEG2013)
str(HUDVEG2015)

str(CBVVEG2011)
str(MARVEG2011)


colnames(CBVVEG2013)
colnames(HUDVEG2015)

colnames(CBVVEG2011)
colnames(MARVEG2011)

############################################################
#Standardizing Dataframes

CBVVEG2011_2<-CBVVEG2011%>%
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

MARVEG2011_2<-MARVEG2011%>%
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

NERR.SAV.dat<-bind_rows(
  CBVVEG2011_2,
  CBVVEG2013,
  CBVVEG2014,
  CBVVEG2015,
  CBVVEG2016,
  CBVVEG2017,
  ELKVEG2010,
  ELKVEG2011,
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
  MARVEG2011_2,
  PDBVEG2011,
  PDBVEG2012,
  PDBVEG2013,
  PDBVEG2014,
  PDBVEG2015,
  PDBVEG2016,
  PDBVEG2017,
  PDBVEG2018,
  PDBVEG2019,
  PDBVEG2020
  
)

str(NERR.SAV.dat)

############################################################
#Fixing Date Column from CSV Conversion

NERR.SAV.dat<-NERR.SAV.dat%>%
  mutate(Date = as.numeric(Date),
         Date = as.Date(Date, origin = "1899-12-30"))

############################################################
#Cleaning Denisty Column

unique(NERR.SAV.dat$Density)

NERR.SAV.dat2<-NERR.SAV.dat%>%
  mutate(Density = case_when(Density=="dominant or codominant species" |
                               Density=="Density" |
                               Density=="per m2, whole number" |
                               Density == "ND" ~ NA,
                             TRUE ~ Density),
         Density2 = as.numeric(Density),
         Density2 = round(Density2))

nerr.denscheck<-NERR.SAV.dat2%>%
  select(Density, Density2)%>%
  distinct()

############################################################
#Standardizing GPS

NERR.SAV.dat3<-NERR.SAV.dat2%>%
  group_by(Reserve, SiteID)%>%
  mutate(latitude = mean(as.numeric(Lat), na.rm = T),
         longitude =  mean(as.numeric(Long), na.rm = T))%>%
  ungroup()

############################################################
#Formatting Data

NERR.SAV.ADdat<-NERR.SAV.dat3%>%
  filter(!is.na(Density2) & Density2 > 0)%>%   #Filtering out points without percent cover measurements
  mutate(site = paste0(toupper(Reserve), "_", toupper(SiteID)), 
         Parameter = "Percent Cover, Density",
         data = "NOAA NERR - SAV",
         Taxa = "Vegetation",
         Specificity = "Species",
         ecosystem = "Salt Marsh & Estuary",
         std_id = paste0("smew_", "NOAA.NERR_", site),
         subp = ifelse(is.na(Subplot), 999, Subplot),
         plot = paste0(PlotID, "_", subp),
         transect = paste0(TransectID, "_", Rep),
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
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
  filter(!is.na(year))%>%  #Filtering out sites with no Date info
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
                             spp_full_name == "Plantae" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Typha x glauca" ~ "glauca",
                             spp_full_name == "Bryophyta (aquatic)" |
                               spp_full_name == "unvegetated" |
                               spp_full_name == "Unvegetated" |
                               spp_full_name == "Algae" |
                               spp_full_name == "Bryophyta" |
                               spp_full_name == "Bare" |
                               spp_full_name == "Plantae" | 
                               species == "sp." |
                               is.na(species) ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, abundance, density, dens_units)%>%
  mutate(notes = "plot marks 'Plot/Subplot', transect marks 'Transect/Rep', aggregation is at plot/subplot/transect/y/m/d")%>%
  mutate(dupes = case_when(std_id == "smew_NOAA.NERR_LKS_PO" & plot == "1_999" & transect == "1.1000000000000001_NA" & spp_full_name=="Typha sp." &
                             month == "8" & day == "17"|
                             std_id == "smew_NOAA.NERR_LKS_PO" & plot == "4_999" & transect == "6.3_NA" & spp_full_name=="Cornus racemosa" |
                             std_id == "smew_NOAA.NERR_LKS_PO" & plot == "1_999" & transect == "2.2999999999999998_NA" & spp_full_name=="Utricularia vulgaris"
                           & year == "2020" & month =="8" & day =="20" ~ "X",
                           TRUE ~ NA))%>%
  filter(is.na(dupes))%>%
  select(-dupes)


spp.check<-NERR.SAV.ADdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")
#Save Files
write.csv(NERR.SAV.ADdat, file="AbunDens.Species.Indv.SMEW.NOAA.NERR.SAV.csv", row.names = F)
############################################################

