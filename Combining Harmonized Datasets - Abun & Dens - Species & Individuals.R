############################################################
#Load Packages
library(tidyverse)

############################################################
#Set Working Directory for File Import
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Abun & Dens - Species & Individuals")

############################################################
#Importing and Combining Data
# Get a list of all .csv files in the working directory
csv_files <- list.files(pattern = "\\.csv$")

# Create an empty list to store data frames
ad_spp_data_list <- list()

# Define the column classes
col_classes <- c(
  "character", #std_id
  "character", #data
  "character", #ecosystem
  "character", #site
  "character", #plot
  "character", #transect
  "numeric", #latitude
  "numeric", #longitude
  "numeric", #year
  "numeric", #month
  "numeric", #day
  "character", #spp_full_name
  "character", #genus
  "character", #species
  "numeric",    #abundance
  "numeric", #density
  "character", #density units
  "character" # notes
)

# Loop through each CSV file and read it into a dataframe with the specified column classes
for (file in csv_files) {
  file_name <- tools::file_path_sans_ext(basename(file))  # Extract file name without extension
  ad_spp_data_list[[file_name]] <- read.csv(file, colClasses = col_classes)  # Read CSV file and store it in the list
}

# Combine all data frames in the list into a single data frame
powell.ad.spp.dat <- bind_rows(ad_spp_data_list)%>%
  mutate(ecosystem = case_when(ecosystem == "Grassland" ~ "Grassland/Savanna",
                               ecosystem == "Savanna" ~ "Grassland/Savanna",
                               ecosystem == "Salt Marsh & Estuary" ~ "Salt Marsh/Estuary/Wetland",
                               TRUE ~ ecosystem))

#Removing Duplicate Values from NOAA NERR MV / SAV Datasets
NERR.ad.subset<-powell.ad.spp.dat%>%
  filter(data %in% c("NOAA NERR - Marsh Vegetation", "NOAA NERR - SAV"))

NERR.ad.duplicates<- NERR.ad.subset %>%
  mutate(duplicate = duplicated(paste(site, plot, transect, 
                                      year, month, day,
                                      spp_full_name, genus, species, abundance, density)) | 
           duplicated(paste(site, plot, transect, 
                            year, month, day,
                            spp_full_name, genus, species, abundance, density), fromLast = TRUE))%>%
  filter(duplicate == "TRUE")%>%
  arrange(site, plot, transect, year, month, day, spp_full_name)

Non.NERR.ad.subset<-powell.ad.spp.dat%>%
  filter(!data %in% c("NOAA NERR - Marsh Vegetation", "NOAA NERR - SAV"))

nrow(NERR.ad.subset)+nrow(Non.NERR.ad.subset)
nrow(powell.ad.spp.dat)

cols_to_dup_check<-c("std_id", "site", "plot", "transect", 
                     "year", "month", "day",
                     "spp_full_name", "genus", "species",
                     "abundance", "density")

NERR.ad.subset.clean<-NERR.ad.subset%>%
  distinct(across(all_of(cols_to_dup_check)), .keep_all = TRUE)

nrow(NERR.ad.duplicates)/2
nrow(NERR.ad.subset)-nrow(NERR.ad.subset.clean)

#Remaking Full Dataset
powell.ad.spp.dat<-bind_rows(
  Non.NERR.ad.subset,
  NERR.ad.subset.clean
)

str(powell.ad.spp.dat)
unique(powell.ad.spp.dat$data)
length(unique(powell.ad.spp.dat$data))
length(unique(powell.ad.spp.dat$site))

data.test<-powell.ad.spp.dat%>%
  group_by(data, ecosystem)%>%
  summarise(length(unique(site)))%>%
  arrange(ecosystem, data)
print(data.test, n=nrow(data.test))

powell.ad.spp.dat%>%
  group_by(ecosystem)%>%
  summarise(length(unique(site)))

powell.ad.spp.dat%>%
  select(ecosystem, latitude, longitude)%>%
  distinct()%>%
  group_by(ecosystem)%>%
  summarise(n())

range(powell.ad.spp.dat$density)
range(powell.ad.spp.dat$abundance)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Fully Harmonized Data CSVs")
#Save Files
write.csv(powell.ad.spp.dat, file="Harmonized.Abundance.Density.Spp.Ind.csv", row.names = F)
############################################################
