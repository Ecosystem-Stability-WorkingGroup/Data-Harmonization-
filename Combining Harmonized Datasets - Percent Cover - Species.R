############################################################
#Load Packages
library(tidyverse)

############################################################
#Set Working Directory for File Import
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Percent Cover - Species")

############################################################
#Importing and Combining Data
# Get a list of all .csv files in the working directory
csv_files <- list.files(pattern = "\\.csv$")

# Create an empty list to store data frames
pc_spp_data_list <- list()

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
  "numeric",    #percent_cover
  "character" # notes
)

# Loop through each CSV file and read it into a dataframe with the specified column classes
for (file in csv_files) {
  file_name <- tools::file_path_sans_ext(basename(file))  # Extract file name without extension
  pc_spp_data_list[[file_name]] <- read.csv(file, colClasses = col_classes)  # Read CSV file and store it in the list
}

# Combine all data frames in the list into a single data frame
powell.pc.spp.dat <- bind_rows(pc_spp_data_list)%>%
  mutate(ecosystem = case_when(ecosystem == "Grassland" ~ "Grassland/Savanna",
                               ecosystem == "Savanna" ~ "Grassland/Savanna",
                               ecosystem == "Salt Marsh & Estuary" ~ "Salt Marsh/Estuary/Wetland",
                               TRUE ~ ecosystem))

#Removing Duplicate Values from NOAA NERR MV / SAV Datasets
NERR.pc.subset<-powell.pc.spp.dat%>%
  filter(data %in% c("NOAA NERR - Marsh Vegetation", "NOAA NERR - SAV"))

NERR.pc.duplicates<- NERR.pc.subset %>%
  mutate(duplicate = duplicated(paste(site, plot, transect, 
                                      year, month, day,
                                      spp_full_name, genus, species, percent_cover)) | 
           duplicated(paste(site, plot, transect, 
                            year, month, day,
                            spp_full_name, genus, species, percent_cover), fromLast = TRUE))%>%
  filter(duplicate == "TRUE")%>%
  arrange(site, plot, transect, year, month, day, spp_full_name)

Non.NERR.pc.subset<-powell.pc.spp.dat%>%
  filter(!data %in% c("NOAA NERR - Marsh Vegetation", "NOAA NERR - SAV"))

nrow(NERR.pc.subset)+nrow(Non.NERR.pc.subset)
nrow(powell.pc.spp.dat)

cols_to_dup_check<-c("std_id", "site", "plot", "transect", 
                     "year", "month", "day",
                     "spp_full_name", "genus", "species",
                     "percent_cover")

NERR.pc.subset.clean<-NERR.pc.subset%>%
  distinct(across(all_of(cols_to_dup_check)), .keep_all = TRUE)

nrow(NERR.pc.duplicates)/2
nrow(NERR.pc.subset)-nrow(NERR.pc.subset.clean)

#Remaking Full Dataset
powell.pc.spp.dat<-bind_rows(
  Non.NERR.pc.subset,
  NERR.pc.subset.clean
)


str(powell.pc.spp.dat)
unique(powell.pc.spp.dat$data)
length(unique(powell.pc.spp.dat$data))
length(unique(powell.pc.spp.dat$site))

data.test<-powell.pc.spp.dat%>%
  group_by(data, ecosystem)%>%
  summarise(length(unique(site)))%>%
  arrange(ecosystem, data)
print(data.test, n=nrow(data.test))

powell.pc.spp.dat%>%
  group_by(ecosystem)%>%
  summarise(length(unique(site)))

powell.pc.spp.dat%>%
  select(ecosystem, latitude, longitude)%>%
  distinct()%>%
  group_by(ecosystem)%>%
  summarise(n())

range(powell.pc.spp.dat$percent_cover)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Fully Harmonized Data CSVs")
#Save Files
write.csv(powell.pc.spp.dat, file="Harmonized.Percent.Cover.Spp.Ind.csv", row.names = F)
############################################################
