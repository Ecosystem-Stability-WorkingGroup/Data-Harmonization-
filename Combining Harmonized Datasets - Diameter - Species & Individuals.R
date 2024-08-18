############################################################
#Load Packages
library(tidyverse)

############################################################
#Set Working Directory for File Import
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")

############################################################
#Importing and Combining Data
# Get a list of all .csv files in the working directory
csv_files <- list.files(pattern = "\\.csv$")

# Create an empty list to store data frames
diam_spp_data_list <- list()

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
  "character",    #tree_id
  "character", #stem_id
  "numeric",  #diameter
  "character", #diam_units
  "character" # notes
)

# Loop through each CSV file and read it into a dataframe with the specified column classes
for (file in csv_files) {
  file_name <- tools::file_path_sans_ext(basename(file))  # Extract file name without extension
  diam_spp_data_list[[file_name]] <- read.csv(file, colClasses = col_classes)  # Read CSV file and store it in the list
}

# Combine all data frames in the list into a single data frame
powell.dm.spp.dat <- bind_rows(diam_spp_data_list)%>%
  mutate(ecosystem = case_when(ecosystem == "Grassland" ~ "Grassland/Savanna",
                               ecosystem == "Savanna" ~ "Grassland/Savanna",
                               ecosystem == "Salt Marsh & Estuary" ~ "Salt Marsh/Estuary/Wetland",
                               TRUE ~ ecosystem))

str(powell.dm.spp.dat)
unique(powell.dm.spp.dat$data)
length(unique(powell.dm.spp.dat$data))
length(unique(powell.dm.spp.dat$site))

data.test<-powell.dm.spp.dat%>%
  group_by(data, ecosystem)%>%
  summarise(length(unique(site)))%>%
  arrange(ecosystem, data)
print(data.test, n=nrow(data.test))

powell.dm.spp.dat%>%
  group_by(ecosystem)%>%
  summarise(length(unique(site)))

powell.dm.spp.dat%>%
  select(ecosystem, latitude, longitude)%>%
  distinct()%>%
  group_by(ecosystem)%>%
  summarise(n())

range(powell.dm.spp.dat$diameter)

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Fully Harmonized Data CSVs")
#Save Files
write.csv(powell.dm.spp.dat, file="Harmonized.Diameter.Spp.Ind.csv", row.names = F)
############################################################
