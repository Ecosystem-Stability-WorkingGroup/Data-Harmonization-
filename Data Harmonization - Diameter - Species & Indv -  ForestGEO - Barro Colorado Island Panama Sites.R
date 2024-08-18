############################################################
#Load Packages
library(tidyverse)
library(zip)
library(fs)
library(readxl)

############################################################
#Automatic CSV Importing
#Pulls .txt from within .zip folders within outer folder
#Combines .txt files into single dataframe based on name of folder
#Adds site name based on name of folder

main_dir <- "C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - Latin America - Barro Colorado Island, Panama"
sub_dirs <- dir_ls(main_dir, type = "directory")


process_subdir <- function(sub_dir) {
  zip_files <- dir_ls(sub_dir, regexp = "PlotDataReport.*\\.zip$")
  df_list <- list()
  
  for (zip_file in zip_files) {
    temp_dir <- file.path(tempdir(), tools::file_path_sans_ext(basename(zip_file)))
    dir.create(temp_dir)
    
    unzip(zip_file, exdir = temp_dir)
    
    txt_files <- dir_ls(temp_dir, regexp = "PlotDataReport.*\\.txt$")
    
    if (length(txt_files) != 1) {
      stop("There should be exactly one text file in each zip file.")
    }
    
    df <- read_delim(txt_files[1], delim = "\t", col_types = cols(.default = "c"))
    
    df_list <- append(df_list, list(df))
  }
  
  combined_df <- bind_rows(df_list)
  return(combined_df)
}

all_data <- list()
for (sub_dir in sub_dirs) {
  sub_dir_name <- basename(sub_dir)
  df <- process_subdir(sub_dir)
  df$Site <- sub_dir_name  # Add the Site column
  all_data <- append(all_data, list(df))
  assign(sub_dir_name, df)
}

# Combine all dataframes into a single dataframe
ForestGeo.Panama.dat <- bind_rows(all_data)


#Write CSV for Ease Later
#write.csv(ForestGeo.Panama.dat, file = "ForestGeo.Combined.Panama.Data.csv", row.names = F)

#Coordinate Data
ForestGeo.Panama.coords<-read_excel("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Forests/ForestGeo/Tree - Latin America - Barro Colorado Island, Panama/panamatreesite.xlsx")


############################################################
#Crosschecking Coordinate Data
ForestGeo.Panama.coords.cross<-ForestGeo.Panama.dat%>%
  select(Site, Status)%>%
  distinct()%>%
  left_join(ForestGeo.Panama.coords[,c("Plot", "Latitude", "Longitude")], 
            by = c("Site" = "Plot"))%>%
  select(-Status)%>%
  distinct()

#Updating Sites in Coords to Match Where Possible
ForestGeo.Panama.coords2<-ForestGeo.Panama.coords%>%
  select(Plot, Latitude, Longitude)%>%
  mutate(Site = case_when(Plot == "Finca_Roubik" ~ "FincaRoubik" ,
                          Plot == "Plot 31" ~ "Plot31",
                          Plot == "Plot 32" ~ "Plot32",
                          TRUE ~ Plot))%>%
  select(-Plot)

#Second Crosscheck
ForestGeo.Panama.coords.cross2<-ForestGeo.Panama.dat%>%
  select(Site, Status)%>%
  distinct()%>%
  left_join(ForestGeo.Panama.coords2, 
            by = c("Site"))%>%
  select(-Status)%>%
  distinct()

#Two sites: PanamaPacifico and Cerro_Pelado_CIHH_UTP do not have coordinate info

############################################################
#Formatting Data


#ForestGeo - Tree - Latin America - Barro Colorado Island, Panama
# - ForestGeo.Combined.Panama.Data.csv
ForestGeo.Panama.DIAdat<-ForestGeo.Panama.dat%>%
  left_join(ForestGeo.Panama.coords2, by = c("Site"))%>%
  filter(Latitude != "NULL")%>%
  filter(Status == "alive")%>%
  mutate(site = Site, 
         Parameter = "DBH, Mortality",
         data = "ForestGEO - Tree - Panama",
         Taxa = "Trees",
         Specificity = "Individuals",
         ecosystem = "Forests",
         std_id = paste0("forest_", "Forest.GEO_", site),
         plot = Quadrat,
         transect = 999,
         date = lubridate::parse_date_time(Date, orders = c("mdy", "dmy", "ymd")),
         year = year(date),
         month = month(date),
         month = ifelse(is.na(month), 999, month),
         day = day(date),
         day = ifelse(is.na(day), 999, day),
         spp_full_name = Latin,
         tree_id = as.character(TreeID),
         stem_id = as.character(StemID),
         diameter = as.numeric(DBH),
         diam_units = "unclear",
         latitude = as.numeric(Latitude),
         longitude =  as.numeric(Longitude))%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  mutate(genus = case_when(spp_full_name == "Unidentified (sherman)" |
                             spp_full_name == "Unidentified arecaceae-soberania" |
                             spp_full_name == "Unidentified myrtaceae hoja mediana-fincaroubik" |
                             spp_full_name == "Unidentified myrtaceae hoja pequena-fincaroubik" |
                             spp_full_name == "Unidentified myrtaceae hoja sesil-fincaroubik" |
                             spp_full_name == "Unidentified rubiaceae-fincaroubik" |
                             genus == "Unidentified" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Unidentified (sherman)" |
                               spp_full_name == "Unidentified arecaceae-soberania" |
                               spp_full_name == "Unidentified myrtaceae hoja mediana-fincaroubik" |
                               spp_full_name == "Unidentified myrtaceae hoja pequena-fincaroubik" |
                               spp_full_name == "Unidentified myrtaceae hoja sesil-fincaroubik" |
                               spp_full_name == "Unidentified rubiaceae-fincaroubik" |
                               species == "sp.1" | 
                               species == "sp.1_(hojas_chicas)" | 
                               species == "sp.10" | 
                               species == "sp.11" | 
                               species == "sp.13" | 
                               species == "sp.14" | 
                               species == "sp.16" | 
                               species == "sp.17" | 
                               species == "sp.18" | 
                               species == "sp.19" | 
                               species == "sp.2" | 
                               species == "sp.20" | 
                               species == "sp.21" | 
                               species == "sp.3" | 
                               species == "sp.4" | 
                               species == "sp.4_(tiny_leaf)" | 
                               species == "sp.5" | 
                               species == "sp.6" | 
                               species == "sp.7" | 
                               species == "sp.8" | 
                               species == "sp.9" ~ "998",
                             TRUE ~ species))%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, tree_id, stem_id, diameter, diam_units)%>%
  filter(!is.na(diameter)& diameter >0)%>%    
  mutate(notes = "plot marks 'Quadrat'; live trees only; unclear if DBH is mm or cm")



str(ForestGeo.Panama.dat)
str(ForestGeo.Panama.DIAdat)

#Species checking
spp.check<-ForestGeo.Panama.DIAdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()

############################################################
#Save File Appropriate Folders

#Set Working Directory
setwd("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/All Sites - Data Harmonization/Individual Formatted Data - Diameter - Species & Individuals")
#Save Files
write.csv(ForestGeo.Panama.DIAdat, file="Diameter.Species.Indv.Forest.ForestGeo.Panama.BCI.csv", row.names = F)
############################################################
