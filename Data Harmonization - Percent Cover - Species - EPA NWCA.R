############################################################
#Load Packages
library(tidyverse)
library(lubridate)
############################################################
#Automatic CSV Importing

#EPA NWCA - Plot Coordinate Data - 2011
NWCA.plot.11.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/EPA NWCA/nwca11_siteinfo.csv")
str(NWCA.plot.11.dat)

#EPA NWCA - Plot Coordinate Data - 2016
NWCA.plot.16.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/EPA NWCA/nwca-2016-site-information-data_0.csv")
str(NWCA.plot.16.dat)

#EPA NWCA - Plot Coordinate Data - 2021
NWCA.plot.21.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/EPA NWCA/nwca21_siteinfo-data.csv")
str(NWCA.plot.21.dat)


#EPA NWCA - Tree Data - 2011
#nwca2011_tree
NWCA.tree.11.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/EPA NWCA/nwca2011_tree.csv")
str(NWCA.tree.11.dat)

#EPA NWCA - Tree Data - 2016
#nwca_2016_tree_cover_count_-_data_csv
NWCA.tree.16.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/EPA NWCA/nwca_2016_tree_cover_count_-_data_csv.csv")
str(NWCA.tree.16.dat)

#EPA NWCA - Tree Data - 2021
#nwca21_tree_wide-data
NWCA.tree.21.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/EPA NWCA/nwca21_tree_wide-data.csv")
str(NWCA.tree.21.dat)

#EPA NWCA - Plant Cover Data - 2011
#nwca2011_plant_pres_cvr
NWCA.veg.11.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/EPA NWCA/nwca2011_plant_pres_cvr.csv")
str(NWCA.veg.11.dat)

#EPA NWCA - Plant Cover Data - 2016
#nwca-2016-plant-species-cover-height-data
NWCA.veg.16.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/EPA NWCA/nwca-2016-plant-species-cover-height-data.csv")
str(NWCA.veg.16.dat)

#EPA NWCA - Plant Cover Data - 2021
#nwca21_plant_wide-data
NWCA.veg.21.dat<-read.csv("C:/Users/rfidler/Desktop/Powell Ecosystem Stability/Data/Salt Marsh & Estuary/EPA NWCA/nwca21_plant_wide-data.csv")
str(NWCA.veg.21.dat)


############################################################
#Restructuring and Combining Tree Data

#Changing Tree 2011 from Long to Wide
NWCA.tree.11.dat<-NWCA.tree.11.dat%>%
  pivot_wider(names_from = PARAMETER, values_from = RESULT)

#Changing SITE_ID Data in 2021 to Match with Coordinate Formatting
NWCA.tree.21.dat<-NWCA.tree.21.dat%>%
  mutate(SITE_ID = UNIQUE_ID)

#Shifting all Columns to Character for Binding
NWCA.tree.11.dat<-NWCA.tree.11.dat%>%mutate_all(as.character)
NWCA.tree.16.dat<-NWCA.tree.16.dat%>%mutate_all(as.character)
NWCA.tree.21.dat<-NWCA.tree.21.dat%>%mutate_all(as.character)

#Binding Common Columns to Check Compatibility
NWCA.tree.all.dat<-bind_rows(
  NWCA.tree.11.dat,
  NWCA.tree.16.dat,
  NWCA.tree.21.dat
)

str(NWCA.tree.all.dat)

colSums(is.na(NWCA.tree.all.dat))


############################################################
#Restructuring and Combining Vegetation Data

#Shifting all Columns to Character for Binding
NWCA.veg.11.dat<-NWCA.veg.11.dat%>%mutate_all(as.character)
NWCA.veg.16.dat<-NWCA.veg.16.dat%>%mutate_all(as.character)
NWCA.veg.21.dat<-NWCA.veg.21.dat%>%mutate_all(as.character)


#Binding Common Columns to Check Compatibility
NWCA.veg.all.dat<-bind_rows(
  NWCA.veg.11.dat,
  NWCA.veg.16.dat,
  NWCA.veg.21.dat
)

str(NWCA.veg.all.dat)

colSums(is.na(NWCA.veg.all.dat))


############################################################
#Restructuring and Combining Plot Coordinate Data

NWCA.coords<-bind_rows(
  NWCA.plot.11.dat[,c("UNIQUE_ID", "SITE_ID", "PSTL_CODE", "LAT_DD83", "LON_DD83")],
  NWCA.plot.16.dat[,c("UNIQUE_ID", "SITE_ID", "PSTL_CODE", "LAT_DD83", "LON_DD83")],
  NWCA.plot.21.dat[,c("UNIQUE_ID", "SITE_ID", "PSTL_CODE", "LAT_DD83", "LON_DD83")],
)%>%
  distinct()


############################################################
#Creating Matching UNIQUE_ID values to match with Coordinate Data
#Sites are listed slightly differently despite being the same between years


####################
#Tree Data
NWCA.tree.all.dat2<-NWCA.tree.all.dat%>%
  select(SITE_ID, STATE)%>%   
  distinct()%>%
  left_join(NWCA.coords[,c("SITE_ID", "LAT_DD83", "LON_DD83")], by=c("SITE_ID"))%>%
  distinct()%>%
  left_join(NWCA.coords[,c("UNIQUE_ID", "LAT_DD83", "LON_DD83")], by=c("SITE_ID" = "UNIQUE_ID"))%>%
  mutate(Latitude = case_when(is.na(LAT_DD83.x) ~ LAT_DD83.y,
                              TRUE ~ LAT_DD83.x),
         Longitude = case_when(is.na(LON_DD83.x) ~ LON_DD83.y,
                               TRUE ~ LON_DD83.x))%>%
  select(-c(LAT_DD83.x, LAT_DD83.y, LON_DD83.x, LON_DD83.y))%>%
  group_by(SITE_ID)%>%
  mutate(Latitude = mean(Latitude, na.rm = T),
         Longitude = mean(Longitude, na.rm=T))%>%
  distinct()

NWCA.tree.all.dat2<-as.data.frame(NWCA.tree.all.dat2)
str(NWCA.tree.all.dat2)

#Creating New Site Names based on Lat/Long

unique_latlon <- NWCA.tree.all.dat2 %>%
  distinct(Latitude, Longitude) %>%
  mutate(Name = paste0("NWCA_", row_number()))

#Reintroducing Site Names
NWCA.tree.all.dat3<-NWCA.tree.all.dat2%>%
  left_join(unique_latlon, by = c ("Latitude", "Longitude"))%>%
  select(-STATE)%>%
  distinct()

#Adding New Site Names to Raw Data
NWCA.tree.all.dat4<-NWCA.tree.all.dat%>%
  group_by(SITE_ID)%>%
  left_join(NWCA.tree.all.dat3, by = c("SITE_ID"))%>%
  ungroup()

####################
#Vegetation Data
NWCA.veg.all.dat2<-NWCA.veg.all.dat%>%
  select(SITE_ID, STATE)%>%   
  distinct()%>%
  left_join(NWCA.coords[,c("SITE_ID", "LAT_DD83", "LON_DD83")], by=c("SITE_ID"))%>%
  distinct()%>%
  left_join(NWCA.coords[,c("UNIQUE_ID", "LAT_DD83", "LON_DD83")], by=c("SITE_ID" = "UNIQUE_ID"))%>%
  mutate(Latitude = case_when(is.na(LAT_DD83.x) ~ LAT_DD83.y,
                              TRUE ~ LAT_DD83.x),
         Longitude = case_when(is.na(LON_DD83.x) ~ LON_DD83.y,
                               TRUE ~ LON_DD83.x))%>%
  select(-c(LAT_DD83.x, LAT_DD83.y, LON_DD83.x, LON_DD83.y))%>%
  group_by(SITE_ID)%>%
  mutate(Latitude = mean(Latitude, na.rm = T),
         Longitude = mean(Longitude, na.rm=T))%>%
  distinct()

NWCA.veg.all.dat2<-as.data.frame(NWCA.veg.all.dat2)
str(NWCA.veg.all.dat2)

#Creating New Site Names based on Lat/Long

unique_latlon <- NWCA.veg.all.dat2 %>%
  distinct(Latitude, Longitude) %>%
  mutate(Name = paste0("NWCA_", row_number()))

#Reintroducing Site Names
NWCA.veg.all.dat3<-NWCA.veg.all.dat2%>%
  left_join(unique_latlon, by = c ("Latitude", "Longitude"))%>%
  select(-STATE)%>%
  distinct()

#Adding New Site Names to Raw Data
NWCA.veg.all.dat4<-NWCA.veg.all.dat%>%
  group_by(SITE_ID)%>%
  left_join(NWCA.veg.all.dat3, by = c("SITE_ID"))%>%
  ungroup()

###########################################################
#Crosschecking New Site Names between Tree/Veg
f.tree<-NWCA.tree.all.dat4%>%
  select(Name, Latitude, Longitude)%>%
  mutate(X = "Tree")

f.veg<-NWCA.veg.all.dat4%>%
  select(Name, Latitude, Longitude)%>%
  mutate(X = "Veg")

f.cross<-f.tree%>%
  full_join(f.veg, by=c("Name", "Latitude", "Longitude"))%>%
  distinct()

f.cross2<-f.cross%>%
  filter(is.na(X.x) | is.na(X.y))


###########################################################
#Creating Unified Names, lat/long
NWCA.unified.coords<-f.cross%>%
  select(Name, Latitude, Longitude)%>%
  group_by(Name)%>%
  mutate(Latitude = mean(Latitude, na.rm = T),
         Longitude = mean(Longitude, na.rm = T))%>%
  distinct()

############################################################
#Fixing Unstandardized Date Columns

# Function to parse dates and format them consistently
standardize_dates <- function(date_column) {
  # Define possible date formats
  possible_formats <- c("%d-%b-%y", "%m/%d/%Y")
  
  # Parse dates using parse_date_time
  parsed_dates <- parse_date_time(date_column, orders = possible_formats)
  
  # Handle parsing failures by replacing with NA
  parsed_dates[is.na(parsed_dates)] <- NA
  
  # Format all dates to "MM/DD/YYYY"
  formatted_dates <- format(parsed_dates, "%m/%d/%Y")
  
  return(formatted_dates)
}

NWCA.veg.all.dat4$DATE_COL <- standardize_dates(NWCA.veg.all.dat4$DATE_COL)
NWCA.tree.all.dat4$DATE_COL <- standardize_dates(NWCA.tree.all.dat4$DATE_COL)

############################################################
#Formatting Data

#Function to clean species text format:
clean_and_convert <- function(text) {
  # Convert to a valid encoding
  text <- iconv(text, from = "latin1", to = "UTF-8", sub = "")
  # Replace special characters with a space or remove them
  text <- gsub("[^A-Za-z0-9 ]", " ", text)
  # Capitalize only the first letter of the first word
  text <- paste0(toupper(substr(text, 1, 1)), tolower(substr(text, 2, nchar(text))))
  return(text)
}

#EPA NWCA - Tree Data 
# - NWCA.tree.all.dat
NWCA.tree.all.pcdat<-NWCA.tree.all.dat4%>%
  filter(!is.na(TREE_SPECIES))%>%
  select(-c(Latitude, Longitude))%>%
  left_join(NWCA.unified.coords, by = c("Name"))%>%
  mutate(Date = as.Date(DATE_COL, format = "%m/%d/%Y"),
         Year = as.numeric(format(Date, "%Y")),
         Site = Name, 
         Parameter = "Percent Cover",
         Data = "EPA NWCA - Tree Counts and Cover",
         Taxa = "Trees",
         Specificity = "Species",
         Ecosystem = "Salt Marsh & Estuary",
         Latitude = Latitude,
         Longitude =  Longitude,
         std_id = paste0("smew_", "EPA.NWCA_", Site),
         plot = PLOT,
         transect = 999,
         month = as.numeric(format(Date, "%m")),
         day = as.numeric(format(Date, "%d")))%>%
         mutate(spp_full_name = sapply(TREE_SPECIES, clean_and_convert))%>%
         mutate(
         VS = as.numeric(VSMALL_TREE),
         S = as.numeric(SMALL_TREE),
         HM = as.numeric(HMED_TREE),
         LM = as.numeric(LMED_TREE),
         L = as.numeric(TALL_TREE),
         VL = as.numeric(VTALL_TREE))%>%
  mutate(percent_cover = rowSums(select(., VS, S, HM, LM, L, VL)))%>%
  rename("data" = "Data",
         "ecosystem" = "Ecosystem",
         "site" = "Site",
         "latitude" = "Latitude",
         "longitude" = "Longitude",
         "year" = "Year")%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  filter(!is.na(percent_cover) & percent_cover >0)%>%
  mutate(genus = case_when(spp_full_name == "Tree  broadleaf  dicot" |
                             spp_full_name == "Shrub  broadleaf" |
                             spp_full_name == "Shrub    5m " |
                             spp_full_name == "Tree" |
                             spp_full_name == "Shrub" |
                             spp_full_name =="Unknown" ~ "998",
                             TRUE ~ genus),
         species = case_when(spp_full_name == "Tree  broadleaf  dicot" |
                             spp_full_name == "Shrub  broadleaf" |
                             spp_full_name == "Shrub    5m " |
                             spp_full_name == "Tree" |
                             spp_full_name == "Shrub" |
                             spp_full_name =="Unknown" |
                             is.na(species) ~ "998",
                             spp_full_name =="Acer  freemanii" ~ "freemanii",
                             spp_full_name == "Quercus  willdenowiana" ~ "willdenowiana",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'PLOT', aggregation at site/plot/y/m/d")

NWCA.tree.all.pcdat<-as.data.frame(NWCA.tree.all.pcdat)
str(NWCA.tree.all.dat4)
str(NWCA.tree.all.pcdat)
range(NWCA.tree.all.pcdat$percent_cover)

#Species checking
spp.check<-NWCA.tree.all.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-NWCA.tree.all.pcdat%>%
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
write.csv(NWCA.tree.all.pcdat, file="Percent.Cover.Species.SMEW.EPANWCA.Trees.csv", row.names = F)
############################################################

#EPA NWCA - Vegetation Data 
# - NWCA.veg.all.dat
NWCA.veg.all.pcdat<-NWCA.veg.all.dat4%>%
  filter(!is.na(COVER) & COVER >0)%>%
  select(-c(Latitude, Longitude))%>%
  left_join(NWCA.unified.coords, by = c("Name"))%>%
  mutate(Date = as.Date(DATE_COL, format = "%m/%d/%Y"),
         Year = as.numeric(format(Date, "%Y")),
         Site = Name, 
         Parameter = "Percent Cover",
         Data = "EPA NWCA - Vegetation Counts and Cover",
         Taxa = "Vegetation",
         Specificity = "Species",
         Ecosystem = "Salt Marsh & Estuary",
         Latitude = Latitude,
         Longitude =  Longitude,
         spp_full_name = sapply(SPECIES, clean_and_convert),
         percent_cover = as.numeric(COVER),
         std_id = paste0("smew_", "EPA.NWCA_", Site),
         plot = PLOT,
         transect = 999,
         month = as.numeric(format(Date, "%m")),
         day = as.numeric(format(Date, "%d")))%>%
  rename("data" = "Data",
         "ecosystem" = "Ecosystem",
         "site" = "Site",
         "latitude" = "Latitude",
         "longitude" = "Longitude",
         "year" = "Year")%>%
  separate(
    col = spp_full_name,
    into = c("genus", "species"),
    sep = " ", 
    remove = FALSE)%>%
  select(std_id, data, ecosystem, site, plot, transect, latitude, longitude, 
         year, month, day, spp_full_name, genus, species, percent_cover)%>%
  mutate(genus = case_when(spp_full_name == "Tree  broadleaf  dicot" |
                             spp_full_name == "Shrub  broadleaf" |
                             spp_full_name == "Shrub    5m " |
                             spp_full_name == "Tree" |
                             spp_full_name == "Shrub" |
                             spp_full_name == "Graminoid  grass or grasslike " |
                             spp_full_name == "Subshrub  broadleaf" |
                             spp_full_name == "Forb  dicot" |
                             spp_full_name == "Tree  deciduous  broadleaf  dicot" |
                             spp_full_name == "Forb  monocot" |
                             spp_full_name == "Forb  herbaceous  not grass nor grasslike " |
                             spp_full_name == "Shrub  deciduous  broadleaf  dicot" |
                             spp_full_name == "Shrub  deciduous  broadleaf" |
                             spp_full_name == "Forb  monocot  perennial" |
                             spp_full_name == "Vine  woody" |
                             spp_full_name == "Vine  woody  dicot" |
                             spp_full_name == "Forb  introduced" |
                             spp_full_name == "Vine  herbaceous" |
                             spp_full_name == "Vine  herbaceous  dicot" |
                             spp_full_name == "Forb  dicot  perennial" |
                             spp_full_name == "Shrub  broadleaf  dicot" |
                             spp_full_name == "Tree  deciduous  broadleaf" |
                             spp_full_name == "Vine  dicot" |
                             spp_full_name == "Subshrub  deciduous  broadleaf" |
                             spp_full_name == "Forb  dicot  annual" |
                             spp_full_name == "Shrub  broadleaf" |
                             spp_full_name == "Shrub    5m " |
                             spp_full_name == "Tree  broadleaf  dicot" |
                             spp_full_name == "Tree" |
                             spp_full_name == "Subshrub" |
                             spp_full_name == "Shrub" |
                             spp_full_name == "Shrub" |
                             spp_full_name == "Shrub" |
                             spp_full_name == "Shrub" |
                             spp_full_name == "Shrub" |
                             spp_full_name == "Shrub" |
                             spp_full_name == "Shrub" |
                             
                             spp_full_name =="Unknown" ~ "998",
                           TRUE ~ genus),
         species = case_when(spp_full_name == "Tree  broadleaf  dicot" |
                               spp_full_name == "Shrub  broadleaf" |
                               spp_full_name == "Shrub    5m " |
                               spp_full_name == "Tree" |
                               spp_full_name == "Shrub" |
                               spp_full_name == "Graminoid  grass or grasslike " |
                               spp_full_name == "Subshrub  broadleaf" |
                               spp_full_name == "Forb  dicot" |
                               spp_full_name == "Tree  deciduous  broadleaf  dicot" |
                               spp_full_name == "Forb  monocot" |
                               spp_full_name == "Forb  herbaceous  not grass nor grasslike " |
                               spp_full_name == "Shrub  deciduous  broadleaf  dicot" |
                               spp_full_name == "Shrub  deciduous  broadleaf" |
                               spp_full_name == "Forb  monocot  perennial" |
                               spp_full_name == "Vine  woody" |
                               spp_full_name == "Vine  woody  dicot" |
                               spp_full_name == "Forb  introduced" |
                               spp_full_name == "Vine  herbaceous" |
                               spp_full_name == "Vine  herbaceous  dicot" |
                               spp_full_name == "Forb  dicot  perennial" |
                               spp_full_name == "Shrub  broadleaf  dicot" |
                               spp_full_name == "Tree  deciduous  broadleaf" |
                               spp_full_name == "Vine  dicot" |
                               spp_full_name == "Subshrub  deciduous  broadleaf" |
                               spp_full_name == "Forb  dicot  annual" |
                               spp_full_name == "Shrub  broadleaf" |
                               spp_full_name == "Shrub    5m " |
                               spp_full_name == "Tree  broadleaf  dicot" |
                               spp_full_name == "Tree" |
                               spp_full_name == "Subshrub" |
                               spp_full_name == "Carex  section ovales" |
                               spp_full_name == "Carex  section stellulatae" |
                               spp_full_name =="Unknown" |
                               is.na(species) ~ "998",
                             spp_full_name =="Acer  freemanii" ~ "freemanii",
                             spp_full_name == "Quercus  willdenowiana" ~ "willdenowiana",
                             spp_full_name == "Delphinium  occidentale" ~ "occidentale",
                             spp_full_name == "Delphinium  burkei" ~ "burkei",
                             spp_full_name == "Dryopteris  boottii" ~ "boottii",
                             spp_full_name == "Elymus  pseudorepens" ~ "pseudorepens",
                             spp_full_name == "Lonicera  bella" ~ "bella",
                             spp_full_name == "Oclemena  blakei" ~ "blakei",
                             spp_full_name == "Typha  glauca" ~ "glauca",
                             spp_full_name == "Viola  primulifolia" ~ "primulifolia",
                             TRUE ~ species))%>%
  mutate(notes = "plot marks 'PLOT', aggregation at site/plot/y/m/d")

str(NWCA.veg.all.dat4)  
str(NWCA.veg.all.pcdat)


#Species checking
spp.check<-NWCA.veg.all.pcdat%>%
  select(spp_full_name, genus, species)%>%
  distinct()


#Backcheck for percent cover aggregation levels
check1<-NWCA.veg.all.pcdat%>%
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
write.csv(NWCA.veg.all.pcdat, file="Percent.Cover.Species.SMEW.EPANWCA.Veg.csv", row.names = F)
############################################################



#Final site Crosscheck
c1<-NWCA.tree.all.pcdat%>%
  filter(site == "NWCA_2975")%>%
  select(site, latitude, longitude)%>%
  distinct()

c2<-NWCA.veg.all.pcdat%>%
  filter(site == "NWCA_2975")%>%
  select(site, latitude, longitude)%>%
  distinct()

c3<-bind_rows(c1, c2)

