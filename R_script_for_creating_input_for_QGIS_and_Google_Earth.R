# Create input for QGIS and Google Earth

# The output from the force-stratified-random module produces a gpkg output (unless you changed it). 
# Load it into the R environment and recode the numbers back to abbreviations to make it easier to 
# read in QGIS and Google Earth and change the labels if necessary.

# NOTE THAT THE EXAMPLE OUTPUTS OF THE CODE CAN BE FOUND IN THE ONLINE DOCUMENTATION OF
# THIS SCRIPT: https://github.com/paulvpop/gis-land-cover-mapping/blob/main/12.%20Accuracy%20assessment.md#create-input-for-qgis-and-google-earth

# Set working directory:
setwd("D:/RS_GIS_files/Best_selected_models")

# Load necessary library
library(sf)
library(dplyr)

# Read in the stratified random sample output from the previous step
sample_gpkg <- st_read("sample.gpkg")

# Check the structure and first few rows if you want
str(sample_gpkg)
head(sample_gpkg)

# The class labels are in the 'label_map' column

# Recode the names:
sample_gpkg <- sample_gpkg %>%
  mutate(label_map = recode(label_map,
                            "1" ="TF",
                            "2" = "SN",
                            "3" = "OF",
                            "4" = "BA", 
                            "5" = "CC",
                            "6" = "AJ",
                            "7" = "WRC",
                            "8" = "SH",
                            "9" = "BU",
                            "10" = "AGR",
                            "11" = "MGR",
                            "12" = "WA",
                            "13" = "SS")) %>%
  select(-label_reference)

# Write the file as gpkg for viewing in QGIS
st_write(sample_gpkg, "sample_recoded.gpkg")

# Prepare the data for KML export for viewing in Google Earth (Pro)
# The 'Name' field appears in Google Earth's Places panel sidebar.
# The 'Description' field populates the clickable properties balloon.

# Create FID
sample_gpkg <- sample_gpkg %>%
  mutate(fid = row_number())

# Make the fields appear in GEE:
sample_gpkg <- sample_gpkg %>%
  mutate(Name = label_map,# Shows in the sidebar list
         Description = paste("fid:", fid)) 

# Write to a KML file
st_write(sample_gpkg, "sample_recoded.kml") 
