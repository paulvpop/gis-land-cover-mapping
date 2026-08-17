## Create input for the force-stratified-sample module of FORCE

# As the pixel count should come from the image that is being supplied to the force-stratified-sample
# module, and not the averages, the pixels_landsat or pixels_sentinel can't be used. So, count the
# pixels for each class in the imagery to be supplied to the module. Choose the raster containing all 
#classes which you feel is the most accurate based on visual inspection or through some other method.

# *IMPORTANT: Checklist of changes needed in the script for your personal use:*
# 1. Under the section "Recode the names", change the abbreviations of classes and the corresponding
#   numbers to match yours.

# Set the working directory:
setwd("D:/RS_GIS_files/Best_selected_models")

# Load the imagery. 
library(terra)

# Make sure it is the same directory:
landsat <- "Landsat_selected.tif"
r <- rast(landsat)

# Using freq() to get the pixel count
freq_table <- freq(r)
print(freq_table)

# Remove the Na value (55537) and the 'layer' column
freq_table <- freq_table %>% filter(value != 55537) %>%
  select(value, count)

# Rename 'value' to 'class'
freq_table <- rename(freq_table, class = value)

# Load the str_sample_landsat.csv if the str_sample_landsat has been removed
str_sample_landsat <- read.csv("sample-size-landsat.csv")

# Recode the names:
str_sample_landsat <- str_sample_landsat %>%
  mutate(class = recode(class,
                        "TF" = "1",
                        "SN" = "2",
                        "OF" = "3",
                        "BA" = "4", 
                        "CC" = "5",
                        "AJ" = "6",
                        "WRC" = "7",
                        "SH" = "8",
                        "BU" = "9",
                        "AGR" = "10",
                        "MGR" = "11",
                        "WA" = "12",
                        "SS"= "13")) %>%
  select(class, compromise)

# Convert class columns in both dataframes to the same type (integer)
freq_table$class <- as.integer(freq_table$class)
str_sample_landsat$class <- as.integer(str_sample_landsat$class)

# Now join
combined_df <- freq_table %>%
  left_join(str_sample_landsat, by = "class")

# Save as csv
write.csv(combined_df, "str_sample_input_landsat.csv", row.names = FALSE, quote = FALSE)
