# This is an optimised and minimalist workflow for post processing model prediction outputs (workflow 2 in section 11. Post processing)

#Set the working directory:
#ctrl+Shift+H or setwd("D:/GIS/LULC-SIANG/classified_imagery")

#Uncomment (remove #) the following line and Install the packages 'raster' and 
#'sp' if not already installed. Note that you may have to install or update some
#dependencies if the installation is not working. Check the errors/warnings. 
#install.packages(c("raster","sf"))

#Load the 'raster' and 'sf' package:
library(raster)
library(sf)

#SKIP THE NEXT THREE STEP STEPS IF YOU ARE STARTING WITH A KML FILE

# Add the  kmz file containing the training data to the "classified_imagery" folder
#list the kmz files in the folder (should be just the one file with the training data)
kmz <- list.files(pattern="*.kmz", full.names=FALSE)

# Create temporary directory
temp_dir <- tempdir()

# Unzip KMZ (this will extract the KML file inside
unzip(kmz, exdir = temp_dir)

# Find the extracted KML file (or just the kml file if you're starting directly from kml)
kml_file <- list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE)

# Read the KML file with sf
data <- st_read(kml_file)

#Output in Windows: 
# Reading layer `2024_updated' from data source 
#   `C:\Users\GIS\AppData\Local\Temp\RtmpQPSSTT\doc.kml' using driver `KML'
# Simple feature collection with 2358 features and 2 fields
# Geometry type: POINT
# Dimension:     XYZ
# Bounding box:  xmin: 93.99069 ymin: 27.56451 xmax: 95.56589 ymax: 29.33588
# z_range:       zmin: 0 zmax: 0
# Geodetic CRS:  WGS 84

# Extract all raster values at once using terra 

#Load the 'terra' package. We will use sf and dplyr too
library(terra)
library(dplyr)

# Get coordinates and convert to terra vect
coords_vect <- vect(data)

#Change working directory to where the DEM to be used is located (or else, move 
#the DEM to the current working directory): setwd("D:/GIS/LULC-SIANG/DEM")

# Load DEM using terra directly (skip raster package)
dem <- rast("alos.vrt")

# Extract elevation
elev_vals <- terra::extract(dem, coords_vect)[, 2]  # Second column has values

#Ignore these warnings:
# Warning messages:
#   1: In class(object) <- "environment" :
#   Setting class(x) to "environment" sets attribute to NULL; result will no longer be an S4 object
# 2: In class(object) <- "environment" :
#   Setting class(x) to "environment" sets attribute to NULL; result will no longer be an S4 object
# 3: In class(object) <- "environment" :
#   Setting class(x) to "environment" sets attribute to NULL; result will no longer be an S4 object
# 4: In class(object) <- "environment" :
#   Setting class(x) to "environment" sets attribute to NULL; result will no longer be an S4 object
# 5: In class(object) <- "environment" :
#   Setting class(x) to "environment" sets attribute to NULL; result will no longer be an S4 object

# Create result dataframe
result_df <- data.frame(
  Name = data$Name,
  Longitude = st_coordinates(data)[, 1],
  Latitude = st_coordinates(data)[, 2],
  Elevation = elev_vals
)

# Calculate and extract all terrain indices in one go
terrain_params <- c('aspect', 'slope', 'TPI', 'TRI', 'roughness', 'flowdir')

# Create and extract all values in single pass (took around 10-20 s)
all_terrain_vals <- do.call(cbind, lapply(terrain_params, function(param) {
  cat("Processing", param, "...\n")
  #Create the DEM objects like slope, aspect, roughness etc
  terrain_raster <- terrain(dem, v = param, 
                            unit = ifelse(param %in% c('aspect', 'slope'), 'degrees', 'radians'),
                            neighbors = 8)
  #Neighbours = 8 best for rough surfaces Horn (1981) like Siang 
  #Extract the values at each of the training data coordinates
  terra::extract(terrain_raster, coords_vect)[, 2]
}))

#Rename the column names of 'all_terrain_vals'
colnames(all_terrain_vals) <- c("Aspect", "Slope", "TPI", "TRI", "Roughness", "Flowdir")
#Add the slope, aspect, TPI, TRI, roughness and flowdir information for the corresponding 
#coordinates in result_df
result_df <- bind_cols(result_df, all_terrain_vals)

# Split the dataframe by the 'Name' column to get the different classes (to be used
#in a later step):
split_df_list <- result_df %>% group_split(Name)

# Mask all rasters at once
shp_vect <- vect(read_sf("study_area/study_area.shp"))

# Check CRS and project shapefile to match DEM CRS 
if (!same.crs(dem, shp_vect)) {
  cat("CRS mismatch detected. Projecting shapefile to match DEM CRS...\n")
  cat("DEM CRS:", crs(dem, describe = TRUE)$name, "\n")
  cat("Shapefile CRS:", crs(shp_vect, describe = TRUE)$name, "\n")
  
  shp_vect_projected <- project(shp_vect, dem)
  cat("Shapefile reprojected to match DEM CRS\n")
} else {
  cat("CRS match confirmed - no reprojection needed\n")
  shp_vect_projected <- shp_vect
}

# Now mask all rasters (after extracting the relevant feature like aspect if necessary (not necessary for elevation)) using the already-projected shapefile (takes around a minute)
cat("Masking rasters...\n")
#DEM is equivalent to elevation
elev <- mask(dem, shp_vect_projected)
# The terrain functions calculate things like aspect, slope etc before masking
asp <- mask(terrain(dem, v = 'aspect', unit = 'degrees', neighbors = 8), shp_vect_projected)
slp <- mask(terrain(dem, v = 'slope', unit = 'degrees', neighbors = 8), shp_vect_projected)
cTPI <- mask(terrain(dem, v = 'TPI'), shp_vect_projected)
cTRI <- mask(terrain(dem, v = 'TRI'), shp_vect_projected)
rough <- mask(terrain(dem, v = 'roughness'), shp_vect_projected)
fldr <- mask(terrain(dem, v = 'flowdir'), shp_vect_projected)

# Cleanup
rm(dem, all_terrain_vals, elev_vals, coords_vect, shp_vect)
gc()

#Find the difference between maximum and minimum between different classes and
#combined histograms (for the whole data and not just tne points where the training
#data lies). These global_stats can be extracted through the following:

global_stats <- as.data.frame(cbind(
  Elevation_max = elev@pntr[["range_max"]],
  Elevation_min = elev@pntr[["range_min"]],
  Aspect_max = asp@pntr[["range_max"]],
  Aspect_min = asp@pntr[["range_min"]],
  Slope_max = slp@pntr[["range_max"]],
  Slope_min = slp@pntr[["range_min"]],
  Roughness_max = rough@pntr[["range_max"]],
  Roughness_min = rough@pntr[["range_min"]],
  Flowdir_max = fldr@pntr[["range_max"]],
  Flowdir_min = fldr@pntr[["range_min"]],
  TPI_max = cTPI@pntr[["range_max"]],
  TPI_min = cTPI@pntr[["range_min"]],
  TRI_max = cTRI@pntr[["range_max"]],
  TRI_min = cTRI@pntr[["range_min"]]
))

#Note that the word 'pntr' may need to be changed to 'cpp' if using an earlier
#version of the terra package(?) (the above operation will throw this error otherwise:
# Error in h(simpleError(msg, call)) : error in evaluating the argument 'x' in 
#selecting a method for function 'as.data.frame': no slot of name "cpp" for this 
#object of class "SpatRaster"
#You can check by looking at one of the above objects. For example, check under 
#which main heading "range_max" and "range_min" of the object 'elev' is under:
View(elev)

#Load required package 'purrr' (in addition to 'dplyr')
library(purrr)
# Ignore warning:
# Warning message:
#   In class(object) <- "environment" :
#   Setting class(x) to "environment" sets attribute to NULL; result will no longer be an S4 object

# Define variables of interest
vars <- c("Elevation", "Aspect", "Slope", "Roughness", "Flowdir", "TPI", "TRI")

#Create the following function:
compare_stats <- function(class_df, global_stats) {
  class_stats <- class_df %>%
    summarise(across(all_of(vars),
                     list(max = ~max(., na.rm = TRUE),
                          min = ~min(., na.rm = TRUE))))
  
  # Create a tidy long-format result
  tibble(
    Class = unique(class_df$Name)[1],
    map_dfr(vars, ~ {
      tibble(
        Variable = .x,
        max_diff = global_stats[[paste0(.x, "_max")]] - class_stats[[paste0(.x, "_max")]],
        min_diff = global_stats[[paste0(.x, "_min")]] - class_stats[[paste0(.x, "_min")]],
        range = global_stats[[paste0(.x, "_max")]] - global_stats[[paste0(.x, "_min")]],
        global_max = global_stats[[paste0(.x, "_max")]], 
        global_min = global_stats[[paste0(.x, "_min")]]
      )
    })
  )
}

#The following code snippet uses map_dfr() to apply compare_stats() to each 
#element of split_df_list (with global_stats as additional argument)
#The results are row-bound into a data frame (Un-nesting the results is not 
#necessary since the columns are not nested)
diff_results <- map_dfr(split_df_list, compare_stats, global_stats) 

#This is in itself is difficult to interpret. So, express the differences in
#terms of the proportion of the range of each class:

#Add the percentage of proportions as two more columns in the 'diff_results' object
diff_results <- diff_results %>% mutate(max_diff_perc = (max_diff/range)*100,
                                        min_diff_perc = (min_diff/range)*100)

#Find the significant differences (with >15% and <15% taken as the threshold after 
#some trial and error testing) and add them as two more columns:
diff_results <- diff_results %>% 
  mutate(sig_max_diff =
           case_when(max_diff_perc > 15 ~ "sig",
                     max_diff_perc < 15 ~ "insig"),
         sig_min_diff =
           case_when(min_diff_perc < -15 ~ "sig",
                     min_diff_perc > -15 ~ "insig"))

#Find the maximum and minimum values for each class:
diff_results <- diff_results %>% 
  mutate(class_max =
           case_when(sig_max_diff == "sig" & Variable == "Elevation" 
                     ~ global_stats$Elevation_max - max_diff,
                     sig_max_diff == "insig" & Variable == "Elevation" 
                     ~ global_stats$Elevation_max,
                     sig_max_diff == "sig" & Variable == "Aspect" 
                     ~ global_stats$Aspect_max - max_diff,
                     sig_max_diff == "insig" & Variable == "Aspect" 
                     ~ global_stats$Aspect_max,
                     sig_max_diff == "sig" & Variable == "Slope" 
                     ~ global_stats$Slope_max - max_diff,
                     sig_max_diff == "insig" & Variable == "Slope" 
                     ~ global_stats$Slope_max,
                     sig_max_diff == "sig" & Variable == "Roughness" 
                     ~ global_stats$Roughness_max - max_diff,
                     sig_max_diff == "insig" & Variable == "Roughness" 
                     ~ global_stats$Roughness_max,
                     sig_max_diff == "sig" & Variable == "Flowdir" 
                     ~ global_stats$Flowdir_max - max_diff,
                     sig_max_diff == "insig" & Variable == "Flowdir" 
                     ~ global_stats$Flowdir_max,
                     sig_max_diff == "sig" & Variable == "TPI" 
                     ~ global_stats$TPI_max - max_diff,
                     sig_max_diff == "insig" & Variable == "TPI" 
                     ~ global_stats$TPI_max,
                     sig_max_diff == "sig" & Variable == "TRI" 
                     ~ global_stats$TRI_max - max_diff,
                     sig_max_diff == "insig" & Variable == "TRI" 
                     ~ global_stats$TRI_max),
         
         
         class_min =
           case_when(sig_min_diff == "sig" & Variable == "Elevation" 
                     ~ global_stats$Elevation_min - min_diff,
                     sig_min_diff == "insig" & Variable == "Elevation" 
                     ~ global_stats$Elevation_min,
                     sig_min_diff == "sig" & Variable == "Aspect" 
                     ~ global_stats$Aspect_min - min_diff,
                     sig_min_diff == "insig" & Variable == "Aspect" 
                     ~ global_stats$Aspect_min,
                     sig_min_diff == "sig" & Variable == "Slope" 
                     ~ global_stats$Slope_min - min_diff,
                     sig_min_diff == "insig" & Variable == "Slope" 
                     ~ global_stats$Slope_min,
                     sig_min_diff == "sig" & Variable == "Roughness" 
                     ~ global_stats$Roughness_min - min_diff,
                     sig_min_diff == "insig" & Variable == "Roughness" 
                     ~ global_stats$Roughness_min,
                     sig_min_diff == "sig" & Variable == "Flowdir" 
                     ~ global_stats$Flowdir_min - min_diff,
                     sig_min_diff == "insig" & Variable == "Flowdir" 
                     ~ global_stats$Flowdir_min,
                     sig_min_diff == "sig" & Variable == "TPI" 
                     ~ global_stats$TPI_min - min_diff,
                     sig_min_diff == "insig" & Variable == "TPI" 
                     ~ global_stats$TPI_min,
                     sig_min_diff == "sig" & Variable == "TRI" 
                     ~ global_stats$TRI_min - min_diff,
                     sig_min_diff == "insig" & Variable == "TRI" 
                     ~ global_stats$TRI_min))

#Save as csv
write.csv(diff_results,"diff_results.csv")  

#Select, mask and intersect the variables for each class (using the 'terra', 
#'dplyr' and 'purrr' packages). A function is created for the same:
process_class_rasters <- function(class_name, diff_data, rasters) {
  # Filter data for the current class
  class_data <- diff_data %>% filter(Class == class_name)
  
  # Initialize list to store processed rasters
  processed_rasters <- list()
  
  # Process each variable for the class
  for (i in 1:nrow(class_data)) {
    row <- class_data[i, ]
    
    # Skip if both sig flags are insignificant
    if (row$sig_max_diff == "insig" && row$sig_min_diff == "insig") next
    
    # Get the corresponding raster
    raster_name <- case_when(
      row$Variable == "Elevation" ~ "elev",
      row$Variable == "Aspect" ~ "asp",
      row$Variable == "Slope" ~ "slp",
      row$Variable == "Roughness" ~ "rough",
      row$Variable == "Flowdir" ~ "fldr",
      row$Variable == "TPI" ~ "cTPI",
      row$Variable == "TRI" ~ "cTRI",
      TRUE ~ NA_character_
    )
    
    if (is.na(raster_name)) next
    
    # Get the raster
    r <- rasters[[raster_name]]
    
    # Create mask based on significance flags
    if (row$sig_max_diff == "sig" && row$sig_min_diff == "sig") {
      # Both significant - mask outside class range
      masked_r <- clamp(r, lower = row$class_min, upper = row$class_max, values = FALSE)
    } else if (row$sig_max_diff == "sig") {
      # Only max significant - mask above class_max
      masked_r <- clamp(r, upper = row$class_max, values = FALSE)
    } else if (row$sig_min_diff == "sig") {
      # Only min significant - mask below class_min
      masked_r <- clamp(r, lower = row$class_min, values = FALSE)
    } else {
      # Neither significant (shouldn't happen due to earlier check)
      next
    }
    
    # Add to processed list
    processed_rasters[[raster_name]] <- masked_r
  }
  
  # If no rasters were selected, return NULL
  if (length(processed_rasters) == 0) return(NULL)
  
  # Intersect all processed rasters
  # First convert list to SpatRaster with multiple layers
  raster_stack <- rast(processed_rasters)
  
  # Create intersection (values will be NA where any layer is NA)
  intersected_raster <- app(raster_stack, fun = function(x) if (all(!is.na(x))) 1 else NA)
  
  # Set the name
  names(intersected_raster) <- class_name
  
  return(intersected_raster)
}

# The rasters should be in a named list like this:
rasters_list <- list(
  elev = elev,
  asp = asp,
  slp = slp,
  rough = rough,
  fldr = fldr,
  cTPI = cTPI,
  cTRI = cTRI
)

# Get unique classes
unique_classes <- unique(diff_results$Class)

# Process all classes (it took around 35 minutes)
result_rasters <- map(setNames(unique_classes, unique_classes), 
                      ~process_class_rasters(.x, diff_results, rasters_list))

# Remove NULL entries (classes with no significant variables)
result_rasters <- compact(result_rasters)

#The next step is the beginning of bulk processing of all the models in the device. 

#First, set the working directory. This directory should contain all the folders containing the models. For safety, you can keep it as the highest directory possible - home directory or D drive (if all the folders are in D drive):
setwd("D:/GIS")

#The function that does post-processing at once requires a csv file that contains several model details including the name of the model and and the directory path of model. It will look like the following:

#   sl_no satellite             model_name BA_as_TF SH_as_TF BA.SH_as_TF  directory
#1      1  Sentinel  dir_sent_RFC_seven_ml        1        0           0  D:\\GIS\\force
#2      2  Sentinel  dir_sent_RFC_eight_ml        1        0           0  D:\\GIS\\force
#3      3  Sentinel   dir_sent_RFC_nine_ml        0        0           1  D:\\GIS\\force
#4      4  Landsat     dir_land_RFC_ten_ml        0        0           1  D:\\GIS\\force2
#5      5  Landsat  dir_land_RFC_eleven_ml        0        0           0  D:\\GIS\\force2
#6      6  Sentinel    dir_ml_sent_RFC_pbc        0        0           0 D:\\GIS\\force3
#7      7  Sentinel    dir_ml_sent_RFC_one        0        0           0 D:\\GIS\\force3
#8      8  Sentinel    dir_ml_sent_SVC_one        0        0           0 D:\\GIS\\force3
#9      9  Landsat    dir_ml_land_RFC_four        0        1           0 D:\\GIS\\force2
#10    10  Sentinel    dir_ml_sent_SVC_two        0        1           0 D:\\GIS\\force3
#11    11  Sentinel   dir_ml_sent_RFC_five        0        1           0 D:\\GIS\\force3
#12    12  Sentinel  dir_ml_sent_SVC_three        0        1           0 D:\\GIS\\force3
#13    13  Sentinel    dir_ml_sent_RFC_six        0        1           0 /home/GIS/force2
#14    14  Sentinel   dir_ml_sent_SVC_four        0        1           0 /home/GIS/force2
#15    15  Sentinel   dir_ml_sent_RFC_nine        0        1           0 /home/GIS/force2

# Load the csv containing model details
mod_det <- read.csv("force/model_details5.csv")

library(terra)
library(stringr)
library(sf)

# Pre-compute ALL shared resources (class_mapping and reprojection for the shapefile to match the MLP files) ONCE before bulk automated processing
cat("Pre-computing shared resources...\n")

# Pre-compute class mapping (constant)
class_mapping <- c(
  "MODEL_CLASS_001" = "TF",
  "MODEL_CLASS_002" = "SN", 
  "MODEL_CLASS_003" = "OF",
  "MODEL_CLASS_004" = "BA",
  "MODEL_CLASS_005" = "CC",
  "MODEL_CLASS_006" = "AJ",
  "MODEL_CLASS_007" = "WRC",
  "MODEL_CLASS_008" = "SH",
  "MODEL_CLASS_009" = "BU",
  "MODEL_CLASS_010" = "AGR",
  "MODEL_CLASS_011" = "MGR",
  "MODEL_CLASS_012" = "WA",
  "MODEL_CLASS_013" = "SS"
)

#The already loaded "shp" will be used in the following loop. Pre-reproject the 
#shapefile ONCE for all models (since all MLPS which will be converted to VRTs will
#have the same CRS - EPSG:7755)
cat("Reprojecting shapefile to EPSG:7755 for all models...\n")
target_crs <- "EPSG:7755"
shp_reprojected <- st_transform(shp, target_crs)
shp_vect <- vect(shp_reprojected)  # Convert to SpatVector for faster masking

cat("Shapefile reprojection completed\n")

# Modified function with better path handling
process_with_vrt <- function(model_name) {
  
  # Find the model info
  model_info <- mod_det[mod_det$model_name == model_name, ]
  
  if (nrow(model_info) == 0) {
    stop(paste("Model", model_name, "not found in mod_det"))
  }
  
  # Use the directory path directly from mod_det
  dir_path <- paste0(model_info$directory,"/",model_name)
  # Clean path separators if needed
  dir_path <- gsub("\\\\", "/", dir_path)
  
  cat("Processing model:", model_name, "\n")
  cat("Directory:", dir_path, "\n")
  
  # Check if directory exists
  if (!dir.exists(dir_path)) {
    warning("Directory does not exist: ", dir_path)
    return(FALSE)
  }
  
  # Create VRT - search in the specific model directory
  mlp_files <- list.files(dir_path, pattern = "PREDICTION_HL_ML_MLP\\.tif$", 
                          recursive = TRUE, full.names = TRUE)
  
  print(paste("Found", length(mlp_files), "PREDICTION_HL_ML_MLP.tif files"))
  
  if (length(mlp_files) == 0) {
    warning("No MLP files found for model: ", model_name, " in ", dir_path)
    return(FALSE)
  }
  
  # Read the first file to get original band names
  first_raster <- rast(mlp_files[1])
  original_band_names <- names(first_raster)
  
  print("Original band names:")
  print(original_band_names)
  
  # Create the virtual raster tile
  cat("Creating VRT from", length(mlp_files), "MLP files...\n")
  vrt_file <- file.path(dir_path, "merged_mosaic.vrt")
  mosaic_vrt <- vrt(mlp_files, vrt_file, overwrite = TRUE)
  
  # Apply original band names to the VRT
  names(mosaic_vrt) <- original_band_names
  
  print("VRT band names after renaming:")
  print(names(mosaic_vrt))
  
  # Vectorized renaming
  current_names <- names(mosaic_vrt)
  new_names <- ifelse(current_names %in% names(class_mapping),
                      class_mapping[current_names],
                      current_names)
  
  names(mosaic_vrt) <- new_names
  
  print("VRT band names after class mapping:")
  print(names(mosaic_vrt))
  
  # Setup output directory - create it within the model's directory
  output_dir <- file.path(dir_path, "processed")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created directory:", output_dir, "\n")
  } else {
    cat("Directory exists:", output_dir, "\n")
  }
  
  mask_values <- c(NA, 0)
  threads <- 14
  
  # Process each band from the VRT directly
  for (class_name in names(mosaic_vrt)) {
    cat("Processing class:", class_name, "...\n")
    
    tryCatch({
      # Get the input raster from VRT
      r <- mosaic_vrt[[class_name]]
      
      # Check if we have a corresponding result_raster for this class
      if (!class_name %in% names(result_rasters)) {
        cat("Skipping", class_name, "- no corresponding result_raster found\n")
        next
      }
      
      # Get the corresponding mask from result_rasters
      mask_band <- result_rasters[[class_name]]
      
      # Reproject mask to match VRT band's CRS
      mask_reproj <- project(mask_band, crs(r), method = "near", threads = threads)
      
      # Resample mask to match VRT band's resolution
      mask_resam <- resample(mask_reproj, r, method = "near", threads = threads)
      
      # Create a mask where mask_band has valid values
      valid_mask <- !is.na(mask_resam) & mask_resam != 0
      
      # Apply the mask to the VRT band
      r_masked <- r * valid_mask
      
      # Replace NAs with 0
      r_masked[is.na(r_masked)] <- 0
      
      # Clip to shapefile
      r_cropped <- mask(r_masked, shp_vect)
      
      # Check non-zero values
      non_zero_count <- global(!is.na(r_cropped) & r_cropped > 0, "sum", na.rm = TRUE)[1,1]
      cat("Non-zero pixels in", class_name, ":", non_zero_count, "\n")
      
      # Save output
      output_file <- file.path(output_dir, paste0(class_name, "_processed.tif"))
      writeRaster(
        r_cropped,
        filename = output_file,
        datatype = "INT2S",
        NAflag = -9999,
        gdal = c("COMPRESS=DEFLATE", "TFW=YES"),
        overwrite = TRUE
      )
      
      cat("Successfully processed", class_name, "\n")
      
      # Clean up
      rm(r, mask_band, mask_reproj, mask_resam, r_cropped)
      gc()
      
    }, error = function(e) {
      cat("Error processing", class_name, ":", e$message, "\n")
    })
  }
  
  return(TRUE)
}

# Run for all models
for (i in 1:nrow(mod_det)) {
  model_name <- mod_det$model_name[i]
  cat("\n", rep("=", 50), "\n", sep = "")
  cat("Processing model", i, "of", nrow(mod_det), ":", model_name, "\n")
  cat(rep("=", 50), "\n\n", sep = "")
  
  tryCatch({
    success <- process_with_vrt(model_name)
    if (success) {
      cat("✓ Successfully completed", model_name, "\n")
    } else {
      cat("⚠ Completed with warnings:", model_name, "\n")
    }
  }, error = function(e) {
    cat("✗ Failed to process", model_name, ":", e$message, "\n")
  })
}


#Output:
#==================================================
#Processing model 1 of 5 : dir_sent_RFC_eight_ml 
#==================================================

#Processing model: dir_sent_RFC_eight_ml 
#Directory: D:/Paul_Pop_RS_GIS_files/force/dir_sent_RFC_eight_ml 
#[1] "Found 33 PREDICTION_HL_ML_MLP.tif files"
#[1] "Original band names:"
# [1] "MODEL_CLASS_001" "MODEL_CLASS_002" "MODEL_CLASS_003" "MODEL_CLASS_005" "MODEL_CLASS_006"
# [6] "MODEL_CLASS_007" "MODEL_CLASS_008" "MODEL_CLASS_009" "MODEL_CLASS_010" "MODEL_CLASS_011"
#[11] "MODEL_CLASS_012" "MODEL_CLASS_013"
#Creating VRT from 33 MLP files...
#[1] "VRT band names after renaming:"
# [1] "MODEL_CLASS_001" "MODEL_CLASS_002" "MODEL_CLASS_003" "MODEL_CLASS_005" "MODEL_CLASS_006"
# [6] "MODEL_CLASS_007" "MODEL_CLASS_008" "MODEL_CLASS_009" "MODEL_CLASS_010" "MODEL_CLASS_011"
#[11] "MODEL_CLASS_012" "MODEL_CLASS_013"
#[1] "VRT band names after class mapping:"
# [1] "TF"  "SN"  "OF"  "CC"  "AJ"  "WRC" "SH"  "BU"  "AGR" "MGR" "WA"  "SS" 
#Reprojecting shapefile to match VRT CRS...
#Created directory: D:/Paul_Pop_RS_GIS_files/force/dir_sent_RFC_eight_ml/processed 
#Processing class: TF ...
#Non-zero pixels in TF : 95668977          
#Successfully processed TF 
#Processing class: SN ...
#Non-zero pixels in SN : 7931184           
#Successfully processed SN 
#Processing class: OF ...
#Non-zero pixels in OF : 1941672           
#Successfully processed OF 
#Processing class: CC ...
#Non-zero pixels in CC : 113400            
#Successfully processed CC 
#Processing class: AJ ...
#Non-zero pixels in AJ : 125361            
#Successfully processed AJ 
#Processing class: WRC ...
#Non-zero pixels in WRC : 1472026          
#Successfully processed WRC 
#Processing class: SH ...
#Non-zero pixels in SH : 19816537          
#Successfully processed SH 
#Processing class: BU ...
#Non-zero pixels in BU : 117037            
#Successfully processed BU 
#Processing class: AGR ...
#Non-zero pixels in AGR : 148663           
#Successfully processed AGR 
#Processing class: MGR ...
#Non-zero pixels in MGR : 1001121          
#Successfully processed MGR 
#Processing class: WA ...
#Non-zero pixels in WA : 1831789           
#Successfully processed WA 
#Processing class: SS ...
#Non-zero pixels in SS : 1429952           
#Successfully processed SS 
#✓ Successfully completed dir_sent_RFC_eight_ml 

                            
#==================================================
#Processing model 5 of 5 : dir_ml_sent_RFC_pbc 
#==================================================

#Processing model: dir_ml_sent_RFC_pbc 
#Directory: D:/Paul_Pop_RS_GIS_files/force3/dir_ml_sent_RFC_pbc 
#[1] "Found 33 PREDICTION_HL_ML_MLP.tif files"
#[1] "Original band names:"
# [1] "MODEL_CLASS_001" "MODEL_CLASS_002" "MODEL_CLASS_003" "MODEL_CLASS_004" "MODEL_CLASS_005"
# [6] "MODEL_CLASS_006" "MODEL_CLASS_007" "MODEL_CLASS_008" "MODEL_CLASS_009" "MODEL_CLASS_010"
#[11] "MODEL_CLASS_011" "MODEL_CLASS_012" "MODEL_CLASS_013"
#Creating VRT from 33 MLP files...
#[1] "VRT band names after renaming:"
# [1] "MODEL_CLASS_001" "MODEL_CLASS_002" "MODEL_CLASS_003" "MODEL_CLASS_004" "MODEL_CLASS_005"
# [6] "MODEL_CLASS_006" "MODEL_CLASS_007" "MODEL_CLASS_008" "MODEL_CLASS_009" "MODEL_CLASS_010"
#[11] "MODEL_CLASS_011" "MODEL_CLASS_012" "MODEL_CLASS_013"
#[1] "VRT band names after class mapping:"
# [1] "TF"  "SN"  "OF"  "BA"  "CC"  "AJ"  "WRC" "SH"  "BU"  "AGR" "MGR" "WA"  "SS" 
#Reprojecting shapefile to match VRT CRS...
#Created directory: D:/Paul_Pop_RS_GIS_files/force3/dir_ml_sent_RFC_pbc/processed 
#Processing class: TF ...
#Non-zero pixels in TF : 43186526          
#Successfully processed TF 
#Processing class: SN ...
#Non-zero pixels in SN : 7582564           
#Successfully processed SN 
#Processing class: OF ...
#Non-zero pixels in OF : 580921            
#Successfully processed OF 
#Processing class: BA ...
#Non-zero pixels in BA : 5740512           
#Successfully processed BA 
#Processing class: CC ...
#Non-zero pixels in CC : 116793            
#Successfully processed CC 
#Processing class: AJ ...
#Non-zero pixels in AJ : 61641             
#Successfully processed AJ 
#Processing class: WRC ...
#Non-zero pixels in WRC : 1059440          
#Successfully processed WRC 
#Processing class: SH ...
#Non-zero pixels in SH : 11728796          
#Successfully processed SH 
#Processing class: BU ...
#Non-zero pixels in BU : 203853            
#Successfully processed BU 
#Processing class: AGR ...
#Non-zero pixels in AGR : 337866           
#Successfully processed AGR 
#Processing class: MGR ...
#Non-zero pixels in MGR : 1034902          
#Successfully processed MGR 
#Processing class: WA ...
#Non-zero pixels in WA : 1729416           
#Successfully processed WA 
#Processing class: SS ...
#Non-zero pixels in SS : 1029785           
#Successfully processed SS 
#✓ Successfully completed dir_ml_sent_RFC_pbc 

#For models which are not found in the system or the working directory, we get the following output:

#==================================================
#Processing model 99 of 114 : dir_ml_RFC_25 
#==================================================

#Processing model: dir_ml_RFC_25 
#Directory: /home/GIS/force/dir_ml_RFC_25 
#⚠ Completed with warnings: dir_ml_RFC_25 

#==================================================
#Processing model 100 of 114 : dir_ml_SVC_21 
#==================================================

#Processing model: dir_ml_SVC_21 
#Directory: /home/GIS/force/dir_ml_SVC_21  
#⚠ Completed with warnings: dir_ml_SVC_21 
