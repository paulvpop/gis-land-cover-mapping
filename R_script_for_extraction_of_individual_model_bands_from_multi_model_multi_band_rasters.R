#This is given as Step 5 under "Model Prediction.md"

### Step 5: Convert multi-models-multi-band-rasters to single-model-multi-band-rasters

#DO THIS STEP ONLY IF YOU HAVE MULTI-MODEL-MULTI-BAND-RASTERS AS MODEL PREDICTION OUTPUTS. For example, if each of the model output rasters contain proportional, equiavlised, and anti-proportional bands for each class.

#If you have run prediction for multiple models at the same time, you should use the following R Script for extraction of individual model bands from combined multi-band rasters to create seperate folders of the same structure for individual models (needed for the input of the post-processing step).

#Set input directory (to where the raster files which needs splitting is located):
setwd("D:/GIS/Multi-model-extraction")

#Load the file containing the folder names containing the rasters and the directory paths
#where these folders are located
rast <- read.csv("multi_model_raster_details.csv")

#The above csv file ("multi_model_raster_details.csv") should look like this:

#sl_no	model_name	directory
#1	dir_ml_RFC_landsat	C:\Users\Kedaravindan Bhaskar\force
#2	dir_ml_RFC_landsat_nodata	D:\Paul_Pop_RS_GIS_files\Reva

#Load necessary packages
library(terra)
library(stringr)
library(dplyr)
library(purrr)

# Function to extract band information from raster (with error messaging)
get_band_names <- function(raster_path) {
  tryCatch({
    r <- rast(raster_path)
    band_names <- names(r)
    return(band_names)
  }, error = function(e) {
    message(sprintf("Error reading %s: %s", basename(raster_path), e$message))
    return(character(0))
  })
}

# Function to parse band name and extract components

parse_band_name <- function(band_name) {
  # Extract the part after "Band XX: "
  clean_name <- str_replace(band_name, "^Band \\d+: ", "")
  
  # Extract components using regex
  pattern <- "MODEL_CLASS_(\\d+)_(PROPORTIONAL|EQUALIZED|ANTIPROPORTIONAL)"
  matches <- str_match(clean_name, pattern)
  #matches is a matrix from str_match() containing the regex pattern matches
  
  if (all(!is.na(matches))) {
  #!is.na(matches) checks which elements are not NA (i.e., successfully matched)
  #all(!is.na(matches)) returns TRUE only if ALL parts of the pattern were found
    # Create the base name without suffix
    base_name <- paste0("MODEL_CLASS_", matches[1, 2])
    
    return(list(
      class_number = as.numeric(matches[1, 2]), # The number after CLASS_
      suffix = matches[1, 3],                   # PROPORTIONAL/EQUALIZED/ANTIPROPORTIONAL
      base_name = base_name,                    # Base name without suffix
      full_name = clean_name                    # The complete cleaned band name
    ))
  } else {
    return(NULL)                                # If unsuccessful, returns null
  }
}

# Function to create reorganized raster

create_reorganized_raster <- function(input_raster_path, output_dir, suffix_type, relative_path) {
  tryCatch({
    # Read the multi-band raster
    r <- rast(input_raster_path)
    band_names <- names(r)
    
    # Filter bands that match the desired suffix
    selected_bands <- character(0)
    band_indices <- integer(0)
    base_names <- character(0)  # Store base names without suffix
    
    for (i in seq_along(band_names)) {
      band_info <- parse_band_name(band_names[i])
      if (!is.null(band_info) && band_info$suffix == suffix_type) {
        selected_bands <- c(selected_bands, band_info$full_name)
        band_indices <- c(band_indices, i)
        base_names <- c(base_names, band_info$base_name)  # Store base name
      }
    }
    
    # Skip if no bands found for this suffix
    if (length(band_indices) == 0) {
      message(sprintf("No bands found for suffix '%s' in %s", suffix_type, basename(input_raster_path)))
      return(FALSE)
    }
    
    # Extract the selected bands
    selected_raster <- r[[band_indices]]
    
    # Update band names to base names (without suffix)
    names(selected_raster) <- base_names
    
    # Create the full output path with original subfolder structure
    output_path <- file.path(output_dir, relative_path)
    
    # Create the output directory if it doesn't exist
    output_dir_path <- dirname(output_path)
    if (!dir.exists(output_dir_path)) {
      dir.create(output_dir_path, recursive = TRUE)
    }
    
    # Write the new multi-band raster
    writeRaster(selected_raster, output_path, overwrite = TRUE, NAflag = -9999)
    
    message(sprintf("Created: %s with %d bands (renamed without suffix)", 
                    output_path, length(band_indices)))
    
    # Optional: Print band names to verify
    if (length(band_indices) > 0) {
      cat("  Band names in output: ", paste(base_names, collapse = ", "), "\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    message(sprintf("Error processing %s: %s", basename(input_raster_path), e$message))
    return(FALSE)
  })
}

# Function to find prediction TIFF files within model-specific subdirectories

find_prediction_tif_files <- function(base_dir, model_name) {
  # Normalize the base directory path
  base_dir_normalized <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  
  # Recursively search for the model directory in all subdirectories
  all_dirs <- list.dirs(base_dir_normalized, full.names = TRUE, recursive = TRUE)
  
  # Find directories that match the model name (case-insensitive)
  matching_dirs <- all_dirs[grepl(paste0("^", model_name, "$"), basename(all_dirs), ignore.case = TRUE)]
  
  if (length(matching_dirs) == 0) {
    message(sprintf("No directory found for model '%s' in %s", model_name, base_dir_normalized))
    message("Available directories:")
    print(unique(basename(all_dirs)))
    return(data.frame(full_path = character(0), relative_path = character(0)))
  }
  
  # Use the first matching directory (or you could handle multiple matches)
  model_dir <- matching_dirs[1]
  if (length(matching_dirs) > 1) {
    message(sprintf("Multiple directories found for model '%s', using: %s", model_name, model_dir))
    message("All matching directories:")
    print(matching_dirs)
  } else {
    message(sprintf("Found model directory: %s", model_dir))
  }
  
  # Find all TIFF files recursively within the model directory
  tif_files <- list.files(model_dir, pattern = "\\.tif$", 
                          full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  
  if (length(tif_files) == 0) {
    message(sprintf("No TIFF files found in %s", model_dir))
    return(data.frame(full_path = character(0), relative_path = character(0)))
  }
  
  # Filter only prediction files
  prediction_files <- tif_files[grepl("PREDICTION", basename(tif_files), ignore.case = TRUE)]
  
  if (length(prediction_files) == 0) {
    message(sprintf("No prediction files found in %s. Found these TIFF files:", model_dir))
    print(basename(tif_files))
    return(data.frame(full_path = character(0), relative_path = character(0)))
  }
  
  message(sprintf("Found %d prediction files in %s", length(prediction_files), model_name))
  
  # Get relative paths (relative to the model directory)
  relative_paths <- sapply(prediction_files, function(full_path) {
    full_path_normalized <- normalizePath(full_path, winslash = "/", mustWork = FALSE)
    
    # Remove the model directory part
    if (startsWith(full_path_normalized, model_dir)) {
      relative <- substr(full_path_normalized, nchar(model_dir) + 1, nchar(full_path_normalized))
      # Remove leading slash if present
      return(sub("^/", "", relative))
    } else {
      return(basename(full_path_normalized))
    }
  })
  
  # Ensure we're using forward slashes for consistency
  relative_paths <- gsub("\\\\", "/", relative_paths)
  
  return(data.frame(
    full_path = prediction_files,
    relative_path = relative_paths,
    stringsAsFactors = FALSE
  ))
}

# Function to process all rasters in a model directory

process_model_directory <- function(base_dir, model_name, output_base_dir) {
  # Find prediction TIFF files within the model-specific directory
  tif_files_df <- find_prediction_tif_files(base_dir, model_name)
  
  if (nrow(tif_files_df) == 0) {
    message(sprintf("No prediction TIFF files found for model %s in %s", model_name, base_dir))
    return(NULL)
  }
  
  message(sprintf("Found %d prediction TIFF files for model %s", nrow(tif_files_df), model_name))
  
  # Determine available suffixes from the first valid file
  suffixes <- character(0)
  files_tried <- 0
  max_files_to_try <- min(5, nrow(tif_files_df))
  
  while (length(suffixes) == 0 && files_tried < max_files_to_try) {
    files_tried <- files_tried + 1
    first_raster_bands <- get_band_names(tif_files_df$full_path[files_tried])
    
    if (length(first_raster_bands) > 0) {
      suffixes <- unique(sapply(first_raster_bands, function(band) {
        info <- parse_band_name(band)
        if (!is.null(info)) info$suffix else NA
      }))
      suffixes <- suffixes[!is.na(suffixes)]
    }
    
    if (length(suffixes) == 0 && files_tried < max_files_to_try) {
      message(sprintf("File %d/%d has no recognizable band names, trying next...", 
                      files_tried, max_files_to_try))
    }
  }
  
  if (length(suffixes) == 0) {
    message("No valid band names found in any of the first few files.")
    # Debug: print band names from first file
    if (nrow(tif_files_df) > 0) {
      message("Band names from first file:")
      print(get_band_names(tif_files_df$full_path[1]))
    }
    return(NULL)
  }
  
  message(sprintf("Found suffixes: %s", paste(suffixes, collapse = ", ")))
  
  # Process for each suffix type
  for (suffix in suffixes) {
    # Create output directory for this suffix and model
    output_dir_name <- paste0(model_name, "_", suffix)
    output_dir <- file.path(output_base_dir, output_dir_name)
    
    # Create base directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      message(sprintf("Created directory: %s", output_dir))
    }
    
    # Process each raster file while preserving folder structure
    message(sprintf("Processing %d files for suffix '%s'...", nrow(tif_files_df), suffix))
    
    results <- mapply(function(full_path, relative_path) {
      create_reorganized_raster(full_path, output_dir, suffix, relative_path)
    }, tif_files_df$full_path, tif_files_df$relative_path)
    
    successful <- sum(results)
    message(sprintf("Successfully processed %d/%d files for suffix '%s'", 
                    successful, nrow(tif_files_df), suffix))
  }
  
  return(suffixes)
}

# Main processing function

process_all_models <- function(rasters_df, output_base_path) {
  # Create output base directory
  if (!dir.exists(output_base_path)) {
    dir.create(output_base_path, recursive = TRUE)
  }
  
  # Process each model directory
  for (i in 1:nrow(rasters_df)) {
    model <- rasters_df[i, ]
    message(sprintf("\n=== Processing model: %s ===", model$model_name))
    message(sprintf("Input directory: %s", model$directory))
    
    # Check if directory exists
    if (!dir.exists(model$directory)) {
      message(sprintf("Directory does not exist: %s", model$directory))
      next
    }

    # Process the model directory
    suffixes <- process_model_directory(model$directory, model$model_name, output_base_path)
    
    if (!is.null(suffixes)) {
      message(sprintf("Completed processing for model: %s", model$model_name))
      message(sprintf("Created folders for suffixes: %s", paste(suffixes, collapse = ", ")))
    } else {
      message(sprintf("No processing done for model: %s", model$model_name))
    }
  }
}

# Set your output base path

if (.Platform$OS.type == "windows") {
  output_base_path <- "D:\\Reorganized_Rasters"
} else {
  output_base_path <- "/home/user/Reorganized_Rasters"  # Linux path
}

# Run the processing

process_all_models(rast, output_base_path)

#The output would look like this:

# === Processing model: dir_ml_RFC_landsat_1 ===
#   Input directory: C:\GIS\force
# Found model directory: C:/GIS/force/dir_ml_RFC_landsat
# Found 33 prediction files in dir_ml_RFC_landsat
# Found 33 prediction TIFF files for model dir_ml_RFC_landsat
# Found suffixes: ANTIPROPORTIONAL, EQUALIZED, PROPORTIONAL
# Created directory: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL
# Processing 33 files for suffix 'ANTIPROPORTIONAL'...
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0000_Y0000/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0000_Y0001/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0000_Y0002/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0000_Y0003/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0001_Y0000/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0001_Y0001/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0001_Y0002/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0001_Y0003/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0001_Y0004/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0001_Y0005/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0001_Y0006/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0002_Y-001/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0002_Y0000/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0002_Y0001/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0002_Y0002/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0002_Y0003/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0002_Y0004/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0002_Y0005/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0002_Y0006/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0003_Y0000/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0003_Y0001/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0003_Y0002/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0003_Y0003/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0003_Y0004/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0003_Y0005/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0004_Y0000/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0004_Y0001/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0004_Y0002/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0004_Y0003/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0004_Y0004/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0004_Y0005/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0005_Y0003/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_ANTIPROPORTIONAL/X0005_Y0004/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
# Successfully processed 33/33 files for suffix 'ANTIPROPORTIONAL'
# Created directory: D:\Reorganized_Rasters/dir_ml_RFC_landsat_EQUALIZED
# Processing 33 files for suffix 'EQUALIZED'...
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_EQUALIZED/X0000_Y0000/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'EQUALIZED'
#.
#.
#.
# Successfully processed 33/33 files for suffix 'PROPORTIONAL'
# Completed processing for model: dir_ml_RFC_landsat
# Created folders for suffixes: ANTIPROPORTIONAL, EQUALIZED, PROPORTIONAL
# 
# === Processing model: dir_ml_RFC_landsat_nodata ===
#   Input directory: D:\GIS\force2
# Found model directory: D:/GIS/force2/test/dir_ml_RFC_landsat_nodata
# Found 33 prediction files in dir_ml_RFC_landsat_nodata
# Found 33 prediction TIFF files for model dir_ml_RFC_landsat_nodata
# Found suffixes: ANTIPROPORTIONAL, EQUALIZED, PROPORTIONAL
# Processing 33 files for suffix 'ANTIPROPORTIONAL'...
# Created: D:\Reorganized_Rasters/dir_ml_RFC_landsat_nodata_ANTIPROPORTIONAL/X0000_Y0000/PREDICTION_HL_ML_MLP.tif with 13 bands for suffix 'ANTIPROPORTIONAL'
#.
#.
#.
# Successfully processed 33/33 files for suffix 'PROPORTIONAL'
# Completed processing for model: dir_ml_RFC_landsat_nodata
# Created folders for suffixes: ANTIPROPORTIONAL, EQUALIZED, PROPORTIONAL
