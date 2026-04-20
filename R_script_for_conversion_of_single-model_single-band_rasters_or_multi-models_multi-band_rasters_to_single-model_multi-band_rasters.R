### Raster restructuring for the primary workflow 

# The following script can be used for batch restructuring of rasters for a 
# large number of models. It can either be single-model single-band rasters (those 
# single-bands containing every class of that model as pixels) or multi-model 
# multi-band rasters (each band containing one model output, where each band 
# contains every class of that model as pixels) or a mix of both. For the purpose 
# of the batch processing, you need to have a csv file containing the following 
# details (even if it is just one model, the details need to be entered in the csv file).
 
# sl_no	   model_name	                      directory	            missing_classes
# 1	       dir_ml_RFC_40	                  C:\Users\GIS\force	  4,8
# 2	       dir_ml_RFC_41	                  C:\Users\GIS\force	  4,8
# 3	       dir_ml_RFC_42	                  C:\Users\GIS\force	  4,8
# 4	       dir_ml_RFC_pbc_one_two_three	    C:\Users\GIS\force	

# The model_name is the name of the folder containing the model, the directory is 
# the directory containing that folder (it can be a higher/broad level folder as the
# script also recursively checks in the subfolders too), and the missing_classes 
# column need to be filed if the specific model doesn't use all the classes or 
# clubs one more classes under other classes. In the case of the first three models 
# in this example, the class 4 and 8 out of the maximum of 13 classes are excluded
# from the models. They need to be added as a comma-seperated list with no space. 
# In the last model, the prediction files exist as multi-model multi-band rasters. 
# In this case, **the folder name of the model should be named such that the last 
# n number of terms should be the names/numbers (either word or numeric) of each 
# individual model**. For example, dir_ml_RFC_pbc_one_two_three will become 
# dir_ml_RFC_pbc_one, dir_ml_RFC_pbc_two, dir_ml_RFC_pbc_three upon the running 
# of the following script. So, the folder names of such batch-processed folders 
# should be named keeping this sequence in mind. Not that in the  # Generate class 
# values section of the script, if you haven't provided the max_classes (directly 
# in the script) and missing_classes in the csv file, then it will attempt to 
# derive that data by checking 50,000 pixels of the rasters. But this is inefficient 
# and incomplete. So, you can either either 50,000 to a much larger number for 
# accuracy or avoid the processing time and power for this, and manually specify the values. 

# IMPORTANT: Checklist of changes needed in the script for your personal use:
# 1) Set input directory (replace the folder path with your folder path)
# 2) Load the CSV file with model information (replace the file name with your file name)
# 3) If you want to process files other than PREDICTION_HL_ML_MLP.tif, then you should 
# modify the pattern in the line starting with # Look for PREDICTION_HL_ML_MLP.tif files
# 4) The parse band function assumes that your model band names are of the 
# general structure MODEL_CLASS_0000X_Y  where X could be 1 and Y could be EQUALIZED;
# 0001 refers to the number for EQUALIZED and EQUALIZED is the model containing 
# equalized samples from each class. You can modify the function to account for your 
# model band names.
# 5) You need to change the value for the max_classes (depending on your total number 
# of classes) under the line # Global constant for number of classes in all the models 
# AND the second line after # Processing function for a single model

# Set input directory
setwd("D:/GIS/Multi-model-extraction")

# Load the CSV file with model information
rast <- read.csv("multi_model_raster_details.csv")

# Load the necessary 'terra' library
library(terra)

# A number of functions need to be created for the steps like finding the location 
# of the models specified in the csv file, finding band information from band names, 
# parsing folder name and extracting model sequence, and processing the raster files.

# Function to find prediction TIFF files
find_prediction_tif_files <- function(base_dir, model_name) {
  base_dir_normalized <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  
  # Find model directory recursively if needed
  all_dirs <- list.dirs(base_dir_normalized, full.names = TRUE, recursive = TRUE)
  model_dir <- all_dirs[basename(all_dirs) == model_name]
  
  if (length(model_dir) == 0) {
    message(sprintf("ERROR: Directory '%s' not found", model_name))
    return(data.frame(full_path = character(0), relative_path = character(0)))
  }
  
  model_dir <- model_dir[1]
  message(sprintf("Found: %s", model_dir))
  
  # Look for PREDICTION_HL_ML_MLP.tif files (change the name or pattern to include
  # more types of file if you need them)
  tif_files <- list.files(model_dir, 
                          pattern = "PREDICTION_HL_ML_MLP\\.tif$", 
                          full.names = TRUE, 
                          recursive = TRUE, 
                          ignore.case = TRUE)
  
  if (length(tif_files) == 0) {
    message(sprintf("No PREDICTION_HL_ML_MLP.tif files found in %s", model_dir))
    return(data.frame(full_path = character(0), relative_path = character(0)))
  }
  
  message(sprintf("Found %d files", length(tif_files)))
  
  # Get relative paths
  relative_paths <- sapply(tif_files, function(full_path) {
    relative <- sub(paste0("^", model_dir), "", normalizePath(full_path, winslash = "/"))
    sub("^/", "", relative)
  })
  
  return(data.frame(
    full_path = tif_files,
    relative_path = relative_paths,
    stringsAsFactors = FALSE
  ))
}

# Function to parse band information from band names
parse_band_info <- function(band_name, band_index, model_name) {
  # For multi-band rasters with naming pattern like "MODEL_CLASS_00001_EQUALIZED"
  if (grepl("MODEL_CLASS_", band_name)) {
    # Extract the suffix after MODEL_CLASS_
    suffix <- sub("MODEL_CLASS_", "", band_name)
    parts <- strsplit(suffix, "_")[[1]]
    
    if (length(parts) >= 2) {
      # Get the number part (e.g., "00001")
      num_part <- parts[1]
      # Get the type (e.g., "EQUALIZED", "PROPORTIONAL", "ANTIPROPORTIONAL")
      type_part <- paste(parts[-1], collapse = "_")
      
      # Create folder name based on the type
      folder_suffix <- switch(type_part,
                              "EQUALIZED" = "one",
                              "PROPORTIONAL" = "two",
                              "ANTIPROPORTIONAL" = "three",
                              tolower(type_part))
      
      return(list(
        band_prefix = paste0("MODEL_CLASS_", num_part),
        folder_name = paste0(model_name, "_", folder_suffix),
        original_suffix = suffix
      ))
    }
  }
  
  # For single-band rasters or fallback
  return(list(
    band_prefix = "MODEL_CLASS",
    folder_name = model_name,
    original_suffix = NULL
  ))
}

# Global constant for number of classes in all the models
max_classes <- 13  

# Function to generate class values with validation
generate_class_values <- function(max_classes, missing_classes_str = NULL) {
  # Create full sequence of classes (1 to max_classes)
  all_classes <- 1:max_classes
  
  # If no missing classes are specified, return all classes
  if (is.null(missing_classes_str) || is.na(missing_classes_str) || missing_classes_str == "") {
    #message(sprintf("  ✓ Using ALL classes 1-%d (%d classes)", n_classes, n_classes))
    return(all_classes)
  }
  
  # Parse missing classes
  missing_classes <- as.numeric(unlist(strsplit(as.character(missing_classes_str), ",")))
  
  # Validate that missing classes are within range
  invalid_missing <- missing_classes[missing_classes < 1 | missing_classes > max_classes]
  if (length(invalid_missing) > 0) {
    message(sprintf("  ⚠ Warning: Invalid missing classes %s (outside 1-%d), ignoring them", 
                    paste(invalid_missing, collapse=","), max_classes))
    missing_classes <- missing_classes[missing_classes >= 1 & missing_classes <= max_classes]
  }
  
  # Remove missing classes from the full sequence
  existing_classes <- all_classes[!all_classes %in% missing_classes]
  
  message(sprintf("   Maximum number of classes: %d", max_classes))
  message(sprintf("   Classes not present in the model: %s (%d classes)", 
                  if(length(missing_classes)>0) paste(missing_classes, collapse=", ") else "none",
                  length(missing_classes)))
  message(sprintf("   Classes present in the model: %s (%d classes)", 
                  paste(existing_classes, collapse=", "),
                  length(existing_classes)))
  
  return(existing_classes)
}

# Function to parse folder name and extract model sequence (handles both words and numbers)
parse_folder_name <- function(model_name) {
  # Define number words for word-based sequences
  number_words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  
  # Split the folder name by underscores
  parts <- strsplit(model_name, "_")[[1]]
  
  # Check for numeric sequence at the end (e.g., 41, 42, 43)
  # Find consecutive numeric parts at the end
  numeric_sequence <- c()
  if (length(parts) >= 2) {
    # Check from the end backwards for numeric parts
    for (i in length(parts):1) {
      if (grepl("^[0-9]+$", parts[i])) {
        numeric_sequence <- c(parts[i], numeric_sequence)
      } else {
        break
      }
    }
  }
  
  # Check for word-based sequence (e.g. one, two, three...)
  word_sequence <- c()
  word_indices <- which(parts %in% number_words)
  if (length(word_indices) > 0) {
    # Check if they form a consecutive sequence
    is_consecutive <- all(diff(word_indices) == 1)
    if (is_consecutive) {
      word_sequence <- parts[word_indices]
    }
  }
  
  # Determine which type of sequence you have
  if (length(numeric_sequence) > 0) {
    # Numeric sequence found
    base_name_parts <- parts[1:(length(parts) - length(numeric_sequence))]
    base_name <- if (length(base_name_parts) > 0) paste(base_name_parts, collapse="_") else ""
    
    return(list(
      base_name = base_name,
      sequence = numeric_sequence,
      sequence_type = "numeric",
      n_models = length(numeric_sequence)
    ))
    
  } else if (length(word_sequence) > 0) {
    # Word sequence found
    base_name_parts <- parts[1:(word_indices[1]-1)]
    base_name <- if (length(base_name_parts) > 0) paste(base_name_parts, collapse="_") else ""
    
    return(list(
      base_name = base_name,
      sequence = word_sequence,
      sequence_type = "word",
      n_models = length(word_sequence)
    ))
    
  } else {
    # No sequence found - single model
    return(list(
      base_name = model_name,
      sequence = NULL,
      sequence_type = "none",
      n_models = 1
    ))
  }
}

# Function to get output folder name for a specific band index
get_output_folder_name <- function(model_name, band_index) {
  folder_info <- parse_folder_name(model_name)
  
  if (folder_info$n_models == 1 || is.null(folder_info$sequence)) {
    # Single model - return original name
    return(model_name)
  } else {
    # Multi-model - create folder name with specific sequence element
    sequence_element <- folder_info$sequence[band_index]
    
    # For numeric sequences, just append the number
    # For word sequences, append the word
    if (folder_info$sequence_type == "numeric") {
      return(paste(folder_info$base_name, sequence_element, sep="_"))
    } else {
      # Word sequence
      return(paste(folder_info$base_name, sequence_element, sep="_"))
    }
  }
}

# Processing function for a single model
process_single_model_unified <- function(base_dir, model_name, output_base_dir, 
                                         max_classes = 13,  # Set the default/total no. of classes 
                                         missing_classes_str = NULL) {
  
  n_classes = max_classes-length(missing_classes_str)
  
  message(sprintf("Looking in base directory: %s", base_dir))
  
  # Find prediction files
  tif_files_df <- find_prediction_tif_files(base_dir, model_name)
  
  if (nrow(tif_files_df) == 0) {
    message(sprintf("No files found for model: %s", model_name))
    return(FALSE)
  }
  
  # Analyze first file to get band information
  first_file <- tif_files_df$full_path[1]
  message(sprintf("\nAnalyzing first file: %s", basename(first_file)))
  
  r <- tryCatch({
    rast(first_file)
  }, error = function(e) {
    message(sprintf("Error reading raster: %s", e$message))
    return(NULL)
  })
  
  if (is.null(r)) {
    return(FALSE)
  }
  
  n_bands <- nlyr(r)
  band_names <- names(r)
  message(sprintf("Raster has %d band(s)", n_bands))
  message(sprintf("Band names: %s", paste(band_names, collapse = ", ")))
  
  # Parse folder name to understand model structure
  folder_info <- parse_folder_name(model_name)
  message(sprintf("\n  📁 Folder details:"))
  message(sprintf("     Base name: %s", folder_info$base_name))
  
  if (!is.null(folder_info$sequence)) {
    message(sprintf("     Number of models: %d", folder_info$n_models))
    
    # Check if number of bands matches number of models in folder name
    if (n_bands != folder_info$n_models) {
      message(sprintf("  ⚠ Warning: Number of bands (%d) doesn't match number of models in folder name (%d)", 
                      n_bands, folder_info$n_models))
    }
  } else {
    message(sprintf("     Single model (no sequence detected)"))
  }
  
  # Generate class values
  if (is.null(n_classes) || is.na(n_classes)) {
    # Detect from raster
    message("\n  No n_classes specified, detecting from raster...")
    sample_vals <- values(r[[1]])[1:min(50000, ncell(r))]
    class_values <- sort(unique(sample_vals[!is.na(sample_vals) & is.finite(sample_vals)]))
    n_classes <- max(class_values)
    message(sprintf("  ✓ Detected %d classes from raster (values: %s)", 
                    length(class_values), paste(class_values, collapse=", ")))
  } else {
    # Generate from max_classes and missing_classes
    class_values <- generate_class_values(max_classes, missing_classes_str)
  }
  
  if (length(class_values) == 0) {
    message("ERROR: No class values generated! Check your max_classes and missing_classes settings.")
    return(FALSE)
  }
  
  # Confirm which bands will be created
  message(sprintf("\n  Will create %d binary bands for the following classes:", length(class_values)))
  message(sprintf("     %s", paste(sprintf("MODEL_CLASS_%03d", class_values), collapse=", ")))
  
  # Create output directories based on folder name and band count
  output_dirs <- list()
  for (band_idx in 1:n_bands) {
    # Get the appropriate output folder name for this band
    output_folder_name <- get_output_folder_name(model_name, band_idx)
    output_dir <- file.path(output_base_dir, output_folder_name)
    output_dirs[[band_idx]] <- output_dir
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
      message(sprintf("\n  📁 Created output directory for band %d: %s", band_idx, output_dir))
    }
  }
  
  if (n_bands == 1) {
    message(sprintf("\n  🔄 Single-band model: Processing %d files into %d classes", 
                    nrow(tif_files_df), length(class_values)))
  } else {
    message(sprintf("\n  🔄 Multi-band model: Processing %d files, %d bands each -> %d classes per band", 
                    nrow(tif_files_df), n_bands, length(class_values)))
  }
  
  # Process each file
  successful_files <- 0
  total_files <- nrow(tif_files_df)
  
  for (i in 1:total_files) {
    input_path <- tif_files_df$full_path[i]
    relative_path <- tif_files_df$relative_path[i]
    
    tryCatch({
      r <- rast(input_path)
      
      # Process each band separately
      for (band_idx in 1:nlyr(r)) {
        output_dir <- output_dirs[[band_idx]]
        output_path <- file.path(output_dir, relative_path)
        output_dir_path <- dirname(output_path)
        
        if (!dir.exists(output_dir_path)) {
          dir.create(output_dir_path, recursive = TRUE)
        }
        
        # Create binary rasters with consistent naming
        # Initialize as NULL
        binary_rasters <- NULL
        
        for (cv in class_values) {
          class_formatted <- sprintf("%03d", cv)
          band_name <- paste0("MODEL_CLASS_", class_formatted)
          
          # Create binary raster
          binary_raster <- ifel(r[[band_idx]] == cv, 1, 0)
          
          # Set the name
          names(binary_raster) <- band_name
          
          # Combine rasters
          if (is.null(binary_rasters)) {
            binary_rasters <- binary_raster
          } else {
            binary_rasters <- c(binary_rasters, binary_raster)
          }
        }
        
        # Write the raster
        writeRaster(binary_rasters, output_path, overwrite = TRUE)
      }
      
      successful_files <- successful_files + 1
      
      # Progress message for every files
      message(sprintf("  📄 [%d/%d] Processing: %s", i, 
                      total_files, basename(input_path)))
      
    }, error = function(e) {
      message(sprintf("  ❌ ERROR processing file %d/%d (%s): %s", 
                      i, total_files, basename(input_path), e$message))
    })
  }
  
  # Print summary
  if (successful_files == total_files) {
    message(sprintf("\n✅ Successfully processed all %d files for model: %s", 
                    total_files, model_name))
    
    if (n_bands == 1) {
      message(sprintf("  📁 Output folder: %s", output_dirs[[1]]))
    } else {
      message(sprintf("  📁 Created %d output folders:", n_bands))
      for (band_idx in 1:n_bands) {
        message(sprintf("     - %s", output_dirs[[band_idx]]))
      }
    }
  } else {
    message(sprintf("\n⚠️  Processed %d/%d files successfully for model: %s", 
                    successful_files, total_files, model_name))
  }
  
  return(successful_files > 0)
}

# Main processing function that loops through all models
process_all_models_unified <- function(rasters_df, output_base_path) {
  # Create output base directory
  if (!dir.exists(output_base_path)) {
    dir.create(output_base_path, recursive = TRUE)
    message(sprintf("Created base output directory: %s", output_base_path))
  }
  
  # Process each model in the CSV
  for (i in 1:nrow(rasters_df)) {
    model <- rasters_df[i, ]
    
    message("\n", paste(rep("=", 80), collapse = ""))
    message(sprintf("Processing model %d of %d: %s\n", i, nrow(rasters_df), model$model_name))
    
    # Get the missing_classes parameter from CSV
    missing_classes_str <- NULL
    
    # Total/default number of classes in models
    max_classes <- 13
    
    # Check for missing_classes column
    if ("missing_classes" %in% names(model) && !is.na(model$missing_classes) && model$missing_classes != "") {
      missing_classes_str <- as.character(model$missing_classes)
      #message(sprintf("  Excluded classes in the model: %s", missing_classes_str))
    }
    
    # Process the model
    result <- process_single_model_unified(
      model$directory, 
      model$model_name, 
      output_base_path,
      max_classes,
      missing_classes_str
    )
    
    if (!result) {
      message(sprintf("  ⚠ WARNING: No files processed for model: %s", model$model_name))
    }
  }
  
  message("\n", paste(rep("=", 80), collapse = ""))
  message("✅ Processing complete!")
}

# Set output path
if (.Platform$OS.type == "windows") {
  output_base_path <- "D:\\Reorganized_Rasters_Categorical"
} else {
  output_base_path <- "/home/user/Reorganized_Rasters_Categorical"
}

# Run the processing on ALL models
process_all_models_unified(rast, output_base_path)

# Output in the console:
# 
# ================================================================================
#   Processing model 1 of 4: dir_ml_RFC_40
# 
# Looking in base directory: C:\Users\GIS\force
# Found: C:/Users/GIS/force/dir_ml_RFC_40
# Found 33 files
# 
# Analyzing first file: PREDICTION_HL_ML_MLP.tif
# Raster has 1 band(s)
# Band names: MODEL_CLASS
# 
# 📁 Folder details:
#   Base name: dir_ml_RFC
# Number of models: 1
# Maximum number of classes: 13
# Classes not present in the model: 4, 8 (2 classes)
# Classes present in the model: 1, 2, 3, 5, 6, 7, 9, 10, 11, 12, 13 (11 classes)
# 
# Will create 11 binary bands for the following classes:
#   MODEL_CLASS_001, MODEL_CLASS_002, MODEL_CLASS_003, MODEL_CLASS_005, MODEL_CLASS_006, MODEL_CLASS_007, MODEL_CLASS_009, MODEL_CLASS_010, MODEL_CLASS_011, MODEL_CLASS_012, MODEL_CLASS_013
# 
# 📁 Created output directory for band 1: D:\Restructured_Rasters/dir_ml_RFC_40
# 
# 🔄 Single-band model: Processing 33 files into 11 classes
# 📄 [1/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [2/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [3/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [4/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [5/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [6/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [7/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [8/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [9/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [10/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [11/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [12/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [13/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [14/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [15/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [16/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [17/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [18/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [19/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [20/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [21/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [22/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [23/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [24/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [25/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [26/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [27/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [28/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [29/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [30/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [31/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [32/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [33/33] Processing: PREDICTION_HL_ML_MLP.tif
# 
# ✅ Successfully processed all 33 files for model: dir_ml_RFC_40
# 📁 Output folder: D:\Restructured_Rasters/dir_ml_RFC_40
# 
# ================================================================================
#   Processing model 2 of 4: dir_ml_RFC_41
# 
# Looking in base directory: C:\Users\GIS\force
# Found: C:/Users/GIS/force/dir_ml_RFC_41
# Found 33 files
# .
# .
# .
# .
# .
# .
# ================================================================================
#   Processing model 4 of 4: dir_ml_RFC_pbc_one_two_three
# 
# Looking in base directory: C:\Users\GIS\force
# Found: C:/Users/GIS/force/third/dir_ml_RFC_pbc_one_two_three
# Found 33 files
# 
# Analyzing first file: PREDICTION_HL_ML_MLP.tif
# Raster has 3 band(s)
# Band names: MODEL_CLASS_00001_EQUALIZED, MODEL_CLASS_00002_PROPORTIONAL, MODEL_CLASS_00003_ANTIPROPORTIONAL
# 
# 📁 Folder details:
#   Base name: dir_ml_RFC_pbc
# Number of models: 3
# 
# Will create 13 binary bands for the following classes:
#   MODEL_CLASS_001, MODEL_CLASS_002, MODEL_CLASS_003, MODEL_CLASS_004, MODEL_CLASS_005, MODEL_CLASS_006, MODEL_CLASS_007, MODEL_CLASS_008, MODEL_CLASS_009, MODEL_CLASS_010, MODEL_CLASS_011, MODEL_CLASS_012, MODEL_CLASS_013
# 
# 📁 Created output directory for band 1: D:\Restructured_Rasters/dir_ml_RFC_pbc_one
# 
# 📁 Created output directory for band 2: D:\Restructured_Rasters/dir_ml_RFC_pbc_two
# 
# 📁 Created output directory for band 3: D:\Restructured_Rasters/dir_ml_RFC_pbc_three
# 
# 🔄 Multi-band model: Processing 33 files, 3 bands each -> 13 classes per band
# 📄 [1/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [2/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [3/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [4/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [5/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [6/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [7/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [8/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [9/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [10/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [11/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [12/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [13/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [14/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [15/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [16/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [17/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [18/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [19/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [20/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [21/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [22/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [23/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [24/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [25/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [26/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [27/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [28/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [29/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [30/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [31/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [32/33] Processing: PREDICTION_HL_ML_MLP.tif
# 📄 [33/33] Processing: PREDICTION_HL_ML_MLP.tif
# 
# ✅ Successfully processed all 33 files for model: dir_ml_RFC_pbc_one_two_three
# 📁 Created 3 output folders:
#   - D:\Restructured_Rasters/dir_ml_RFC_pbc_one
# - D:\Restructured_Rasters/dir_ml_RFC_pbc_two
# - D:\Restructured_Rasters/dir_ml_RFC_pbc_three
# 
# ================================================================================
#   ✅ Processing complete!
