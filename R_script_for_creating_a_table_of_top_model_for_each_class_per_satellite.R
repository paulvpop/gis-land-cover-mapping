## Create a table of top model for each class per satellite

# NOTE THAT THE EXAMPLE OUTPUTS OF THE CODE CAN BE FOUND IN THE ONLINE DOCUMENTATION OF
# THIS SCRIPT: https://github.com/paulvpop/gis-land-cover-mapping/blob/main/13.%20Integration.md#create-a-table-of-top-models-for-each-class-per-satellite

# Set input directory (to where the model details csv is located) if you
# already don't have it in the R environment (from the Accuracy Assessment
# section). This time, the model_details.csv files should have the details
# of all the models from multiple systems (if located in multiple systems).
setwd("C:/Users/GIS/force")
# OR use ctrl+shift+H

# Read in the model_details.csv
mod_det <- read.csv("model_details.csv")

# Change the working directory to where the class-wise accuracy assessment
# csv files are located (the outputs of the previous section).
setwd("D:/GIS")
# OR use ctrl+shift+H

# Note that if you  have top models in different system, it needs to be transferred 
# to one system for the following procedure. To find out if you have such models, 
# first run this entire workflow once and see if you are getting warning messages 
# which says something like: "1: In get_top_model_info(csv_file, mod_det_df) :
# Could not find model folder for dir_ml_RFC_sixteen in /home/GIS/force"
# If transferring from one system to another, then the model_details.csv needs to
# be updated with the correct path ('directory') for the transferred model folders.
# Make sure that the satellite specific model is placed under the right folder. For 
# example, a Landsat model under the folder "Landsat". While this is not necessary
# for the creation of table or ranking, it is necessary for the actual integration
# step. Then read this updated model_details csv:
mod_det <- read.csv("model_details_updated.csv")

# Load necessary libraries needed for this workflow
# (install if not already present with install.packages("package_name"))
library(dplyr)
library(stringr)
library(fs)

# A number of functions need to be defined for this step

# Function to recursively search for a folder
find_model_folder <- function(base_path, model_name) {
  # First check if the model folder exists directly under base_path
  direct_path <- file.path(base_path, model_name)
  if (dir.exists(direct_path)) {
    cat("  Found direct match:", direct_path, "\n")
    return(list(
      folder_path = direct_path,
      match_type = "direct",
      matches_found = 1
    ))
  }
  
  # If not found directly under, search recursively
  all_dirs <- list.dirs(base_path, recursive = TRUE, full.names = TRUE)
  
  # Find directories that match the model_name (exact match)
  exact_matches <- all_dirs[basename(all_dirs) == model_name]
  
  if (length(exact_matches) > 0) {
    cat("  Found", length(exact_matches), "exact match(es)\n")
    return(list(
      folder_path = exact_matches[1],
      match_type = "exact",
      matches_found = length(exact_matches),
      all_matches = exact_matches
    ))
  }
  
  # Also try fuzzy matching in case of slight name variations (like "rfc_13 instead of RFC_13")
  fuzzy_matches <- all_dirs[grepl(model_name, basename(all_dirs), ignore.case = TRUE)]
  
  if (length(fuzzy_matches) > 0) {
    cat("  Found", length(fuzzy_matches), "fuzzy match(es)\n")
    return(list(
      folder_path = fuzzy_matches[1],
      match_type = "fuzzy",
      matches_found = length(fuzzy_matches),
      all_matches = fuzzy_matches
    ))
  }
  
  # Return NULL if no matches found
  cat("  No matches found for model:", model_name, "\n")
  return(NULL)
}

# Function to extract class name from filename
extract_class_name <- function(filename) {
  # Extract the part between "class_" and the next underscore
  class_part <- str_match(filename, "class_([A-Z]+)_")[,2]
  return(class_part)
}

# Function to extract satellite type from filename
extract_satellite <- function(filename) {
  # Extract satellite name (Landsat or Sentinel)
  if (grepl("_Landsat_", filename)) {
    return("Landsat")
  } else if (grepl("_Sentinel_", filename)) {
    return("Sentinel")
  } else {
    return(NA)
  }
}

# Function to find raster files in a folder
find_raster_files <- function(folder_path, class_name, actual_class) {
  # Look for files with _processed suffix
  all_files <- list.files(folder_path, full.names = TRUE)
  
  # Try different naming patterns in order of preference
  patterns <- c(
    paste0("^", actual_class, "_processed\\.(tif|tfw)$"), # Unchanged workflow: change _processed to _unchanged
    paste0("^", class_name, "_processed\\.(tif|tfw)$"), # Unchanged workflow: change _processed to _unchanged
    paste0(".*", actual_class, ".*_processed\\.(tif|tfw)$"), # Unchanged workflow: change _processed to _unchanged
    paste0(".*", class_name, ".*_processed\\.(tif|tfw)$"), # Unchanged workflow: change _processed to _unchanged
    "_processed\\.(tif|tfw)$" # Unchanged workflow: change _processed to _unchanged
  )
  
  for (pattern in patterns) {
    matching_files <- all_files[grepl(pattern, basename(all_files), ignore.case = TRUE)]
    if (length(matching_files) >= 2) {
      # Separate tif and tfw files
      tif_files <- matching_files[grepl("\\.tif$", matching_files, ignore.case = TRUE)]
      tfw_files <- matching_files[grepl("\\.tfw$", matching_files, ignore.case = TRUE)]
      
      if (length(tif_files) > 0 && length(tfw_files) > 0) {
        return(list(
          tif_file = tif_files[1],
          tfw_file = tfw_files[1]
        ))
      }
    }
  }
  
  return(NULL)
}

# Function to get the top model info with recursive search
get_top_model_info <- function(csv_file, mod_det_df) {
  cat("Processing:", basename(csv_file), "\n")
  
  # Read the CSV file
  class_df <- read.csv(csv_file)
  
  # Get class name and satellite from filename
  class_name <- extract_class_name(basename(csv_file))
  satellite <- extract_satellite(basename(csv_file))
  
  cat("  Class:", class_name, "| Satellite:", satellite, "\n")
  
  # Filter for top model (integrated_rank = 1)
  top_model <- class_df %>%
    filter(integrated_rank == 1) %>%
    dplyr::select(modelName, Class)
  
  if (nrow(top_model) == 0) {
    warning(paste("No top model found (integrated_rank = 1) in", csv_file))
    return(NULL)
  }
  
  # Get the model name
  model_name <- top_model$modelName[1]
  actual_class <- top_model$Class[1]
  
  cat("  Top model:", model_name, "| Class in CSV:", actual_class, "\n")
  
  # Find the directory path from mod_det
  model_path_info <- mod_det_df %>%
    filter(model_name == !!model_name) %>% #The !! is the "bang-bang' operator 
    #!!model_name means "use the value stored in the variable called model_name" 
    #instead of treating it as a column name in the dataframe. 
    dplyr::select(directory, model_name)
  
  if (nrow(model_path_info) == 0) {
    cat("  Model not found in mod_det, trying fuzzy match...\n")
    # Try fuzzy matching in mod_det
    model_path_info <- mod_det_df %>%
      filter(grepl(model_name, model_name, ignore.case = TRUE)) %>%
      dplyr::select(directory, model_name)
  }
  
  if (nrow(model_path_info) == 0) {
    warning(paste("Model", model_name, "not found in mod_det dataframe"))
    return(NULL)
  }
  
  base_path <- model_path_info$directory[1]
  cat("  Base path from mod_det:", base_path, "\n")
  
  # Recursively search for the model folder
  search_result <- find_model_folder(base_path, model_name)
  
  if (is.null(search_result)) {
    warning(paste("Could not find model folder for", model_name, "in", base_path))
    return(NULL)
  }
  
  # Extract folder path and match type
  model_folder_path <- search_result$folder_path
  match_type <- search_result$match_type
  
  # Log the type of match found
  cat("  Found via", match_type, "match\n")
  
  # If multiple matches were found, log them
  if (!is.null(search_result$all_matches) && length(search_result$all_matches) > 1) {
    cat("  Warning: Multiple matches found. Using first match.\n")
    cat("  All matches:\n")
    for (match in search_result$all_matches) {
      cat("    -", match, "\n")
    }
  }
  
  cat("  Using folder:", model_folder_path, "\n")
  
  # Now look for the processed folder
  processed_folder_path <- file.path(model_folder_path, "processed")  # Unchanged workflow: change "processed" to "unchanged"
  
  # Check if processed folder exists  # Unchanged workflow: change processed to unchanged
  if (!dir.exists(processed_folder_path)) {
    cat("  'processed' folder not found in model folder. Checking for files directly in model folder...\n") # Unchanged workflow: change processed to unchanged
    # Try the model folder itself
    processed_folder_path <- model_folder_path
  }
  
  # Find the raster files
  raster_files <- find_raster_files(processed_folder_path, class_name, actual_class)
  
  if (is.null(raster_files)) {
    cat("  Could not find raster files. Available files in", processed_folder_path, ":\n")
    all_files <- list.files(processed_folder_path)
    cat("    ", paste(all_files, collapse = "\n    "), "\n")
    return(NULL)
  }
  
  cat("  Found TIFF file:", basename(raster_files$tif_file), "\n")
  cat("  Found TFW file:", basename(raster_files$tfw_file), "\n")
  
  return(list(
    class_name = class_name,
    actual_class = actual_class,
    satellite = satellite,
    model_name = model_name,
    tif_file = raster_files$tif_file,
    tfw_file = raster_files$tfw_file,
    source_folder = processed_folder_path,
    match_type = match_type,
    model_folder_path = model_folder_path
  ))
}

# Main function to organize all top model files
organize_top_model_rasters <- function(mod_det_path, metrics_folder) {
  # Read model details
  mod_det_df <- read.csv(mod_det_path)
  
  # Find all metrics files
  metrics_files <- list.files(
    path = metrics_folder,
    pattern = "class_.*_metrics_final\\.csv$", # Unchanged workflow: change _metrics_final to _metrics_unchanged_final
    full.names = TRUE
  )
  
  if (length(metrics_files) == 0) {
    stop("No metrics files found in the specified folder")
  }
  
  # Filter out files containing "Overall" (case-insensitive)
  original_count <- length(metrics_files)
  metrics_files <- metrics_files[!grepl("Overall", metrics_files, ignore.case = TRUE)]
  filtered_count <- original_count - length(metrics_files)
  
  cat("Found", original_count, "metrics files total\n")
  if (filtered_count > 0) {
    cat("Filtered out", filtered_count, "files containing 'Overall'\n")
  }
  cat("Processing", length(metrics_files), "files after filtering\n\n")
  
  # Create output folders for Landsat and Sentinel
  output_base <- file.path(metrics_folder, "Top_Models_Organized") # Unchanged workflow: change Top_Models_Organized to Top_Models_Unchanged_Organized 
  landsat_folder <- file.path(output_base, "Landsat")
  sentinel_folder <- file.path(output_base, "Sentinel")
  
  dir.create(landsat_folder, recursive = TRUE, showWarnings = FALSE)
  dir.create(sentinel_folder, recursive = TRUE, showWarnings = FALSE)
  
  cat("Created output folders:\n")
  cat("  Landsat:", landsat_folder, "\n")
  cat("  Sentinel:", sentinel_folder, "\n\n")
  
  # Track processed files
  processed_summary <- data.frame(
    class = character(),
    satellite = character(),
    model = character(),
    status = character(),
    source_file = character(),
    dest_file = character(),
    match_type = character(),
    stringsAsFactors = FALSE
  )
  
  # Track top model details separately for each satellite
  landsat_top_models <- data.frame()
  sentinel_top_models <- data.frame()
  
  # Process each metrics file
  for (csv_file in metrics_files) {
    cat("=", rep("=", 60), "\n", sep = "")
    
    # Get top model info
    model_info <- get_top_model_info(csv_file, mod_det_df)
    
    # Extract top model row from metrics file and categorize by satellite
    if (file.exists(csv_file)) {
      class_df <- read.csv(csv_file)
      # Get the row where integrated_rank == 1
      top_model_row <- class_df %>% filter(integrated_rank == 1)
      
      if (nrow(top_model_row) > 0) {
        # Extract satellite from filename
        satellite_type <- extract_satellite(basename(csv_file))
        # Add filename as a column for reference
        top_model_row$source_file <- basename(csv_file)
        
        # Add to appropriate satellite dataframe
        if (satellite_type == "Landsat") {
          landsat_top_models <- bind_rows(landsat_top_models, top_model_row)
        } else if (satellite_type == "Sentinel") {
          sentinel_top_models <- bind_rows(sentinel_top_models, top_model_row)
        } else {
          # If satellite not identified, add to both or handle as needed
          landsat_top_models <- bind_rows(landsat_top_models, top_model_row)
          sentinel_top_models <- bind_rows(sentinel_top_models, top_model_row)
        }
      }
    }
    
    if (is.null(model_info)) {
      processed_summary <- rbind(processed_summary, data.frame(
        class = extract_class_name(basename(csv_file)),
        satellite = extract_satellite(basename(csv_file)),
        model = "NOT FOUND",
        status = "FAILED",
        source_file = "NOT FOUND",
        dest_file = "NOT COPIED",
        match_type = "NOT FOUND",
        stringsAsFactors = FALSE
      ))
      next
    }
    
    # Determine destination folder based on satellite
    if (model_info$satellite == "Landsat") {
      dest_folder <- landsat_folder
    } else if (model_info$satellite == "Sentinel") {
      dest_folder <- sentinel_folder
    } else {
      warning(paste("Unknown satellite for file:", csv_file))
      next
    }
    
    # Construct destination filenames
    # Use a consistent naming convention
    dest_tif <- file.path(dest_folder, paste0(model_info$class_name, "_top_model.tif"))
    dest_tfw <- file.path(dest_folder, paste0(model_info$class_name, "_top_model.tfw"))
    
    # Copy files
    tryCatch({
      # Check if destination files already exist
      if (file.exists(dest_tif)) {
        cat("  Overwriting existing file:", basename(dest_tif), "\n")
      }
      
      # Copy with progress feedback
      cat("  Copying", basename(model_info$tif_file), "to", dest_folder, "\n")
      file.copy(model_info$tif_file, dest_tif, overwrite = TRUE)
      
      cat("  Copying", basename(model_info$tfw_file), "to", dest_folder, "\n")
      file.copy(model_info$tfw_file, dest_tfw, overwrite = TRUE)
      
      cat("  ✓ Successfully copied", model_info$class_name, "files\n")
      
      # Add to summary
      processed_summary <- rbind(processed_summary, data.frame(
        class = model_info$class_name,
        satellite = model_info$satellite,
        model = model_info$model_name,
        status = "SUCCESS",
        source_file = basename(model_info$tif_file),
        dest_file = basename(dest_tif),
        match_type = model_info$match_type,
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      cat("  ✗ Error copying files:", e$message, "\n")
      
      processed_summary <- rbind(processed_summary, data.frame(
        class = model_info$class_name,
        satellite = model_info$satellite,
        model = model_info$model_name,
        status = "FAILED",
        source_file = basename(model_info$tif_file),
        dest_file = "NOT COPIED",
        match_type = model_info$match_type,
        stringsAsFactors = FALSE
      ))
    })
  }
  
  # Print final summary
  cat("\n", rep("=", 70), "\n", sep = "")
  cat("PROCESSING COMPLETE\n")
  cat(rep("=", 70), "\n\n", sep = "")
  
  # Count successful copies
  success_count <- sum(processed_summary$status == "SUCCESS")
  failed_count <- sum(processed_summary$status == "FAILED")
  
  cat("Summary:\n")
  cat("  Total metrics files processed:", nrow(processed_summary), "\n")
  cat("  Successfully copied:", success_count, "\n")
  cat("  Failed:", failed_count, "\n\n")
  
  # Files by satellite
  landsat_success <- sum(processed_summary$status == "SUCCESS" & processed_summary$satellite == "Landsat")
  sentinel_success <- sum(processed_summary$status == "SUCCESS" & processed_summary$satellite == "Sentinel")
  
  cat("By satellite:\n")
  cat("  Landsat files copied:", landsat_success, "\n")
  cat("  Sentinel files copied:", sentinel_success, "\n\n")
  
  # List copied files
  if (success_count > 0) {
    cat("Successfully copied files:\n")
    success_df <- processed_summary[processed_summary$status == "SUCCESS", ]
    for (i in 1:nrow(success_df)) {
      cat("  ", success_df$class[i], " (", success_df$satellite[i], "): ", 
          success_df$model[i], " [", success_df$match_type[i], "] → ", 
          success_df$dest_file[i], "\n", sep = "")
    }
  }
  
  # List failed files
  if (failed_count > 0) {
    cat("\nFailed files:\n")
    failed_df <- processed_summary[processed_summary$status == "FAILED", ]
    for (i in 1:nrow(failed_df)) {
      cat("  ", failed_df$class[i], " (", failed_df$satellite[i], ") - ", 
          failed_df$model[i], "\n", sep = "")
    }
  }
  
  # Verify folder contents
  cat("\nFolder contents:\n")
  
  if (dir.exists(landsat_folder)) {
    landsat_files <- list.files(landsat_folder)
    cat("  Landsat folder (", length(landsat_files), " files):\n", sep = "")
    if (length(landsat_files) > 0) {
      for (f in landsat_files) {
        cat("    - ", f, "\n", sep = "")
      }
    } else {
      cat("    (empty)\n")
    }
  }
  
  if (dir.exists(sentinel_folder)) {
    sentinel_files <- list.files(sentinel_folder)
    cat("  Sentinel folder (", length(sentinel_files), " files):\n", sep = "")
    if (length(sentinel_files) > 0) {
      for (f in sentinel_files) {
        cat("    - ", f, "\n", sep = "")
      }
    } else {
      cat("    (empty)\n")
    }
  }
  
  # Save summary to CSV
  summary_file <- file.path(output_base, "copy_summary.csv")
  write.csv(processed_summary, summary_file, row.names = FALSE)
  cat("\nDetailed summary saved to:", summary_file, "\n")
  
  # Save top models details separately for each satellite
  landsat_top_models_file <- file.path(output_base, "top_models_landsat.csv")
  sentinel_top_models_file <- file.path(output_base, "top_models_sentinel.csv")
  
  if (nrow(landsat_top_models) > 0) {
    write.csv(landsat_top_models, landsat_top_models_file, row.names = FALSE)
    cat("Landsat top models details saved to:", landsat_top_models_file, "\n")
  } else {
    cat("No Landsat top models found\n")
  }
  
  if (nrow(sentinel_top_models) > 0) {
    write.csv(sentinel_top_models, sentinel_top_models_file, row.names = FALSE)
    cat("Sentinel top models details saved to:", sentinel_top_models_file, "\n")
  } else {
    cat("No Sentinel top models found\n")
  }
  
  # Optional: Also save a combined file for reference
  combined_top_models <- bind_rows(
    if(nrow(landsat_top_models) > 0) mutate(landsat_top_models) else NULL,
    if(nrow(sentinel_top_models) > 0) mutate(sentinel_top_models) else NULL
  )
  
  if (nrow(combined_top_models) > 0) {
    combined_file <- file.path(output_base, "top_models_combined.csv")
    write.csv(combined_top_models, combined_file, row.names = FALSE)
    cat("Combined top models details saved to:", combined_file, "\n")
  }
  
  return(list(
    summary = processed_summary,
    landsat_top_models = landsat_top_models,
    sentinel_top_models = sentinel_top_models,
    landsat_folder = landsat_folder,
    sentinel_folder = sentinel_folder,
    summary_file = summary_file,
    landsat_top_models_file = landsat_top_models_file,
    sentinel_top_models_file = sentinel_top_models_file,
    filtered_count = filtered_count
  ))
}

# The next step is deployment. This step will use the previously created functions 
# to give us csv files for the top models for each class for both Landsat and Sentinel

# Set your paths
mod_det_path <- "C:/Users/GIS/force/model_details.csv"

# Or the updated model details csv
mod_det_path <- "D:/GIS/model_details_updated.csv"

# Set the directory containing all the "class_OF_Landsat_metrics_final.csv", 
# "class_SN_Sentinel_metrics_final.csv" etc as metrics_folder. The dot in the next
# line of code refers to the current directory.
metrics_folder <- "./metrics_files" # Unchanged workflow: change metrics_files to metrics_unchanged_files after placing the respective files there

# Run the main function
result <- organize_top_model_rasters(mod_det_path, metrics_folder)

# Note that in the result output, you will get messages like "No matches found for model: dir_ml_RFC_sixteen" if the model 
# is not there in the device or the directory specified is incorrect. If in another device, copy those specific models
# from the other device and rerun the script again.

# If you want to see the result structure:
print(result$summary)

# Access the output folders:
cat("\nOutput folders created at:\n")
cat("Landsat:", result$landsat_folder, "\n")
cat("Sentinel:", result$sentinel_folder, "\n")
