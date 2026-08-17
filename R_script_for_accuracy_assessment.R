## Accuracy assessment

# This is essentially the core stage of the accuracy assessment section.

# Set broad working directory containing all the models:
setwd("D:/RS_GIS_files/LULC-SIANG/classified_imagery")

# Use the sink function to start logging command outputs into a textfile 
# "accuracy_assessment_output.txt"
sink("accuracy_assessment_output.txt", split = TRUE)
print("This will appear in the console and be saved to output.txt")

# This accuracy assessment workflow will check if the class exists within a 30 m
# radius buffer of training data points. This is to choose biologically meaningful
# information for the accuracy assessment i.e. while machine learning models can
# displace the prediction pixels, if the displacement is within a distance that
# can be visibly seen by a human, instead of exactly at the point, then it can be
# considered as accurately predicted/classified.

# Load necessary packages:
library(dplyr)
library(terra)
library(sf)
library(raster)
library(sp)
library(tidyverse)
library(ggplot2)
library(tibble)

# Add the  kml/kmz file containing the validation data (final_sample_recoded.kml) from
# the previous step to the "classified_imagery" folder list the kmz files in the folder
# (should be just the one file with the validation data)

# SKIP THIS STEP IF YOU ARE STARTING WITH A KML FILE AND NOT A KMZ FILE

# Create temporary directory
temp_dir <- tempdir()

# SKIP THIS STEP IF YOU ARE STARTING WITH A KML FILE
# Unzip KMZ (this will extract the KML file inside
unzip(kmz, exdir = temp_dir)

# Find the extracted KML file 
kml_file <- list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE)
# OR
# or just the kml file if you're starting directly from kml (uncomment and use)
kml_file <- list.files(pattern = "\\.kml$", full.names = TRUE)

# Read the KML file with sf
data <- st_read(kml_file)

# Get coordinates
coordinates <- st_coordinates(data)

# As an input, prepare coordinates as spatial points with correct CRS (WGS84)
pts <- st_as_sf(as.data.frame(coordinates), coords = c("X", "Y"), crs = 4326)

# Define India NSF LCC (EPSG:7755)
india_nsf_lcc <- st_crs(7755)  # WGS 84 / India NSF LCC

# Ensure points are in India NSF LCC
pts_projected <- st_transform(pts, india_nsf_lcc)

# SKIP THE FOLLOWING STEP IF YOU ALREADY HAVE THE OBJECT 'mod_det' in the R environment from the 'Post processing' section

# Load the csv containing model details
mod_det <- read.csv("model_details.csv")

# MAIN LOOP FOR PROCESSING BEGINS HERE:

# Loop through each model in mod_det
for (i in 1:nrow(mod_det)) {
 
  # Extract model details
  model_name <- mod_det$model_name[i]
  base_directory <- mod_det$directory[i]
  BA_as_TF <- mod_det$BA_as_TF[i]
  SH_as_TF <- mod_det$SH_as_TF[i]
  BA.SH_as_TF <- mod_det$BA.SH_as_TF[i]
 
  cat("\n", rep("=", 50), "\n", sep = "")
  cat("Processing model:", model_name)
  cat("\n", rep("=", 50), "\n\n", sep = "")
 
  # Set input directory (where the raster files which has undergone post-processing
  # is located):
  input_dir <- file.path(base_directory, model_name)
  processed_dir <- file.path(input_dir, "processed")
 
  # Check if directory exists and is accessible
  if (!dir.exists(processed_dir)) {
    warning(paste("Directory does not exist or is inaccessible:", processed_dir))
    cat("SKIPPING MODEL:", model_name, "- Directory not accessible\n")
    next  # Skip to next model
  }
 
  # Try to set working directory with error handling
  old_wd <- getwd()
  try_result <- tryCatch({
    setwd(processed_dir)
    TRUE  # Success
  }, error = function(e) {
    warning(paste("Cannot change working directory:", processed_dir))
    cat("Error:", e$message, "\n")
    FALSE  # Failure
  })
 
  if (!try_result) {
    cat("SKIPPING MODEL:", model_name, "- Cannot access working directory\n")
    next  # Skip to next model
  }
 
  # Get list of all processed TIFF files (with capital name abbreviations and the
  # _processed suffix)
  tif_files <- list.files(getwd(), pattern = "_processed\\.tif$", full.names = TRUE)
 
  # Check if any TIFF files were found
  if (length(tif_files) == 0) {
    warning(paste("No processed TIFF files found in:", processed_dir))
    cat("SKIPPING MODEL:", model_name, "- No TIFF files found\n")
    setwd(old_wd)  # Restore original working directory
    next  # Skip to next model
  }
 
  # Process each TIFF file
 
  # Load and reproject all rasters if required
  class_rasters <- tryCatch({
    rasters <- rast(tif_files)
    if (crs(rasters) != india_nsf_lcc$wkt) {
      rasters <- project(rasters, india_nsf_lcc$wkt)
    }
    rasters
  }, error = function(e) {
    warning(paste("Error loading raster files for model:", model_name))
    cat("Raster loading error:", e$message, "\n")
    NULL
  })
 
  if (is.null(class_rasters)) {
    cat("SKIPPING MODEL:", model_name, "- Error loading raster files\n")
    setwd(old_wd)  # Restore original working directory
    next  # Skip to next model
  }
 
  # Get all the class names from the class_rasters (for later use)
  class_names <- names(class_rasters)
 
  # List the classes outputted by the model
  cat("Model predicts/outputs these classes:", paste(class_names, collapse = ", "), "\n")
 
  # Get reference point values (all classes)
  point_allclasses <- terra::extract(class_rasters, vect(pts_projected))
 
  # Initialize final_results with point values
  final_results <- cbind(
    data.frame(Name = data$Name,
               Longitude = coordinates[, "X"],
               Latitude = coordinates[, "Y"]),
    point_allclasses[,-1]  # Exclude ID column
  )
 
  # Recode classes based on mod_det specifications
  if (BA_as_TF == 1) {
    final_results <- final_results %>%
      mutate(Name = recode(Name, "BA" = "TF"))
    cat("Recoded BA as TF\n")
  } else if (SH_as_TF == 1) {
    final_results <- final_results %>%
      mutate(Name = recode(Name, "SH" = "TF"))
    cat("Recoded SH as TF\n")
  } else if (BA.SH_as_TF == 1) {
    final_results <- final_results %>%
      mutate(Name = recode(Name,
                           "BA" = "TF",
                           "SH" = "TF"))
    cat("Recoded BA and SH as TF\n")
  } else {
    cat("No recoding of class applied\n")
  }
 
  # There maybe be some rasters in some models with the class_rasters that are
  # that have zero values throughout (zero predictions). Identify and remove
  # such class rasters
  class_cols <- 4:ncol(final_results)  # Class columns start from column 4
  class_sums <- colSums(final_results[, class_cols, drop = FALSE])
 
  # Find classes with zero predictions
  zero_prediction_classes <- names(class_sums[class_sums == 0])
 
  if (length(zero_prediction_classes) > 0) {
    cat("Removing classes with zero predictions:",
        paste(zero_prediction_classes, collapse = ", "), "\n")
   
    # Remove these classes from final_results
    final_results <- final_results[, !names(final_results) %in% zero_prediction_classes]
   
    # Update class_names to exclude zero-prediction classes
    class_names <- setdiff(class_names, zero_prediction_classes)
   
    # Update class_rasters to exclude zero-prediction classes
    if (length(class_names) > 0) {
      class_rasters <- class_rasters[[class_names]]
    } else {
      warning("No classes left after removing zero-prediction classes!")
      setwd(old_wd)
      next
    }
   
    cat("Active classes after zero-value raster removal:", paste(class_names, collapse = ", "), "\n")
  }
 
  # Check if we have multiple classes for meaningful analysis
  if (length(class_names) < 2) {
    cat("SKIPPING MODEL: Only one class (", class_names, ")
          remains - insufficient for accuracy assessment\n", sep = "")
    setwd(old_wd)
    next
  }
 
  # Update class_cols after potential removal
  class_cols <- 4:ncol(final_results)
 
  # Filter training data to only include classes that the model predicts/has non-zero
  #values
  final_results <- final_results %>%
    filter(Name %in% class_names)
 
  # Add a column indicating whether the target class is present or absent
 
  # Get column names of final_results
  column_names <- colnames(final_results)
 
  # Create row sums:
  final_results$row_sum <- rowSums(final_results[,class_cols])
 
  # Create a logical vector indicating if each point has a '2' or greater values under its named
  # class
  result <- final_results$Name %in% column_names &
    final_results$row_sum > 1
 
  # Convert to numeric and add as a new column to final_results
  final_results$multi_class <- as.numeric(result)
  # Remove the result list
  rm(result)
 
  # Identify points that need cleaning (target class = 1 at point, plus one or more other class = 1)
  multi_class_rows <- which(final_results$multi_class == 1)
 
  # For each problematic row, keep only the target class (if present)
  for (i_row in multi_class_rows) {
    target_class <- final_results$Name[i_row]
   
    # Check if target class exists in this row's classes
    if (target_class %in% names(final_results[, class_cols])) {
      # Get the value for target class
      target_value <- final_results[i_row, target_class]
     
      # If target class is present (1), set all others to 0
      if (target_value == 1) {
        final_results[i_row, class_cols] <- 0  # First set all to 0
        final_results[i_row, target_class] <- 1  # Then restore target class
      }
    }
  }
 
  # Remove row_sum and multi_class columns (their values have changed due to the last
  # for-loop)
  final_results <- final_results %>% dplyr::select (-c("row_sum","multi_class"))
 
  # Add a column indicating whether the target class is present or absent
 
  # Get column names of final_results
  column_names <- colnames(final_results)
 
  # Create a logical vector indicating if each point has a '1' under its named class
  result <- final_results$Name %in% column_names &
    final_results[cbind(1:nrow(final_results), match(final_results$Name, column_names))] == 1
 
  # Convert to numeric and add as a new column to final_results
  final_results$class_present <- as.numeric(result)
 
  # Identify points that need buffer check (target class = 0 at point)
  points_to_check <- which(final_results$class_present == 0)
 
  # Process buffers for potential class replacements
  for (tif_file in tif_files) {
    var_name <- sub("_processed\\.tif$", "", basename(tif_file))
   
    # Skip if this class was removed due to zero predictions
    if (!var_name %in% class_names) {
      next
    }
   
    r <- class_rasters[[var_name]]
   
    #If there are any points in the points_to_check object, create buffers for
    #for only those points
    if (length(points_to_check) > 0) {
      subset_pts <- pts_projected[points_to_check,]
      buffer_30m <- st_buffer(subset_pts, dist = 30)
     
      # Extract buffer values
      buffer_values <- terra::extract(r, vect(buffer_30m))
     
      # Process each point that might need updating
      for (i_pt in seq_along(points_to_check)) {
        pt_id <- points_to_check[i_pt]
        target_class <- final_results$Name[pt_id]  # Get the expected class for this point
       
        # Only process if this buffer class matches the point's expected class
        if (target_class == var_name) {
          # Check if target class exists in buffer
          if (any(buffer_values[buffer_values$ID == i_pt, 2] == 1, na.rm = TRUE)) {
           
            # For debugging: Check sum before before update
            before_sum <- sum(final_results[pt_id, 4:(ncol(final_results)-1)])
            #cat("DEBUG: Point", pt_id, "before - sum =", before_sum, "\n")
           
            # Found target class in buffer but not at point - apply replacement rules
            final_results[pt_id, var_name] <- 1  # Set target class to 1
           
            # Set all other classes to 0 for this point (use actual column names)
            class_cols <- 4:(ncol(final_results)-1) #The columns containing the class (from 4 to n-1)
            other_classes <- setdiff(names(final_results)[class_cols], var_name)
            # Gets the column names at positions specified by class_cols and returns
            # the elements in columns names that are not being processed.
            # For example, if column names = "AGR", "AJ", "BU", "CC", "MGR", "OF",
            # "SH", "SN", "SS", "TF", "WA", "WRC" and the class being currently
            # processed (var_name) = "AGR", then the result would be all classes
            # except AGR
            final_results[pt_id, other_classes] <- 0
            # This sets all other class columns (other than the target class AGR)
            #to 0 for this specific point
           
            # For debugging: Check sum after update
            after_sum <- sum(final_results[pt_id, 4:(ncol(final_results)-1)])
            #cat("DEBUG: Point", pt_id, "after - sum =", after_sum, "\n")
           
            if (after_sum != 1) {
              cat("ERROR: Point", pt_id, "still has sum =", after_sum, "after buffer update!\n")
             
            }
          }
        }
       
        #cat("Updated", sum(final_results[[var_name]] == 1), "presences for", var_name, "\n")
      }
    }
  }
 
  # Emergency cleanup: Ensure each point has exactly one class by fixing multi-class points
  #cat("\nCleaning up multi-class points...\n")
  class_cols <- 4:(ncol(final_results)-1)
 
  for (i in 1:nrow(final_results)) {
    row_sum <- sum(final_results[i, class_cols])
   
    if (row_sum > 1) {
      expected_class <- final_results$Name[i]
     
      # Only keep this point classified if the expected class is actually present
      if (expected_class %in% names(final_results)[class_cols] &&
          final_results[i, expected_class] == 1) {
        # Clear other classes, keep expected class
        other_classes <- setdiff(names(final_results)[class_cols], expected_class)
        final_results[i, other_classes] <- 0
        #cat("Fixed point", i, "- kept expected class", expected_class, "\n")
      } else {
        # Expected class not present - clear ALL classes (point becomes unclassified)
        final_results[i, class_cols] <- 0
        cat("Fixed point", i, "- cleared all classes (expected class not present)\n")
      }
    }
  }
 
  # Verification
  final_results$row_sum <- rowSums(final_results[, class_cols])
  point_allclasses$row_sum <- rowSums(point_allclasses[,-1])
 
  cat("\nOriginal point values:\n")
  print(table(point_allclasses$row_sum))
 
  cat("\nFinal results after buffer enhancement:\n")
  print(table(final_results$row_sum))
 
  # # Write final results to CSV with model-specific filename
  # output_filename <- paste0("extracted_values_with_buffer_check_", model_name, ".csv")
  # write.csv(final_results, output_filename, row.names = FALSE)
 
  cmatrix <- final_results
 
  # For the contingency table, filter out 0s and remove unnecessary columns
  cmatrix2 <- cmatrix %>%
    filter(!(row_sum %in% 0)) %>%
    dplyr::select(-c(Longitude, Latitude, class_present, row_sum))
 
  # Create a contingency table (base confusion matrix)
  true_classes <- cmatrix2$Name
  predicted_classes <- apply(cmatrix2[, -1], 1, function(row) {
    colnames(cmatrix2[, -1])[which.max(row)]  # Get column name with value 1
  })

  # Ensure all classes are represented in both dimensions
  # Get all unique classes from both actual and predicted
  all_class_levels <- sort(unique(c(true_classes, predicted_classes)))
  
  # Create confusion matrix table after converting the classes to factor     # with all levels to ensure they appear in the table
  conf_matrix <- table(
    Predicted = factor(predicted_classes, levels = all_class_levels),
    Actual = factor(true_classes, levels = all_class_levels)
  )
 
  # Add margin totals
  conf_matrix <- addmargins(conf_matrix)
 
  # # Print the confusion matrix
  # print("Confusion Matrix:")
  # print(conf_matrix)
 
  # Create a dataframe for pixels/coordinates which were not classified into any class (NC)
  # This will be needed later on.
 
  # Create NC counts with ALL classes from the confusion matrix
  all_classes <- colnames(conf_matrix)[-ncol(conf_matrix)]  # Exclude 'Sum'
 
  nc <- cmatrix %>%
    mutate(NC = case_when(row_sum == 0 ~ 1,
                          row_sum == 1 ~ 0))  %>%
    dplyr::select("Name","NC") %>%
    table() %>%
    as.data.frame() %>%
    filter(NC == 1) %>%
    dplyr::select(Name, Freq) %>%
    # Ensure all classes are represented
    complete(Name = all_classes, fill = list(Freq = 0)) %>%
    deframe()

  # Reorder nc to match the order of all_classes (which is alphabetical)
  nc <- nc[all_classes]
 
  # Verify alignment (the order and names of classes should be the same)
  cat("Classes in conf_matrix:", paste(all_classes, collapse = ", "), "\n")
  cat("Classes in nc:         ", paste(names(nc), collapse = ", "), "\n")
 
  # Create a plot from this table:
  p <- ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "white") +
    scale_fill_gradient(low = "blue", high = "red", name = "Count") +
    theme_minimal() +
    labs(title = paste("Confusion Matrix"),
         x = "Actual class",
         y = "Predicted class")
 
  # Save as PNG with model-specific filename
  ggsave(
    filename = paste0("confusion_matrix_", model_name, ".png"),
    plot = p,
    device = "png",
    dpi = 300,
    width = 12,
    height = 5,
  )
 
  # Calculate overall accuracy
 
  # Convert conf_matrix to dataframe:
  matrix_df <- as.data.frame.matrix(conf_matrix)
  # Remove the column named sum
  matrix_df <- matrix_df %>% dplyr::select(-c(Sum))
  # Remove the row named sum  
  matrix_df <- matrix_df[rownames(matrix_df) != "Sum", ]
 
  # Convert matrix_df to a matrix/array (not the same as conf_matrix as it
  # is table/matrix/array)
  matrix <- as.matrix(matrix_df)

  # Now the matrix should be square (same number of rows and columns).
  # But if there are classes with zero predictions, they might still be missing.
  # Ensure the matrix is square by adding missing rows/columns if needed.
  
  # Get all classes that should be present
  all_classes <- colnames(conf_matrix)[colnames(conf_matrix) != "Sum"]
  
  # Check if matrix is square
  if (nrow(matrix) != ncol(matrix)) {
    cat("WARNING: Matrix is not square. Fixing...\n")
    cat("Rows:", rownames(matrix), "\n")
    cat("Cols:", colnames(matrix), "\n")
    
    # Create a full matrix with all classes
    full_matrix <- matrix(0, nrow = length(all_classes), ncol = length(all_classes))
    rownames(full_matrix) <- all_classes
    colnames(full_matrix) <- all_classes
    
    # Fill in existing values
    for (row_name in rownames(matrix)) {
      if (row_name %in% all_classes) {
        for (col_name in colnames(matrix)) {
          if (col_name %in% all_classes) {
            full_matrix[row_name, col_name] <- matrix[row_name, col_name]
          }
        }
      }
    }
    
    matrix <- full_matrix
    cat("Fixed matrix is now", nrow(matrix), "x", ncol(matrix), "\n")
  }

  # Get the diagonal
  diag_values <- diag(matrix)
  # Predicted cases per class (sums of rows) - use matrix
  rowsums <- apply(matrix, 1, sum)
  # Actual cases per class (sums of columns) - use matrix  
  colsums <- apply(matrix, 2, sum)
 
  # Instead of kappa, use quantity disagreement and allocation disagreement instead.
  classes <- all_classes  # Use all classes
 
  # Create a properly aligned nc vector
  nc_ordered <- nc[classes]  # This reorders nc to match classes order

 # Check if any nc values are NA (shouldn't happen if nc has all classes)
  if (any(is.na(nc_ordered))) {
    cat("WARNING: Some classes missing from nc. Filling with 0...\n")
    nc_ordered[is.na(nc_ordered)] <- 0
  }

  # Find and print the overall accuracy
  accuracy <- sum(diag_values) / (sum(matrix) + sum(nc_ordered))
  accuracy <- accuracy * 100
  cat("\nOverall Accuracy:", round(accuracy, 2), "\n")
  OA <- accuracy / 100
 
  # Check if diag_values has names, if not, assign them from classes
  if (is.null(names(diag_values))) {
    # If diag doesn't have names, we need to be careful
    # The matrix might have dropped a row/column
   
    # Get the row and column names
    row_names <- rownames(matrix)
    col_names <- colnames(matrix)
   
    # Find common classes between row and column names
    common_classes <- intersect(row_names, col_names)
   
    # Create a full diagonal vector with all classes
    full_diag <- rep(0, length(classes))
    names(full_diag) <- classes
   
    # Fill in the diagonal values for classes that exist
    for (class_name in common_classes) {
      if (class_name %in% row_names && class_name %in% col_names) {
        # Find the position of this class in the matrix
        row_idx <- which(row_names == class_name)
        col_idx <- which(col_names == class_name)
        full_diag[class_name] <- matrix[row_idx, col_idx]
      }
    }
   
    diag_values <- full_diag
  } else {
    # If diag has names, ensure all classes are represented
    full_diag <- rep(0, length(classes))
    names(full_diag) <- classes
   
    # Fill in existing diagonal values
    for (class_name in names(diag_values)) {
      if (class_name %in% classes) {
        full_diag[class_name] <- diag_values[class_name]
      }
    }
   
    diag_values <- full_diag
  }
 
  # Now ensure rowsums and colsums also have all classes
  full_rowsums <- rep(0, length(classes))
  names(full_rowsums) <- classes
 
  for (class_name in names(rowsums)) {
    if (class_name %in% classes) {
      full_rowsums[class_name] <- rowsums[class_name]
    }
  }
 
  full_colsums <- rep(0, length(classes))
  names(full_colsums) <- classes
 
  for (class_name in names(colsums)) {
    if (class_name %in% classes) {
      full_colsums[class_name] <- colsums[class_name]
    }
  }
 
  # Now calculate total_samples
  total_samples <- nrow(cmatrix2) + sum(nc_ordered)
 
  # Verify all lengths match
  if (length(classes) == length(diag_values) &&
      length(classes) == length(full_rowsums) &&
      length(classes) == length(full_colsums) &&
      length(classes) == length(nc_ordered)) {
   
    # Per-class precision, recall, and F-1 (handles division by zero)
    precision <- ifelse((full_rowsums + nc_ordered) > 0, 
                        diag_values / (full_rowsums + nc_ordered), 
                        0)
    recall <- ifelse((full_colsums + nc_ordered) > 0,
                     diag_values / (full_colsums + nc_ordered),
                     0)
    f1 <- ifelse((precision + recall) > 0,
                 2 * ((precision * recall) / (precision + recall)),
                 0)
   
    # Create a dataframe for these metrics
    prf1 <- data.frame(precision, recall, f1)
   
    # Add the classnames and the macro-averaged values
    prf1 <- prf1 %>%
      tibble::rownames_to_column(var = "Class") %>%
      as.data.frame() %>%
      add_row(Class = "Macro/Mean",
              precision = mean(.$precision, na.rm = TRUE),
              recall = mean(.$recall, na.rm = TRUE),
              f1 = mean(.$f1, na.rm = TRUE))
 
  # Create a dataframe containing true positives, false positives, and false negatives
  metrics_df <- data.frame(
    Class = classes,
    TP = diag_values,
    FP = full_rowsums - diag_values,
    FN = full_colsums - diag_values + nc_ordered
  )
 
  # Remove the extra rownames containing the class names:
  rownames(metrics_df) <- NULL
 
  # Quantity disagreement:
  QD <- abs(metrics_df$FN - metrics_df$FP) / total_samples
  overallQD <- (sum(QD)/2)
  QD <- c(QD, overallQD)
 
  # Allocation disagreement:
  AD <- 2 * pmin(metrics_df$FP / total_samples, metrics_df$FN / total_samples)
  overallAD <- (sum(AD)/2)
  AD <- c(AD, overallAD)
 
  # Create a dataframe containing all metrics
  all_metrics <- cbind(
    data.frame(modelName = model_name,
               overallAccuracy = OA,
               prf1,
               quantityDisagreement = QD,
               allocationDisagreement = AD)
  )
 
  #Change the name "Macro/Mean" to "Mean/Overall"
  all_metrics$Class[all_metrics$Class == "Macro/Mean"] <- "Mean/Overall"
 
  # For the overall row: list all zero-prediction classes
  overall_row <- all_metrics$Class == "Mean/Overall"
  if (length(zero_prediction_classes) > 0) {
    all_metrics$zero_prediction_classes[overall_row] <- paste(zero_prediction_classes, collapse = ", ")
    all_metrics$zero_prediction_count[overall_row] <- length(zero_prediction_classes)
  } else {
    all_metrics$zero_prediction_classes[overall_row] <- "None"
    all_metrics$zero_prediction_count[overall_row] <- 0
  }
 
  # For individual class rows, leave these columns blank
  all_metrics$zero_prediction_classes[!overall_row] <- NA
  all_metrics$zero_prediction_count[!overall_row] <- NA
 
  # Save metrics with model-specific filename
  metrics_filename <- paste0("accuracy_metrics_", model_name, ".csv")
  write.csv(all_metrics, metrics_filename, row.names = FALSE)
 
  } else {
    cat("ERROR: Dimension mismatch still exists!\n")
    cat("Lengths - classes:", length(classes),
        "diag_values:", length(diag_values),
        "full_rowsums:", length(full_rowsums),
        "full_colsums:", length(full_colsums),
        "nc_ordered:", length(nc_ordered), "\n")
  }
 
  # Restore original working directory before moving to next model
  setwd(old_wd)
 
  cat("\n✓ Successfully completed processing for model:", model_name, "\n")
}

cat("\n", rep("=", 50), "\n", sep = "")
cat("Loop completed! Saved the accuracy metrics of", sum(sapply(1:nrow(mod_det), function(i) {
  input_dir <- file.path(mod_det$directory[i], mod_det$model_name[i])
  processed_dir <- file.path(input_dir, "processed")
  dir.exists(processed_dir)
})), "out of", nrow(mod_det), "models when sufficient data was present")
cat("\n", rep("=", 50), "\n\n", sep = "")

# Close the sink (for the txt output)
sink()
