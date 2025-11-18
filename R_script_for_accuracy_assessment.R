# This is the optimised workflow for accuracy assessment after post-processing (from the step '12. Accuracy assessment')

## Accuracy assessment

#Set broad working directory
setwd("D:/GIS/force")

#To start logging command outputs into a textfile "output.txt"
sink("accuracy_assessment_output.txt", split = TRUE)
print("This will appear in the console and be saved to output.txt")

# This accuracy assessment workflow will check if the class exists within a 30 m 
# radius buffer of training data points. This is to choose biologically meaningful 
# information for the accuracy assessment i.e. while machine learning models can
# displace the prediction pixels, if the displacement is within a distance that
# can be visibly seen by a human, instead of exactly at the point, then it can be 
# considered as accurately predicted/classified.

# We will be using the already loaded libraries terra, sf, and dplyr
# Load necessary package 'sp' (other than 'raster'), tidyverse, and ggplot2:
library(sp)
library(tidyverse)
library(ggplot2)

# As an input, prepare coordinates as spatial points with correct CRS (WGS84)
pts <- st_as_sf(as.data.frame(coordinates), coords = c("X", "Y"), crs = 4326)

# Define India NSF LCC (EPSG:7755)
india_nsf_lcc <- st_crs(7755)  # WGS 84 / India NSF LCC

# Ensure points are in India NSF LCC
pts_projected <- st_transform(pts, india_nsf_lcc)

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
    
    #List the classes outputted by the model
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
    final_results <- final_results %>% select (-c("row_sum","multi_class"))
    
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
    
    # Create confusion matrix table
    conf_matrix <- table(Predicted = predicted_classes, Actual = true_classes)
    
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
      select("Name","NC") %>%
      table() %>%
      as.data.frame() %>%
      filter(NC == 1) %>%
      select(Name, Freq) %>%
      # Ensure all classes are represented
      complete(Name = all_classes, fill = list(Freq = 0)) %>%
      deframe()
    
    # Verify alignment
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
    accuracy <- sum(diag(matrix)) / (sum(matrix) + sum(nc))
    accuracy <- accuracy * 100
    cat("\nOverall Accuracy:", round(accuracy, 2), "\n")
    OA <- accuracy / 100
    
    # Predicted cases per class (sums of rows) - use matrix
    rowsums <- apply(matrix, 1, sum)
    # Actual cases per class (sums of columns) - use matrix  
    colsums <- apply(matrix, 2, sum)
    
    # Per-class precision, recall, and F-1 (producer's accuracy is the same as recall
    # and user's accuracy is the same as precision)
    precision = diag(matrix) / (rowsums + nc) 
    recall = diag(matrix) / (colsums + nc)  
    f1 = 2 * ((precision * recall) / (precision + recall))
    
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
    
    # Instead of kappa, use quantity disagreement and allocation disagreement instead.
    classes <- colnames(conf_matrix)[-ncol(conf_matrix)]  # Exclude 'Sum'
    total_samples <- nrow(cmatrix2) + sum(nc)
    cat("Total samples:", total_samples, "\n")
    
    #Create a dataframe containing true positives, false positives & false negatives
    metrics <- data.frame(
      Class = classes,
      TP = diag(matrix),
      FP = rowsums - diag(matrix),
      FN = colsums - diag(matrix) + nc
    )
    
    # Remove the extra rownames containing the class names:
    rownames(metrics) <- NULL
    
    # Quantity disagreement:
    QD <- abs(metrics$FN - metrics$FP) / total_samples
    overallQD <- (sum(QD)/2)
    QD <- c(QD, overallQD)
    
    # Allocation disagreement:
    AD <- 2 * pmin(metrics$FP / total_samples, metrics$FN / total_samples)
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


#Close the sink (for the txt output)
sink()


## Statistics aggregation 


setwd("D:/GIS")

# Combine the rows of similar classes for every model and output individual csv files for each class: 

library(dplyr)
library(purrr)
library(readr)

# First, filter to only directories that exist and normalise the paths for existing ones
# using consistent separators (forward slashes)
mod_det_fixed <- mod_det %>%
  mutate(directory_exists = dir.exists(directory)) %>%
  filter(directory_exists) %>%
  mutate(directory = normalizePath(directory, winslash = "/"))

cat("Number of valid directories in the system:", nrow(mod_det_fixed), "\n")

valid_accuracy_data <- mod_det_fixed %>%
  mutate(file_path = file.path(directory, model_name, "processed", 
                               paste0("accuracy_metrics_", model_name, ".csv"))) %>%
  filter(file.exists(file_path)) %>%
  mutate(accuracy_data = map2(file_path, satellite, ~{
    df <- read.csv(.x)
    # Add satellite information to the accuracy data
    df$satellite <- .y
    # Check if Mean/Overall has zero_prediction_count = 0
    mean_row <- df %>% filter(Class == "Mean/Overall" & zero_prediction_count == 0)
    if (nrow(mean_row) > 0) df else NULL
  })) %>%
  pull(accuracy_data) %>%
  compact() %>%
  bind_rows()

write.csv(valid_accuracy_data, "accuracy_data_for_all_valid_models.csv")

# Function to add rankings for each class
add_rankings <- function(data) {
  data %>%
    # Rankings for metrics where higher is better (rank 1 = highest value)
    mutate(rank_precision = rank(-precision, ties.method = "min"),
           rank_recall = rank(-recall, ties.method = "min"),
           rank_f1 = rank(-f1, ties.method = "min"),
           rank_quantityDisagreement = rank(-(1-quantityDisagreement), ties.method = "min"),
           rank_allocationDisagreement = rank(-(1-allocationDisagreement), ties.method = "min")) %>%
           # Of all the accuracy metrics that are available, since the purpose of the exercise
           # is to get classification that reflects true class on the ground as well as 
           # getting having the right prediction of each class across space, recall and precision are 
           # important. So, their harmonic mean (f1) is used for the integrated rank. 
           # Also, allocation disagreement is more relevant than quantity disagreement since 
           # it is more important to know how many classes were correctly placed in the 
           # map as opposed to the comparison of the number of classified pixels in the 
           # reference map vs classified map (quantity disagreement). So, the weightage
           # for allocation disagreement is the highest in the integrated rank 
           # Weightage for f1 is higher than quantity disagreement 
           # Priority-based integrated ranking:
           # 1st: allocationDisagreement (ascending - lower is better)
           # 2nd: f1 (descending - higher is better)
           # 3rd: quantityDisagreement (ascending - lower is better)
           arrange(allocationDisagreement, desc(f1), quantityDisagreement) %>%
             mutate(integrated_rank = row_number()) %>%
             select(-c(zero_prediction_classes, zero_prediction_count))
                                 }

# Split by satellite AND class, add rankings, and write to files
valid_accuracy_data %>%
  group_by(satellite, Class) %>%
  group_split() %>%
  walk(~{
    group_data <- .x
    satellite_name <- unique(group_data$satellite)
    class_name <- unique(group_data$Class)
    
    if (!is.na(satellite_name) && !is.na(class_name)) {
      # Add rankings for this satellite-class combination
      ranked_data <- add_rankings(group_data)
      
      safe_satellite <- gsub("/", "_", satellite_name)
      safe_class <- gsub("/", "_", class_name)
      filename <- paste0("class_", safe_class, "_", safe_satellite, "_metrics.csv")
      write.csv(ranked_data, filename, row.names = FALSE)
      
      cat("Created: ", filename, " with ", nrow(ranked_data), " models\n", sep = "")
    }
  })

# If there are more than one device in which the models have been saved, then,
# put them all under one folder. For example, accuracy_metrics_aggregation_linux.
# and accuracy_metrics_aggregation_windows (for a device running GNU/Linux and \
# Windows OS respectively), and then put these two folders under a single folder
# in one device:

# Define the main folder containing both system folders
main_folder <- "path/to/your/main/folder"  # Replace with your actual path
To BE DELETED
main_folder <- "D:/Paul_Pop_RS_GIS_files/LULC-SIANG"

# Define the system folders
system_folders <- c("accuracy_metrics_aggregation_linux", 
                    "accuracy_metrics_aggregation_windows")

# Get all unique CSV files across both system folders
all_csv_files <- system_folders %>%
  map(~ {
    folder_path <- file.path(main_folder, .x)
    if (dir.exists(folder_path)) {
      list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
    } else {
      character(0)
    }
  }) %>%
  reduce(c)

# Extract base filenames to identify matching files
file_groups <- all_csv_files %>%
  tibble(file_path = .) %>%
  mutate(base_name = basename(file_path)) %>%
  group_by(base_name) %>%
  group_split()

# Function to add rankings for each class (updated version)
add_rankings <- function(data) {
  data %>%
    # Remove old ranking columns (from previous calculations)
    select(-any_of(c("rank_precision", "rank_recall", "rank_f1", "rank_quantityDisagreement", 
                     "rank_allocationDisagreement", "integrated_rank"))) %>%
    # Re-add the newly calculated rankings
    mutate(rank_precision = rank(-precision, ties.method = "min"),
           rank_recall = rank(-recall, ties.method = "min"),
           rank_f1 = rank(-f1, ties.method = "min"),
           rank_quantityDisagreement = rank(-(1-quantityDisagreement), ties.method = "min"),
           rank_allocationDisagreement = rank(-(1-allocationDisagreement), ties.method = "min")) %>%
          arrange(allocationDisagreement, desc(f1), quantityDisagreement) %>%
            mutate(integrated_rank = row_number())
                              }


# Read and combine all class-satellite files from both systems
all_data <- system_folders %>%
  map_dfr(~ {
    folder_path <- file.path(main_folder, .x)
    if (dir.exists(folder_path)) {
      # Get all class-satellite CSV files
      csv_files <- list.files(folder_path, pattern = "class_.*_metrics\\.csv$", full.names = TRUE)
      if (length(csv_files) > 0) {
        map_dfr(csv_files, ~ read.csv(.x))
      }
    }
  }) %>%
  # Remove duplicates based on key identifiers
  distinct(modelName, Class, satellite, .keep_all = TRUE)

# Process by satellite and class
all_data %>%
  group_by(satellite, Class) %>%
  group_split() %>%
  walk(~ {
    group_data <- .x
    satellite_name <- unique(group_data$satellite)
    class_name <- unique(group_data$Class)
    
    if (!is.na(satellite_name) && !is.na(class_name)) {
      # Add rankings for this satellite-class combination
      ranked_data <- add_rankings(group_data)
      
      safe_satellite <- gsub("/", "_", satellite_name)
      safe_class <- gsub("/", "_", class_name)
      filename <- paste0("class_", safe_class, "_", safe_satellite, "_metrics_final.csv")
      
      write.csv(ranked_data, filename, row.names = FALSE)
      cat("Created: ", filename, " with ", nrow(ranked_data), " models\n", sep = "")
    }
  })
