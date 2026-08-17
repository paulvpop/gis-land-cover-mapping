# Pixel statistics aggregation

# If the post-processed rasters are stored in two different systems, the data
# from both need to be combined.

# *IMPORTANT: Checklist of changes needed in the script for your personal use:*
# 1. Give your folder paths under "Define folder paths".
# 2. Under  # Define the 13 classes, modify the text to the classes you are using.
# 3. Under # Define recoding types, change the recording to match yours.
# 4. Under "Create satellite directories in output", change the "Sentinel","Landsat"
#   to the list of satellites you have.

# NOTE THAT THE EXAMPLE OUTPUTS OF THE CODE CAN BE FOUND IN THE ONLINE DOCUMENTATION OF
# THIS SCRIPT: https://github.com/paulvpop/gis-land-cover-mapping/blob/main/12.%20Accuracy%20assessment.md#pixel-statistics-aggregation

# Load required libraries
library(dplyr)
library(tidyr)

# Define folder paths
folder1 <- "D:/RS_GIS_files/LULC-SIANG/pixel_count_linux"
folder2 <- "D:/RS_GIS_files/LULC-SIANG/pixel_count_windows"
output_dir <- "pixel_count_merged"

# Create output directory
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Function to merge CSV files
merge_csv_files <- function(file1, file2, output_file) {
  # Read both files
  if (file.exists(file1)) {
    df1 <- read.csv(file1, stringsAsFactors = FALSE)
  } else {
    stop(paste("File not found:", file1))
  }
  
  if (file.exists(file2)) {
    df2 <- read.csv(file2, stringsAsFactors = FALSE)
  } else {
    stop(paste("File not found:", file2))
  }
  
  # Check if data frames have the same structure
  if (!identical(names(df1), names(df2))) {
    warning(paste("Column names differ for", basename(output_file), 
                  "- attempting to merge by common columns"))
    common_cols <- intersect(names(df1), names(df2))
    if (length(common_cols) == 0) {
      stop(paste("No common columns for", basename(output_file)))
    }
    df1 <- df1[, common_cols]
    df2 <- df2[, common_cols]
  }
  
  # Merge data frames
  merged_df <- bind_rows(df1, df2)
  
  # Remove duplicates (if any)
  merged_df <- distinct(merged_df)
  
  # Save merged file
  write.csv(merged_df, output_file, row.names = FALSE)
  
  return(nrow(merged_df))
}

# Get list of files from both folders
files1 <- list.files(folder1, pattern = "\\.csv$", full.names = FALSE)
files2 <- list.files(folder2, pattern = "\\.csv$", full.names = FALSE)

# Identify matching files (files with same name in both folders)
common_files <- intersect(files1, files2)
cat("Found", length(common_files), "common files to merge:\n")
print(common_files)

# Create satellite directories in output
satellite_dirs <- c("Sentinel", "Landsat")
for (dir in satellite_dirs) {
  dir_path <- file.path(output_dir, dir)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# Merge common files
merge_results <- data.frame(
  filename = character(),
  rows_folder1 = integer(),
  rows_folder2 = integer(),
  rows_merged = integer(),
  stringsAsFactors = FALSE
)

for (file in common_files) {
  file1 <- file.path(folder1, file)
  file2 <- file.path(folder2, file)
  output_file <- file.path(output_dir, file)

# Get row counts from individual files
rows1 <- nrow(read.csv(file1))
rows2 <- nrow(read.csv(file2))

# Merge files 
rows_merged <- merge_csv_files(file1, file2, output_file)
  
# Record results
merge_results <- rbind(merge_results, data.frame(
    filename = file,
    rows_folder1 = rows1,
    rows_folder2 = rows2,
    rows_merged = rows_merged
  ))
  
  cat("Merged:", file, "->", rows_merged, "rows\n")
}

# Create comprehensive summaries for each satellite
create_comprehensive_summary <- function(satellite, merged_dir) {
  cat("\nCreating comprehensive summary for:", satellite, "\n")
  
  # Define the 13 classes
  classes <- c("AGR", "AJ", "BA", "BU", "CC", "MGR", "OF", "SH", "SN", "SS", "TF", "WA", "WRC")
  
  # Define recoding types
  recoding_types <- c("BA_as_TF", "SH_as_TF", "BA.SH_as_TF", "All_classes")
  
  # Initialize comprehensive summary data frame
  comprehensive_summary <- data.frame(Class = classes, stringsAsFactors = FALSE)
  
  # Process each recoding type
  for (recoding_type in recoding_types) {
    # Look for counts file
    counts_file <- file.path(merged_dir, paste0(satellite, "_", recoding_type, "_counts.csv"))
    
    if (file.exists(counts_file)) {
      cat("  Processing:", recoding_type, "\n")
      
      # Read counts data
      counts_data <- read.csv(counts_file, stringsAsFactors = FALSE)
      
      # Calculate statistics for each class
      for (class in classes) {
        if (class %in% names(counts_data)) {
          # Get values for this class
          vals <- counts_data[[class]]
          vals <- vals[!is.na(vals) & vals > 0]
          
          if (length(vals) > 0) {
            # Remove outliers using IQR method
            Q <- quantile(vals, c(0.25, 0.75), na.rm = TRUE)
            bounds <- c(Q[1] - 1.5*(Q[2]-Q[1]), Q[2] + 1.5*(Q[2]-Q[1]))
            vals_clean <- vals[vals >= bounds[1] & vals <= bounds[2]]
            
            if (length(vals_clean) > 0) {
              # Calculate statistics
              avg_val <- mean(vals_clean)
              sd_val <- sd(vals_clean)
              n_val <- length(vals_clean)
              
              # Store in comprehensive summary
              comprehensive_summary[[paste0(recoding_type, "_avg")]][comprehensive_summary$Class == class] <- round(avg_val, 0)
              comprehensive_summary[[paste0(recoding_type, "_sd")]][comprehensive_summary$Class == class] <- round(sd_val, 0)
              comprehensive_summary[[paste0(recoding_type, "_n")]][comprehensive_summary$Class == class] <- n_val
            }
          }
        }
      }
    } else {
      cat("  Skipping:", recoding_type, "- file not found\n")
    }
  }
  
  # Calculate proportions for each recoding type
  for (recoding_type in recoding_types) {
    avg_col <- paste0(recoding_type, "_avg")
    prop_col <- paste0(recoding_type, "_prop")
    
    if (avg_col %in% names(comprehensive_summary)) {
      # Get average values for this recoding type
      avg_vals <- comprehensive_summary[[avg_col]]
      total <- sum(avg_vals, na.rm = TRUE)
      
      if (total > 0) {
        # Calculate proportions
        proportions <- (avg_vals / total) * 100
        comprehensive_summary[[prop_col]] <- proportions
      } else {
        comprehensive_summary[[prop_col]] <- NA
      }
    }
  }
  
  # Save comprehensive summary
  output_file <- file.path(merged_dir, satellite, paste0(satellite, "_comprehensive_summary.csv"))
  write.csv(comprehensive_summary, output_file, row.names = FALSE)
  
  cat("  Saved comprehensive summary to:", output_file, "\n")

  return(comprehensive_summary)
}

# Create comprehensive summaries for each satellite
all_satellite_summaries <- list()

for (satellite in satellite_dirs) {
  # Check if satellite has any data in merged directory
  satellite_files <- list.files(output_dir, pattern = paste0("^", satellite, "_"), full.names = FALSE)
  
  if (length(satellite_files) > 0) {
    summary <- create_comprehensive_summary(satellite, output_dir)
    all_satellite_summaries[[satellite]] <- summary
  } else {
    cat("\nNo data found for satellite:", satellite, "\n")
  }
}

# Create summary report
cat("\n", rep("=", 80), "\n", sep = "")
cat("MERGE COMPLETE\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Merge results:\n")
print(merge_results)

cat("\nFiles created in", output_dir, ":\n")
# Files created in pixel_count_merged :
files_created <- list.files(output_dir, recursive = TRUE)
for (file in files_created) {
  cat("  ", file, "\n", sep = "")
}

cat("\nSatellite directories created:\n")

# Satellite directories created:
for (satellite in satellite_dirs) {
  satellite_path <- file.path(output_dir, satellite)
  if (dir.exists(satellite_path)) {
    satellite_files <- list.files(satellite_path)
    cat("  ", satellite, " (", length(satellite_files), " files):\n", sep = "")
    for (file in satellite_files) {
      cat("    - ", file, "\n", sep = "")
    }
  }
}

# model_summary_by_satellite_scenario.csv to checked for an issue. 
