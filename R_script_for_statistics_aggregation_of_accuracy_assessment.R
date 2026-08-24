## Statistics aggregation of accuracy assessment

# This section is for combining the rows of similar classes for every model and 
# output individual csv files for each class.

# NOTE THAT THE EXAMPLE OUTPUTS OF THE CODE CAN BE FOUND IN THE ONLINE DOCUMENTATION OF
# THIS SCRIPT: https://github.com/paulvpop/gis-land-cover-mapping/blob/main/12.%20Accuracy%20assessment.md#statistics-aggregation-of-accuracy-assessment

# For the workflow involving non-post-processed imagery (last section of the Post processing
# section), run the script AFTER making the changes where the comments start with # Unchanged workflow

# Set the broader working directory where you want the output of this section to be 
# saved.
setwd("D:/GIS")

# Load the necessary libraries
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

# The following step aggregates the data but removes those models where any
# of the classes has zero prediction pixels even though they should be in the 
# output. Then this file is saved.
valid_accuracy_data <- mod_det_fixed %>%
  mutate(file_path = file.path(directory, model_name, "processed",  # Unchanged workflow: change "processed" to "unchanged"
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
# Unchanged workflow: change "accuracy_data_for_all_valid_models.csv" to "accuracy_data_for_all_valid_unchanged_models.csv"

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
             dplyr::select(-c(zero_prediction_classes, zero_prediction_count))
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
      filename <- paste0("class_", safe_class, "_", safe_satellite, "_metrics.csv") # Unchanged workflow: change "_metrics.csv" to "_metrics_unchanged.csv"
      write.csv(ranked_data, filename, row.names = FALSE)
      
      cat("Created: ", filename, " with ", nrow(ranked_data), " models\n", sep = "")
    }
  })

# If there are more than one device in which the models have been saved, then,
# put them all under one folder. For example, accuracy_metrics_aggregation_linux.
# and accuracy_metrics_aggregation_windows (for a device running GNU/Linux and \
# Windows OS respectively), and then put these two folders under a single folder
# in one device.

# Define the main folder containing both system folders
main_folder <- "D:/GIS/LULC-SIANG"  # Replace with your actual path

# Define the system folders
system_folders <- c("accuracy_metrics_aggregation_linux", 
                    "accuracy_metrics_aggregation_windows")

# Unchanged workflow: change "accuracy_metrics_aggregation_linux" to "accuracy_metrics_aggregation_unchanged_linux"
# and change "accuracy_metrics_aggregation_windows" to "accuracy_metrics_aggregation_unchanged_windows"

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

# Extract base filenames to identify matching files within the multiple subfolders
# (system folders)
file_groups <- all_csv_files %>%
  tibble(file_path = .) %>%
  mutate(base_name = basename(file_path)) %>%
  group_by(base_name) %>%
  group_split()

# Function to add rankings for each class
add_rankings <- function(data) {
  data %>%
    # Remove old ranking columns (from previous calculations)
    dplyr::select(-any_of(c("rank_precision", "rank_recall", "rank_f1", "rank_quantityDisagreement", 
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
# Unchanged workflow: change "class_.*_metrics\\.csv$" to "class_.*_metrics_unchanged\\.csv$"
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
# Unchanged workflow: change "_metrics_final.csv" to "_metrics_unchanged_final.csv"
      
      write.csv(ranked_data, filename, row.names = FALSE)
      cat("Created: ", filename, " with ", nrow(ranked_data), " models\n", sep = "")
    }
  })
