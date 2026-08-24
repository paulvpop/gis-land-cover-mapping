## Ranking the classes

# The "top_models_landsat.csv" and "top_models_sentinel.csv" needs to have
# the row rankings changed so that the classes are ranked (for the purpose of
# integration). The same function used in the statistics aggregation step of 
# accuracy assessment cane be used here.

# For the workflow involving non-post-processed imagery (last section of the Post processing section), 
# run the script AFTER making the changes where the comments start with # Unchanged workflow

# NOTE THAT THE EXAMPLE OUTPUTS OF THE CODE CAN BE FOUND IN THE ONLINE DOCUMENTATION OF
# THIS SCRIPT: https://github.com/paulvpop/gis-land-cover-mapping/blob/main/13.%20Integration.md#ranking-the-classes

# Load required library (if not loaded)
library(dplyr)

# Define function to add rankings for each class
add_rankings <- function(data) {
  data %>%
    # Rankings for metrics where higher is better (rank 1 = highest value)
    mutate(rank_precision = rank(-precision, ties.method = "min"),
           rank_recall = rank(-recall, ties.method = "min"),
           rank_f1 = rank(-f1, ties.method = "min"),
           rank_quantityDisagreement = rank(-(1-quantityDisagreement), ties.method = "min"),
           rank_allocationDisagreement = rank(-(1-allocationDisagreement), ties.method = "min")) %>%
    # Priority-based integrated ranking:
    # 1st: allocationDisagreement (ascending - lower is better)
    # 2nd: f1 (descending - higher is better)
    # 3rd: quantityDisagreement (ascending - lower is better)
    arrange(allocationDisagreement, desc(f1), quantityDisagreement) %>%
    mutate(integrated_rank = row_number())
                            }

# Define the function to automatically find and process (the two) files instead of us individually
#  reading in the csv files
process_top_model_files <- function(root_dir = ".", exclude_combined = TRUE) {
  # Find all CSV files starting with "top_models_" recursively
  all_files <- list.files(root_dir, 
                          pattern = "^top_models_.*\\.csv$", 
                          recursive = TRUE, 
                          full.names = TRUE,
                          ignore.case = TRUE)
  
  # Filter out "top_models_combined.csv" 
  if (exclude_combined) {
    all_files <- all_files[!grepl("top_models_combined\\.csv$", all_files, ignore.case = TRUE)]
  }
  
  # Process each file
  results <- list()
  
  for (file_path in all_files) {
    cat("Processing:", file_path, "\n")
    
    # Read the file
    data <- read.csv(file_path)
    
    # Add rankings
    ranked_data <- add_rankings(data)
    
    # Create output filename
    base_name <- tools::file_path_sans_ext(file_path)
    output_path <- paste0(base_name, "_ranked.csv")
    
    # Save the ranked version
    write.csv(ranked_data, output_path, row.names = FALSE)
    
    # Store in results list for later use
    results[[file_path]] <- ranked_data
    
    cat("  -> Saved to:", output_path, "\n\n")
  }
  
  return(invisible(results))
}

# Run the processing starting from current directory
processed_data <- process_top_model_files()

# Alternatively, specify a different root directory
processed_data <- process_top_model_files("path/to/your/folder")
