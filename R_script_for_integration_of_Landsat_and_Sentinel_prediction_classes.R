## Integration of Landsat and Sentinel prediction classes

# The following workflow can be used for created the integrated final maps for both the 
# Sentinel and Landsat outputs.

# NOTE THAT THE EXAMPLE OUTPUTS OF THE CODE CAN BE FOUND IN THE ONLINE DOCUMENTATION OF
# THIS SECTION: https://github.com/paulvpop/gis-land-cover-mapping/blob/main/13.%20Integration.md#integration-of-landsat-and-sentinel-prediction-classes

# Scenario 1 for necessary input for the integration:
# From the object 'processed_data' (a list) from the earlier step, create an object
# called 'data_by_type'. The processed_data is a list where each element is a ranked
# data frame. The names will be the file paths, so we need to extract the correct ones

# First, identify which file corresponds to Landsat and which to Sentinel
find_data_by_type <- function(processed_data_list) {
  result <- list()
  
  for (name in names(processed_data_list)) {
    # Check if it's Landsat data
    if (grepl("landsat", basename(name), ignore.case = TRUE)) {
      result[["landsat"]] <- processed_data_list[[name]]
    }
    # Check if it's Sentinel data
    else if (grepl("sentinel", basename(name), ignore.case = TRUE)) {
      result[["sentinel"]] <- processed_data_list[[name]]
    }
  }
  
  return(result)
}

# Extract Landsat and Sentinel data from processed_data
data_by_type <- find_data_by_type(processed_data)

# Scenario 2 for necessary input (ONLY DO THIS IF YOUR SCENARIO DOESN'T FIT SCENARIO 1):
# If you don't have the processed_data list from earlier run (accidentally deleted
# or lost or starting a new sesssion, load the ranked CSV files.

# Load the CSV files directly (if you've already run the ranking)
create_data_by_type_from_files <- function() {
  data_by_type <- list()
  
  # Look for the ranked CSV files
  # Check in current directory first
  landsat_file <- list.files(pattern = "top_models.*landsat.*ranked\\.csv$", 
                             ignore.case = TRUE, full.names = TRUE)
  sentinel_file <- list.files(pattern = "top_models.*sentinel.*ranked\\.csv$", 
                              ignore.case = TRUE, full.names = TRUE)
  
  # If not found, check in Top_Models_Organized directory
  if (length(landsat_file) == 0) {
    landsat_file <- "Top_Models_Organized/top_models_landsat_ranked.csv"
  }
  if (length(sentinel_file) == 0) {
    sentinel_file <- "Top_Models_Organized/top_models_sentinel_ranked.csv"
  }
  
  # Load Landsat data
  if (file.exists(landsat_file[1])) {
    data_by_type[["landsat"]] <- read.csv(landsat_file[1])
    cat("Loaded Landsat from:", landsat_file[1], "\n")
  } else {
    warning("Landsat ranked CSV not found at:", landsat_file[1])
  }
  
  # Load Sentinel data
  if (file.exists(sentinel_file[1])) {
    data_by_type[["sentinel"]] <- read.csv(sentinel_file[1])
    cat("Loaded Sentinel from:", sentinel_file[1], "\n")
  } else {
    warning("Sentinel ranked CSV not found at:", sentinel_file[1])
  }
  
  return(data_by_type)
}

# Create data_by_type from files
data_by_type <- create_data_by_type_from_files()

# Define the function to create the ranked composite
create_ranked_composite <- function(ranked_data, data_type = "Landsat", 
                                    base_dir = "Top_Models_Organized") {
  
  # Use terra instead of raster (more modern and handles paths better)
  library(terra)
  library(dplyr)
  
  # Fix Windows paths
  fix_path <- function(path) {
    path <- gsub("\\\\", "/", path)
    # Ensure proper drive letter format
    if (grepl("^[A-Za-z]:/", path)) {
      # Already has drive letter with forward slash
    } else if (grepl("^[A-Za-z]:\\\\", path)) {
      path <- gsub("\\\\", "/", path)
    }
    return(path)
  }
  
  # Get the directory path based on data type
  raster_dir <- file.path(base_dir, data_type)
  raster_dir <- fix_path(raster_dir)
  
  # Check if directory exists
  if (!dir.exists(raster_dir)) {
    stop(paste("Directory not found:", raster_dir))
  }
  
  cat("Looking for rasters in:", raster_dir, "\n")
  
  # List all TIFF files for debugging
  all_tifs <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)
  cat("Found", length(all_tifs), "TIFF files\n")
  # if (length(all_tifs) > 0) {
  #   cat("First 3 files:", paste(basename(head(all_tifs, 3)), collapse = ", "), "\n")
  # }
  
  # Class to value mapping (from Stage 7)
  class_to_value <- c(
    "TF" = 1,
    "SN" = 2,
    "OF" = 3,
    "BA" = 4,
    "CC" = 5,
    "AJ" = 6,
    "WRC" = 7,
    "SH" = 8,
    "BU" = 9,
    "AGR" = 10,
    "MGR" = 11,
    "WA" = 12,
    "SS" = 13)
  
  # Get the correct ranking from the processed data
  ranking_order <- ranked_data %>%
    arrange(desc(integrated_rank)) %>%
    distinct(Class, .keep_all = TRUE) %>% 
    pull(Class)
  
  cat("\nRanking order (from bottom/lower accuracy to top/higher accuracy):\n")
  print(ranking_order)
  
  # Color palette
  color_palette <- c(
    "SH" = "#0D0D0D",    # Black
    "BU" = "#800000",    # Maroon
    "SS" = "#EBD9B0",    # Pale brown
    "TF" = "#3C6301",    # Dark green
    "WA" = "#42D3F2",    # Blue
    "WRC" = "#00FF00",   # Light green
    "BA" = "#FFFF00",    # Yellow
    "CC" = "#808000",    # Olive
    "AJ" = "#BD8D42",    # Burnt earth
    "MGR" = "#DCF046",   # Yellowish green
    "AGR" = "#9CCC65",   # Mint green
    "OF" = "#008000",    # Medium Green
    "SN" = "#F4F4F5"     # White
  )
  
  # Initialize composite raster
  composite_raster <- NULL
  template_raster <- NULL
  
  # Process each class in ranking order
  for (priority in seq_along(ranking_order)) {
    class_name <- ranking_order[priority]
    class_value <- class_to_value[class_name]
    
    cat(paste("\nProcessing class", class_name, "with value", class_value, "..."))
    
    # Find the corresponding TIFF file with flexible naming
    tif_files <- NULL
    
    # Provide the file pattern(s). Change it if different in your case.
    patterns <- c(
      paste0("^", class_name, "_top_model\\.tif$")
    )
    
    for (pattern in patterns) {
      tif_files <- list.files(raster_dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
      if (length(tif_files) > 0) break
    }
    
    if (length(tif_files) == 0) {
      cat(" ❌ No TIFF file found\n")
      next
    }
    
    tif_file <- fix_path(tif_files[1])
    cat(" Found:", basename(tif_file))
    
    # Read the raster using terra (more robust than raster)
    tryCatch({
      r <- rast(tif_file)
      
      # Create binary mask with class value
      # For terra, we need to handle the values differently
      if (is.null(template_raster)) {
        # Create template from first raster
        template_raster <- r
        composite_raster <- rast(template_raster)
        # Initialize with NA
        values(composite_raster) <- NA
      }
      
      # Get values for this raster
      vals <- values(r, mat = FALSE)
      
      # Create mask: where class exists (value == 1)
      mask <- !is.na(vals) & vals == 1
      
      # Get current composite values
      current_vals <- values(composite_raster, mat = FALSE)
      
      # Update: where mask is TRUE and current is NA, set to class value
      update_idx <- mask & is.na(current_vals)
      current_vals[update_idx] <- class_value
      
      # Write back to composite
      values(composite_raster) <- current_vals
      
      cat(" ✅ Added to the composite\n")     
    }, error = function(e) {
      cat(" ❌ Error:", e$message, "\n")
    })
  }
  
  # Check if we have any valid data
  if (is.null(composite_raster)) {
    stop("No valid rasters found to create composite")
  }
  
  # Create value-to-class mapping
  value_to_class <- data.frame(
    value = class_to_value,
    class = names(class_to_value),
    color = sapply(names(class_to_value), function(cn) {
      if (cn %in% names(color_palette)) color_palette[cn] else "#808080"
    })
  )
  value_to_class <- value_to_class[order(value_to_class$value), ]
  rownames(value_to_class) <- NULL
  
  # Return results
  return(list(
    raster = composite_raster,
    value_to_class = value_to_class,
    colors = setNames(value_to_class$color, value_to_class$class),
    order = ranking_order,
    n_classes = length(ranking_order),
    class_mapping = class_to_value
  ))
}

# Now create the integrated map for Landsat and Sentinel.

# Create the integrated map for Landsat.
cat("\n=== Creating Landsat composite raster ===\n")

# Check if data_by_type exists and has landsat data
if (exists("data_by_type") && "landsat" %in% names(data_by_type)) {
  
  # Create the composite using terra
  landsat_result <- create_ranked_composite(
    data_by_type[["landsat"]], 
    "Landsat", 
    "Top_Models_Organized"
  )
  
  # Save the composite
  if (!is.null(landsat_result$raster)) {
    output_file <- "Top_Models_Organized/Landsat_ranked_composite.tif"
    
    # Write using terra
    writeRaster(
      landsat_result$raster,
      filename = output_file,
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "BIGTIFF=YES", "TILED=YES"),
      datatype = "INT2U"
    )
    
    cat("\n✅ Saved Landsat composite to:", output_file, "\n")
    
    # Also save the mapping
    write.csv(
      landsat_result$value_to_class,
      "Top_Models_Organized/Landsat_value_to_class.csv",
      row.names = FALSE
    )
    cat("✅ Saved value-to-class mapping\n")
    
    # Print summary
    cat("\nValue-to-Class Mapping:\n")
    print(landsat_result$value_to_class)
  }
}

# Also create the integrated map for Sentinel
cat("\n=== Creating Sentinel composite raster ===\n")

if (exists("data_by_type") && "sentinel" %in% names(data_by_type)) {
  
  sentinel_result <- create_ranked_composite(
    data_by_type[["sentinel"]], 
    "Sentinel", 
    "Top_Models_Organized"
  )
  
  if (!is.null(sentinel_result$raster)) {
    output_file <- "Top_Models_Organized/Sentinel_ranked_composite2.tif"
    
    writeRaster(
      sentinel_result$raster,
      filename = output_file,
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW", "BIGTIFF=YES", "TILED=YES"),
      datatype = "INT2U"
    )
    
    cat("\n✅ Saved Sentinel composite to:", output_file, "\n")
    
    write.csv(
      sentinel_result$value_to_class,
      "Top_Models_Organized/Sentinel_value_to_class.csv",
      row.names = FALSE
    )
    
    cat("\nValue-to-Class Mapping:\n")
    print(sentinel_result$value_to_class)
  }
  
} else {
  cat("❌ No Sentinel data found\n")
}
```
