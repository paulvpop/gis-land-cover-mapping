# Pixel count and proportions

# Before accuracy assessment can be done, a set of validation data needs to be taken
# This will be done by stratified random sampling by using the classes of the 
# classified maps as the strata. So, there will be 13 strata in case of this study.
# As the paper "Good practices for estimating area and assessing accuracy of land change" 
# (https://samv.elearning.unipd.it/pluginfile.php/175898/mod_resource/content/0/articolo_oloffson.pdf)
# states "whatever the decision is to define  strata when multiple 
# maps are to be assessed, the sample reference data are still valid to assess any 
# of the maps, even if the strata are defined on the basis of a single map." So,
# instead of extracting the boundary of each strata by averaging the shape of the 
# all classes (using the post-processed rasters) and then using these average rasters 
# for the stratified random sampling approach, we can use an manually selected
# model for selecting the strata. Make sure that the output of this model has data 
# for all the strata (13 in this case) and has good accuracy on visual inspection.

# We should also make sure that the created points don't overlap the collected training data.

# *IMPORTANT: Checklist of changes needed in the script for your personal use:*
# 1) Set input directory (replace the folder path with your folder path).
# 2) If you don't have the mode_details.csv open in the R environment from the last section (11. Post processing),
#   then change the directory to the file in the script to load to csv file.
# 3) Define the classes in your models under the line # Define the classes.
# 4) When creating the output directory, rename it depending on whether you have the input processed files
#   of various models in various systems (under the line # Create output directory).
# 5) If you want to use the parallel processing function, which will be much faster but requires good
#   system specifications like higher RAM size and higher number of cores, uncommennt the line under # Process
#   models using PARALLEL processing and comment out the line under # Process models using SEQUENTIAL processing.
#   Sequential processing is kept as the default.

# WARNING!!: before running the workflow, adjust the number of cores in the step
# n_cores <- round(detectCores()*0.7) where 0.7 represent the usage of 70% of
# your system cores. Reduce this if your system can't handle it (or increase if
# your system is really good. But don't run other applications while running this
# workflow). If using the serial processing function, which is best for large
# areas/data and systems with not so great computational capability, you can
# ignore this warning.

# NOTE THAT THE EXAMPLE OUTPUTS OF THE CODE CAN BE FOUND IN THE ONLINE DOCUMENTATION OF
# THIS SCRIPT: https://github.com/paulvpop/gis-land-cover-mapping/blob/main/12.%20Accuracy%20assessment.md#pixel-count-and-proportions

# Set the working directory
setwd("D:/RS_GIS_files/LULC-SIANG")

# Load the csv containing model details (SKIP THIS STEP IF YOU ALREADY HAVE THE OBJECT 'mod_det' in the R environment 
# from the 'Post processing' section)
mod_det <- read.csv("D:/GIS/force/model_details.csv")

# Load required libraries
library(terra)
library(dplyr)
library(tidyr)
library(parallel)
library(doParallel)
library(foreach)

# Define the classes
classes <- c("AGR", "AJ", "BA", "BU", "CC", "MGR", "OF", "SH", "SN", "SS", "TF", "WA", "WRC")

# Create output directory
output_dir <- "pixel_count"

# Rename as pixel_count_Windows, pixel_count_linux etc. Depending on 
# whether you have the input processed files of various models
# in various systems. They can be combined at a later stage.
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Function for processing single model
process_single_model <- function(i, models_subset) {
  model_row <- models_subset[i, ]
  model_name <- model_row$model_name
  base_directory <- model_row$directory
  
  processed_dir <- file.path(base_directory, model_name, "processed")
  
  # Quick checks
  if (!dir.exists(processed_dir)) {
    return(list(model_name = model_name, status = "directory_not_found", data = NULL))
  }
  
  tif_files <- list.files(processed_dir, pattern = "_processed\\.tif$", full.names = TRUE)
  
  if (length(tif_files) < 11) {
    return(list(model_name = model_name, status = "insufficient_classes",
                data = NULL, num_classes = length(tif_files)))
  }
  
  # Process all rasters for this model
  class_data <- list()
  
  for (tif_file in tif_files) {
    class_name <- gsub("_processed\\.tif$", "", basename(tif_file))
    
    # Direct calculation of sum of raster = count of 1s
    r <- rast(tif_file)
    count_ones <- global(r, "sum", na.rm = TRUE)$sum
    
    if (count_ones == 0) {
      return(list(model_name = model_name, status = "zero_ones",
                  data = NULL, zero_class = class_name))
    }
    
    class_data[[class_name]] <- count_ones
  }
  
  # Create result data
  result_row <- list(
    model_name = model_name,
    num_classes = length(tif_files),
    satellite = as.character(model_row$satellite)
  )
  
  # Add class counts (fill with NA for missing classes)
  for (class in classes) {
    result_row[[class]] <- ifelse(is.null(class_data[[class]]), NA, class_data[[class]])
  }
  
  return(list(model_name = model_name, status = "success", data = result_row))
}

# Parallel processing function
process_models_parallel <- function(models_subset, scenario_name, n_cores = NULL) {
  
  cat("Processing", nrow(models_subset), "models for scenario:", scenario_name, "\n")
  
  # Determine number of cores
  if (is.null(n_cores)) {
    # CLOSE EVERYTHING ELSE WHILE RUNNING:
    n_cores <- round(detectCores()*0.7)  # Using 70% of the cores so that the system doesn't crash
    if (n_cores < 1) n_cores <- 1
  }
  
  cat("Using", n_cores, "CPU cores for parallel processing\n")
  
  # Set up parallel backend
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Export necessary functions and variables to workers
  clusterExport(cl, c("classes", "rast", "global", "process_single_model"),
                envir = environment())
  
  # Load terra package on each worker
  clusterEvalQ(cl, {
    library(terra)
  })
  
  # Process in parallel with proper combination
  results <- foreach(i = 1:nrow(models_subset),
                     .packages = c("terra"),
                     .combine = 'list',
                     .multicombine = TRUE,
                     .errorhandling = 'remove') %dopar% {
                       process_single_model(i, models_subset)
                     }
  
  # Stop cluster
  stopCluster(cl)
  
  # Process results
  all_results <- list()
  all_excluded <- list()
  
  for (result in results) {
    model_name <- result$model_name
    
    if (result$status == "success" && !is.null(result$data)) {
      all_results[[length(all_results) + 1]] <- result$data
    } else {
      # Track exclusion reason
      if (result$status == "directory_not_found") {
        all_excluded[[model_name]] <- "Directory not found"
      } else if (result$status == "insufficient_classes") {
        all_excluded[[model_name]] <- paste("Only", result$num_classes, "classes")
      } else if (result$status == "zero_ones") {
        all_excluded[[model_name]] <- paste("Zero 1s in", result$zero_class)
      } else {
        all_excluded[[model_name]] <- result$status
      }
    }
  }
  
  # Combine all results into dataframe
  if (length(all_results) > 0) {
    # Convert list of lists to dataframe
    result_df <- do.call(rbind, lapply(all_results, function(x) {
      data.frame(x, stringsAsFactors = FALSE)
    }))
    
    return(list(
      counts = result_df,
      excluded = all_excluded
    ))
  } else {
    return(list(
      counts = NULL,
      excluded = all_excluded
    ))
  }
}

# Sequential processing function
process_models_sequential <- function(models, scenario_name) {
  cat("Processing", nrow(models), "models sequentially for scenario:", scenario_name, "\n")
  
  all_results <- list()
  excluded <- list()
  
  for (i in 1:nrow(models)) {
    cat("  Processing", i, "/", nrow(models), ":", models$model_name[i], "... ")
    
    result <- process_single_model(i, models)
    
    if (result$status == "success") {
      all_results[[models$model_name[i]]] <- result$data
      cat("SUCCESS\n")
    } else {
      reason <- result$status
      if (result$status == "insufficient_classes" && !is.null(result$num_classes)) {
        reason <- paste0(reason, " (found ", result$num_classes, " files)")
      } else if (result$status == "zero_ones" && !is.null(result$zero_class)) {
        reason <- paste0(reason, " (", result$zero_class, " has zero pixels)")
      } else if (result$status == "directory_not_found") {
        reason <- "processed folder not found"
      }
      excluded[[models$model_name[i]]] <- reason
      cat("FAILED:", reason, "\n")
    }
  }
  
  cat("\nSummary: Successfully processed", length(all_results), "models\n")
  cat("Excluded:", length(excluded), "models\n")
  
  if (length(all_results) == 0) {
    return(list(counts = NULL, excluded = excluded))
  }
  
  # Convert results to data frame
  counts_df <- do.call(rbind, lapply(names(all_results), function(model_name) {
    row_data <- all_results[[model_name]]
    row_df <- as.data.frame(t(unlist(row_data)), stringsAsFactors = FALSE)
    return(row_df)
  }))
  
  return(list(counts = counts_df, excluded = excluded))
}

# Get unique satellites from the data
satellites <- unique(mod_det$satellite)
cat("Satellites found:", paste(satellites, collapse = ", "), "\n")
# Satellites found: Sentinel, Landsat 

# Create nested scenarios: First by satellite, then by recoding scenario
all_scenarios <- list()

for (satellite in satellites) {
  # Filter models for this satellite
  satellite_models <- mod_det %>% filter(satellite == !!satellite)
  
  if (nrow(satellite_models) == 0) next
  
  # Create named list elements for each satellite scenario
  all_scenarios[[paste(satellite, "BA_as_TF", sep = "_")]] <-
    satellite_models %>% filter(BA_as_TF == 1)
  
  all_scenarios[[paste(satellite, "SH_as_TF", sep = "_")]] <-
    satellite_models %>% filter(SH_as_TF == 1)
  
  all_scenarios[[paste(satellite, "BA.SH_as_TF", sep = "_")]] <-
    satellite_models %>% filter(BA.SH_as_TF == 1)
  
  all_scenarios[[paste(satellite, "All_classes", sep = "_")]] <-
    satellite_models %>% filter(BA_as_TF == 0 & SH_as_TF == 0 & BA.SH_as_TF == 0)
}

# Remove empty scenarios
all_scenarios <- all_scenarios[sapply(all_scenarios, nrow) > 0]

cat("\nTotal scenarios to process:", length(all_scenarios), "\n")
# Total scenarios to process: 8 
for (scenario_name in names(all_scenarios)) {
  cat("  ", scenario_name, ": ", nrow(all_scenarios[[scenario_name]]), " models\n", sep = "")
}

# Main processing loop (When finishing parallel processing function, 
# finished in ~4 minutes with 22 cores (and 32 GB RAM), and 
# ~6 minutes with 11 cores (64 GB RAM))
all_results <- list()
summary_stats <- list()

for (scenario_name in names(all_scenarios)) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Processing:", scenario_name, "\n")
  cat(rep("=", 60), "\n\n", sep = "")
  
  models <- all_scenarios[[scenario_name]]
  
  if (nrow(models) == 0) {
    cat("No models in this scenario\n")
    next
  }
  
  cat("Models to process:", nrow(models), "\n")
  
  # Process models using PARALLEL processing
  #result <- process_models_parallel(models, scenario_name)
  
  # Process models using SEQUENTIAL processing
  result <- process_models_sequential(models, scenario_name)
  
  if (!is.null(result$counts) && nrow(result$counts) > 0) {
    # Save raw counts
    write.csv(result$counts,
              file.path(output_dir, paste0(scenario_name, "_counts.csv")),
              row.names = FALSE)
    
    # Calculate averages (excluding outliers) with error handling
    avg_data <- data.frame()
    
    for (class in classes) {
      tryCatch({
        if (class %in% names(result$counts)) {
          #vals <- result$counts[[class]]
          vals <- as.numeric(result$counts[[class]])  # Convert to numeric
          vals <- vals[!is.na(vals) & vals > 0]
          
            # Remove outliers using IQR method with error handling
            if (length(vals) >= 4) {
              Q <- tryCatch({
                quantile(vals, c(0.25, 0.75), na.rm = TRUE)
              }, error = function(e) {
                return(c(NA, NA))
              })
              
              if (!is.na(Q[1]) && !is.na(Q[2]) && Q[1] < Q[2]) {
                iqr <- Q[2] - Q[1]
                bounds <- c(Q[1] - 1.5 * iqr, Q[2] + 1.5 * iqr)
                vals_clean <- vals[vals >= bounds[1] & vals <= bounds[2]]
              } else {
                vals_clean <- vals
              }
            } else {
              vals_clean <- vals
            }
            
            if (length(vals_clean) > 0) {
              avg_data <- rbind(avg_data, data.frame(
                class = class,
                avg = mean(vals_clean, na.rm = TRUE),
                sd = sd(vals_clean, na.rm = TRUE),
                n = length(vals_clean)
              ))
          }
        }
      }, error = function(e) {
        cat("  Warning: Error processing class", class, "-", e$message, "\n")
      })
    }
    
    # Calculate proportions
    if (nrow(avg_data) > 0) {
      total <- sum(avg_data$avg, na.rm = TRUE)
      
      if (total > 0) {
        avg_data$proportion <- (avg_data$avg / total) * 100
      } else {
        avg_data$proportion <- 0
      }
      
      summary_stats[[scenario_name]] <- list(
        counts = result$counts,
        averages = avg_data,
        total = total,
        n_models = nrow(result$counts)
      )
      
      # cat("Included models:", nrow(result$counts), "\n")
      # cat("Excluded models:", length(result$excluded), "\n")
      
      cat("\n", strrep("-", 40), "\n", sep = "")
      cat("Scenario:", scenario_name, "\n")
      cat("  Included models:", nrow(result$counts), "\n")
      cat("  Excluded models:", length(result$excluded), "\n")
      cat("  Total pixels:", format(total, big.mark = ","), "\n")
      
      # Show exclusion reasons if any
      if (length(result$excluded) > 0) {
        cat("\nExclusion reasons:\n")
        excl_reasons <- result$excluded
        for (model in names(excl_reasons)) {
          cat(sprintf("  %-30s: %s\n", model, excl_reasons[[model]]))
        }
        
        # Save exclusion reasons as a csv
        exclusion_df <- data.frame(
          model_name = names(result$excluded),
          reason = unlist(result$excluded)
        )
        write.csv(exclusion_df,
                  file.path(output_dir, paste0(scenario_name, "_exclusion_reasons.csv")),
                  row.names = FALSE)
      }

      # Show all classes by proportion
      if (nrow(avg_data) > 0) {
        cat("\nProportion (in %) of all classes for the scenario:\n")
        # print(avg_data[order(-avg_data$proportion), c("class", "proportion")])
        avg_data_sorted <- avg_data[order(-avg_data$proportion), ]
        print(avg_data_sorted[, c("class", "proportion", "avg", "sd", "n")])
      }
    }
  } else {
    cat("No valid models found\n")
    if (length(result$excluded) > 0) {
      cat("Excluded models:", length(result$excluded), "\n")
      cat("Exclusion reasons:\n")
      excl_reasons <- result$excluded
      for (model in names(excl_reasons)) {
        cat(sprintf("  %-30s: %s\n", model, excl_reasons[[model]]))
      }
    }
  }
}

cat("PROCESSING COMPLETE\n")

# Create satellite-specific analysis
if (length(summary_stats) > 0) {
  # Process each satellite separately
  for (satellite in satellites) {
    # Find scenarios for this satellite
    satellite_scenarios <- names(summary_stats)[grepl(paste0("^", satellite, "_"), names(summary_stats))]
    
    if (length(satellite_scenarios) > 0) {
      cat("\n", rep("*", 60), "\n", sep = "")
      cat("Creating satellite summary for:", satellite, "\n")
      cat(rep("*", 60), "\n\n", sep = "")
      
      # Create satellite directory
      satellite_dir <- file.path(output_dir, satellite)
      if (!dir.exists(satellite_dir)) dir.create(satellite_dir, recursive = TRUE)
      
      # Create comprehensive satellite summary
      satellite_summary <- data.frame(Class = classes)
      
      for (scenario in satellite_scenarios) {
        scenario_clean <- gsub(paste0(satellite, "_"), "", scenario)
        satellite_summary[[paste0(scenario_clean, "_avg")]] <- NA
        satellite_summary[[paste0(scenario_clean, "_sd")]] <- NA
        satellite_summary[[paste0(scenario_clean, "_n")]] <- NA
        satellite_summary[[paste0(scenario_clean, "_prop")]] <- NA
        
        for (i in 1:nrow(satellite_summary)) {
          class <- satellite_summary$Class[i]
          if (class %in% summary_stats[[scenario]]$averages$class) {
            avg_row <- summary_stats[[scenario]]$averages[summary_stats[[scenario]]$averages$class == class, ]
            satellite_summary[[paste0(scenario_clean, "_avg")]][i] <- round(avg_row$avg, 0)
            satellite_summary[[paste0(scenario_clean, "_sd")]][i] <- round(avg_row$sd, 0)
            satellite_summary[[paste0(scenario_clean, "_n")]][i] <- avg_row$n
            satellite_summary[[paste0(scenario_clean, "_prop")]][i] <- avg_row$proportion
            # Not rounding off the proportions so that the total remains 100%
          }
        }
      }
      
      # Save satellite summary
      write.csv(satellite_summary,
                file.path(satellite_dir, paste0(satellite, "_comprehensive_summary.csv")),
                row.names = FALSE)
     
      cat("  Saved satellite summaries to:", satellite_dir, "\n")
    }
  }

# Create overall model summary
model_summary <- data.frame(
    Satellite = sapply(names(all_scenarios), function(x) strsplit(x, "_")[[1]][1]),
    Recoding_Type = sapply(names(all_scenarios), function(x) {
      parts <- strsplit(x, "_")[[1]]
      paste(parts[-1], collapse = "_")
    }),
    Total_Models = sapply(all_scenarios, nrow),
    Included_Models = sapply(names(all_scenarios), function(x) {
      if (x %in% names(summary_stats)) summary_stats[[x]]$n_models else 0
    })
  )
  
  model_summary$Excluded_Models <- model_summary$Total_Models - model_summary$Included_Models
  
  write.csv(model_summary,
            file.path(output_dir, "model_summary_by_satellite_scenario.csv"),
            row.names = FALSE)
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("PROCESSING COMPLETE\n")
  cat(rep("=", 80), "\n\n", sep = "")
  
  cat("Summary of scenarios processed:\n")
  print(model_summary)
  
  cat("\nFiles saved in:", output_dir, "\n")
  cat("Main directory files:\n")
  cat("  - [satellite_scenario]_counts.csv: Raw pixel counts for each model\n")
  cat("  - [satellite_scenario]_exclusion_reasons.csv: Reasons for model exclusion\n")
  cat("  - model_summary_by_satellite_scenario.csv: Model counts summary\n")
  
  cat("\nSatellite-specific directories (with separate summaries):\n")
  for (satellite in satellites) {
    satellite_dir <- file.path(output_dir, satellite)
    if (dir.exists(satellite_dir)) {
      cat("  ", satellite, " directory contains:\n", sep = "")
      cat("    - ", satellite, "_comprehensive_summary.csv: All statistics\n", sep = "")
    }
  }
}  else {
  cat("\nNo valid models found in any scenario\n")
}
