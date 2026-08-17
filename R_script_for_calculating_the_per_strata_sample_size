## Calculate the per strata sample size

# This can be done using the force-sample-size module available in FORCe.
# However, as a rule of thumb based on the Congalton and Green book:
# "Assessing the Accuracy of Remotely Sensed Data: Principles and Practices,
# 2nd edition", if the area is more than a million acres (i.e. > 10 lakh acres)
# or there are more than 12 classes, then use the method I have implemented
# completely in R ("Using R" subsection).

# If your study area is more than a million acres or have more than 12 classes,
# run the "Using R" section instead of the "Using FORCE" section.

# NOTE THAT THE EXAMPLE OUTPUTS OF THE CODE CAN BE FOUND IN THE ONLINE DOCUMENTATION OF
# THIS SECTION: https://github.com/paulvpop/gis-land-cover-mapping/blob/main/12.%20Accuracy%20assessment.md#calculate-the-per-strata-sample-size

### Using FORCE

#### Create inputs for the per strata sample size calculation in FORCE

# For estimating the sample size using a FORCE modules, one of the input files is
# a csv with the class name in one column and the number of pixels in another
# Since our focus is on all classes, the data from the All_classes scenarios for 
# both Landsat and Sentinel will be extracted from the all_satellite_summaries
# created in the the last stage (only in the R environment). 

# Extract the necessary data
pixels_sentinel <- as.data.frame(cbind(all_satellite_summaries[["Sentinel"]][["Class"]], 
                                       all_satellite_summaries[["Sentinel"]][["All_classes_avg"]]))

pixels_landsat <- as.data.frame(cbind(all_satellite_summaries[["Landsat"]][["Class"]], 
                                      all_satellite_summaries[["Landsat"]][["All_classes_avg"]]))

# Add the column names specified in the force-sample-size module
colnames(pixels_sentinel) <- c("class","count")
colnames(pixels_landsat) <- c("class","count")

# Write as csv
write.csv(pixels_sentinel, "pixel_count_sentinel.csv", row.names = FALSE)
write.csv(pixels_landsat, "pixel_count_landsat.csv", row.names = FALSE)

# Create dummy files for the user's accuracy for both Landsat and Sentinel
# It will contain a class column and a corresponding hypothesized/conjectured
# user's accuracy column. Read this paper - Olofsson et al, 2014 
# http://dx.doi.org/10.1016/j.rse.2014.02.015 to understand a bit more. 

# Extract the necessary data
UA_sentinel <- as.data.frame(cbind(all_satellite_summaries[["Sentinel"]][["Class"]], 
                                   " "))

UA_landsat <- as.data.frame(cbind(all_satellite_summaries[["Landsat"]][["Class"]], 
                                  " "))

# Add the column names specified in the force-sample-size module
colnames(UA_sentinel) <- c("class","UA")
colnames(UA_landsat) <- c("class","UA")

# Write as csv
write.csv(UA_sentinel, "UA_sentinel.csv", row.names = FALSE)
write.csv(UA_landsat, "UA_landsat.csv", row.names = FALSE)

# The potential user's accuracy can be entered for each class based on i) expert 
# prior knowledge based on experience on working on same or similar classes in the 
# same type or similar type of habitats/landscapes ii) based on published literature
# for the same iii) doing a trial run of the accuracy metrics with training data 
# (but it can provide false information as the data used for the estimation of 
# the accuracy metrics (user's accuracy in this case) is not independent from the 
# training data) or iv) a combination of the above

# If using iii), finish the "accuracy assessment" section, and then come back to this
# location. The following script will find and out the user's accuracy csv file
# file necessary for the force-sample-size module after averaging the precision 
# (which is the same as user's accuracy) for the models used in the pixel counting
# stage (or the statistics aggregation stage if the pixel counting was done in
# multiple systems). The rest of the section requires that the "accuracy assessment"
# has been done already.

# Set the working directory
setwd("D:/RS_GIS_files")
# OR use ctrl+shift+H

sent_all <- read.csv("LULC-SIANG/pixel_count_merged/Sentinel_All_classes_counts.csv")
land_all <- read.csv("LULC-SIANG/pixel_count_merged/Landsat_All_classes_counts.csv")

# Create a function to batch process all class files at once
process_all_classes_batch <- function() {
  
  # Define all classes
  all_classes <- c("AGR", "AJ", "BA", "BU", "CC", "MGR", "OF", "SH", "SN", "SS", "TF", "WA", "WRC")
  
  # Initialize results dataframes
  sentinel_results <- data.frame(class = all_classes, UA = NA)
  landsat_results <- data.frame(class = all_classes, UA = NA)
  
  # Process each class
  for (class_name in all_classes) {
    cat("\nProcessing class:", class_name, "\n")
    
    # Process Sentinel
    sentinel_file <- paste0("class_", class_name, "_Sentinel_metrics_final.csv")
    if (file.exists(sentinel_file)) {
      sentinel_metrics <- read.csv(sentinel_file)
      # Filter and calculate average
      filtered_sentinel <- sentinel_metrics %>%
        filter(modelName %in% sent_all$model_name)
      
      if (nrow(filtered_sentinel) > 0) {
        sentinel_results$UA[sentinel_results$class == class_name] <- 
          mean(filtered_sentinel$precision, na.rm = TRUE)
        cat("  Sentinel: Found", nrow(filtered_sentinel), "models\n")
      }
    }
    
    # Process Landsat
    landsat_file <- paste0("class_", class_name, "_Landsat_metrics_final.csv")
    if (file.exists(landsat_file)) {
      landsat_metrics <- read.csv(landsat_file)
      # Filter and calculate average
      filtered_landsat <- landsat_metrics %>%
        filter(modelName %in% land_all$model_name)
      
      if (nrow(filtered_landsat) > 0) {
        landsat_results$UA[landsat_results$class == class_name] <- 
          mean(filtered_landsat$precision, na.rm = TRUE)
        cat("  Landsat: Found", nrow(filtered_landsat), "models\n")
      }
    }
  }
  
  # Save results
  write.csv(sentinel_results, "UA_sentinel.csv", row.names = FALSE)
  write.csv(landsat_results, "UA_landsat.csv", row.names = FALSE)
  
  return(list(sentinel = sentinel_results, landsat = landsat_results))
}

# Check if required objects exist

if (!exists("sent_all")) {
  stop("ERROR: sent_all object not found. Please load sent_all data.")
}

if (!exists("land_all")) {
  stop("ERROR: land_all object not found. Please load land_all data.")
}

# Run the function
cat("Starting batch processing...\n")
results <- process_all_classes_batch()

# View the results
cat("\n\nFinal results stored in 'results' object:\n")
cat("Access with: results$sentinel or results$landsat\n")

results$sentinel
results$landsat

### Using R

# As the study area is composed of more than a million acres (10,674,943.042 acres 
# i.e. 4,32,00,000,000 m2 i.e. 4,32,00 km2), and the number of classes is more than 
# 12 (n = 13), as per the book  "Assessing the Accuracy of Remotely Sensed Data: Principles
# and Practices, 2nd Edn",  the sample size per class should be between 75 to 100. So, in 
# the following script, the minimum sample size per class will be kept as 75.

# This section has been adapted and modified from 
# https://github.com/davidfrantz/force/blob/main/rstats/force-sample-size.r

# For Landsat:

# Join the input tables containing pixel and user's accuracy data
table_landsat <- results$landsat %>% 
  inner_join(
    pixels_landsat,
    by = "class"
  )

# Check if join worked
if (nrow(table_landsat) != nrow(pixels_landsat)){
  exit_with_error("count and user_acc could not be joined")
}

# Make sure that the numbers are in the numeric form in the table:
table_landsat <- table_landsat %>% 
  mutate(UA = as.numeric(UA),
         count = as.numeric(count)
  )

# Compute proportional area, standard deviation (S.D), and prop. are X S.D of UA
table_landsat <- table_landsat %>%
  mutate(area = count / sum(count)) %>%
  mutate(stdev = sqrt(UA * (1 - UA))) %>%
  mutate(areaXstdev = area * stdev)

# Calculate the number of recommended samples 
samples_landsat <- (sum(table_landsat$areaXstdev) / 0.01)**2 %>%
  as.integer()

sprintf("Suggested sample size: %d\n", samples_landsat) %>%
  cat()
# Suggested sample size: 1068

# Compute class-wise sample size for equal and proportional allocation,
# and add an empty row for potential compromise between the two types.
table_landsat <- table_landsat %>%
  mutate(equal = round(samples_landsat / nrow(table_landsat))) %>%
  mutate(proportional = round(samples_landsat * area)) %>%
  mutate(compromise = NA)

# Check if there are enough samples in proportional allocation

# Assign a minimum sample size per class
min_size <- 75

if (min(table_landsat$proportional) < min_size) {
  
  cat("Proportional allocation yields too few samples.\n")
  cat("A compromise between equal and proportional allocation is recommended.\n")
  
  # First, assign minimum sample size to small classes
  rare <- table_landsat %>% 
    filter(proportional < min_size) %>% 
    mutate(compromise = min_size)
  
  n_rare <- sum(rare$compromise)
  
  # Find out if the classes which have greater than minimum sample size are at 
  # at least twice as big as the biggest small classes in the proportional
  # column
  
  # Biggest small class
  rare_max <- max(rare$proportional)
  
  # Create a column which shows the proportion of the count in proportional
  # in relation to rare_max
  table_landsat <- table_landsat %>%
    mutate(prop_rare_max = proportional/rare_max)
  
  # Calculate the per strata/class sample size with the following logic.
  # If greater than or equal to twice as big as biggest rare class (<=75) 
  # i.e. >= 2.0 in the prop_rare_max, then multiply the big class numbers
  # in the proportional column by 0.7 (taking 70%), but it should not fall 
  # below 75. Then add this number in the compromise column.
  
  # Create the compromise column calculation
  table_landsat <- table_landsat %>%
    mutate(
      compromise = case_when(
        # If prop_rare_max >= 2.0 (twice as big as biggest rare class)
        prop_rare_max >= 2.0 ~ {
          # Calculate 70% of the proportional value
          reduced_value = round(proportional * 0.7)
          
          # Ensure it doesn't fall below 75
          ifelse(reduced_value < 75, 75, reduced_value)
        },
        prop_rare_max < 2.0 ~ 75,
        # Otherwise, use the existing compromise value (75 for rare classes)
        TRUE ~ compromise
      )
    )
  
  # View the result
  print(table_landsat[, c("class", "equal", "proportional", "compromise")])
  
} else {
  cat("Proportional allocation recommended.\n")
}

# New sample size:
cat("Sample size estimated through a compromise approach:\n", sum(table_landsat$compromise))

# Compute deviation of compromised allocation from proportional in percent
table_landsat <- table_landsat %>%
  mutate(deviation = (compromise - proportional) / proportional * 100)

# Write the output
write.csv(
  table_landsat,
  "sample-size-landsat.csv",
  row.names = FALSE,
  quote = FALSE
)

# OPTIONAL: Note that we can do the same for Sentinel, but it will likely be unfeasible
# to collect the different sets of validation data for both Sentinel and Landsat.
# So, only data for Landsat (comparatively higher numbers - as you will see next) 
# need to be collected.

# For Sentinel:

# Join the input tables containing pixel and user's accuracy data
table_sentinel <- results$sentinel %>% 
  inner_join(
    pixels_sentinel,
    by = "class"
  )

# Check if the join worked
if (nrow(table_sentinel) != nrow(pixels_sentinel)){
  exit_with_error("count and user_acc could not be joined")
}

# Make sure that the numbers are in the numeric form in the table:
table_sentinel <- table_sentinel %>% 
  mutate(UA = as.numeric(UA),
         count = as.numeric(count)
  )

# Compute proportional area, standard deviation (S.D), and prop. are X S.D of UA
table_sentinel <- table_sentinel %>%
  mutate(area = count / sum(count)) %>%
  mutate(stdev = sqrt(UA * (1 - UA))) %>%
  mutate(areaXstdev = area * stdev)

# Calculate the number of recommended samples 
samples_sentinel <- (sum(table_sentinel$areaXstdev) / 0.01)**2 %>%
  as.integer()

sprintf("Suggested sample size: %d\n", samples_sentinel) %>%
  cat()

# Note that the higher resolution Sentinel imagery results in lesser number of
# validation data required than Landsat

# Compute class-wise sample size for equal and proportional allocation,
# and add an emoty row for potential compromise between the two types.
table_sentinel <- table_sentinel %>%
  mutate(equal = round(samples_sentinel / nrow(table_sentinel))) %>%
  mutate(proportional = round(samples_sentinel * area)) %>%
  mutate(compromise = NA)

# Check if there are enough samples in proportional allocation

# Assign a minimum sample size per class
min_size <- 75

if (min(table_sentinel$proportional) < min_size) {
  
  cat("Proportional allocation yields too few samples.\n")
  cat("A compromise between equal and proportional allocation is recommended.\n")
  
  # First, assign minimum sample size to small classes
  rare <- table_sentinel %>% 
    filter(proportional < min_size) %>% 
    mutate(compromise = min_size)
  
  n_rare <- sum(rare$compromise)
  # Find out if the classes which have greater than minimum sample size are 
  # at least twice as big as the biggest small classes in the proportional
  # column
  
  # Biggest small class
  rare_max <- max(rare$proportional)
  
  # Create a column which shows the proportion of the count in proportional
  # in relation to rare_max
  table_sentinel <- table_sentinel %>%
    mutate(prop_rare_max = proportional/rare_max)
  
  # Calculate the per strata/class sample size with the following logic.
  # If greater than or equal to twice as big as biggest rare class (<=75) 
  # i.e. >= 2.0 in the prop_rare_max, then multiply the big class numbers
  # in the proportional column by 0.7 (taking 70%), but it should not fall 
  # below 75. Then add this number in the compromise column.
  
  # Create the compromise column calculation
  table_sentinel <- table_sentinel %>%
    mutate(
      compromise = case_when(
        # If prop_rare_max >= 2.0 (twice as big as biggest rare class)
        prop_rare_max >= 2.0 ~ {
          # Calculate 70% of the proportional value
          reduced_value = round(proportional * 0.7)
          
          # Ensure it doesn't fall below 75
          ifelse(reduced_value < 75, 75, reduced_value)
        },
        prop_rare_max < 2.0 ~ 75,
        # Otherwise, use the existing compromise value (75 for rare classes)
        TRUE ~ compromise
      )
    )
  
  # View the result
  print(table_sentinel[, c("class", "equal", "proportional", "compromise")])
  
} else {
  cat("Proportional allocation recommended.\n")
}

# New sample size:
cat("Sample size estimated through a compromise approach:\n", sum(table_sentinel$compromise))

# Still lower than that for Landsat

# Compute deviation of compromised allocation from proportional in percent
table_sentinel <- table_sentinel %>%
  mutate(deviation = (compromise - proportional) / proportional * 100)

# Write the output
write.csv(
  table,
  "sample-size-sentinel.csv",
  row.names = FALSE,
  quote = FALSE
)
