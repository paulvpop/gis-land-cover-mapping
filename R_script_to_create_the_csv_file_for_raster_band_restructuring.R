# Set the working directory (either copy paste and put it inside setwd (and inside
# quotes) OR Ctrl+Shift+H for interactively setting it)
setwd("D:/RS_GIS_files/force2/third")

# Load a necessary package
library(dplyr)

# Get the list of necessary folder names (assuming that there are only necessary
# folders with the folder name starting with dir_ML_RFC and no other files or
# folders with the same name). This is a case insensitive search which means that 
# any folder/file which has dir_ml_ and dir_ML_ and DIR_ML in its name will all be 
# found and added. 
list1 <- list.files(getwd(), pattern ='dir_ML_', full.names = TRUE, ignore.case = TRUE) %>% as.data.frame()

# Set the column name as 'directory' so that further manipulation is easier by
# calling the column name
colnames(list1) <- "model_name"

# Duplicate this column and then rename the new column as model_name
list1$directory <- list1$model_name

# Remove the file path and retain only the model name in the model_name column
# in a case insensitive manner (which means that any string before the string 
# dir_ml_ and dir_ML_ and DIR_ML will all be found and removed). Using perl=TRUE 
# for lookahead support (to not remove the string "dir_ML_")
list1$model_name <- gsub("^.*(?=dir_ML_)", "", list1$model_name, perl = TRUE, ignore.case = TRUE)

# Now in the column 'directory', remove one slash before dir_ML_RFC, the string
# dir_ML_ itself and the string after dir_ML_, so that only the directory
# path remains (in a case-insensitive manner)
list1$directory <- gsub("/dir_ML_.*", "", list1$directory, ignore.case = TRUE)

# Now you if you have a different folder with more models, you can open that 
# folder and list the files and merge it with the files above after repeating the
# previous steps. If not, you can start from the 'Merge list1 and list2...' step
# but rename list1 to litst (list <- list1)

# Set the working directory (either copy paste and put it inside setwd (and inside
# quotes) OR Ctrl+Shift+H for interactively setting it)
setwd("C:/Users/force/third")

# Get the list of necessary folder names
list2 <- list.files(getwd(), pattern ='dir_ML_', full.names = TRUE, ignore.case = TRUE) %>% as.data.frame()

# Set the column name as 'directory' so that further manipulation is easier by
# calling the column name
colnames(list2) <- "model_name"

# Duplicate this column and then rename the new column as model_name
list2$directory <- list2$model_name

# Remove the file path and retain only the model name in the model_name column
list2$model_name <- gsub("^.*(?=dir_ML_)", "", list2$model_name, perl = TRUE, ignore.case = TRUE)

# Now in the column 'directory', remove everything except the directory path
# path remains (in a case-insensitive manner)
list2$directory <- gsub("/dir_ML_.*", "", list2$directory, ignore.case = TRUE)

# Merge list1 and list2 (and add more if needed)
list <- rbind(list1,list2)

# Create a dataframe with serial numbers (the last number, which is 70 here should
# be the total number of models or model triplets you want to add to the csv file)
serial_df <- data.frame(sl_no = 1:80)

# Create an extended version of list with NA rows for the serial numbers
# which don't have any corresponding models/model triplets
list_extended <- list[1:nrow(serial_df), ]  # This automatically adds NAs

# Combine
list <- bind_cols(serial_df, list_extended)

# Remove the rownames of the combined dataframe (otherwise, it will have weird
# labels like NA.1, NA.2 etc)
row.names(list) <- NULL

# Add another column for adding missing classes
list[, 'missing_classes'] = NA

# Save as a csv file (change the directory before this if you want)
write.csv(list, "model_details_for_raster_restructuring.csv", row.names = FALSE)
