> Accuracy assesment is a very important part of any land cover mapping
> exercise, as models can provide completely to slightly wrong
> predictions of the each class. We should always aim for the latter as
> it is near impossible to get 100% accurate modelled land cover maps
> (at least at this point of time in the 21st century; this may change
> with advancement in tools and techniques).

The metrics that is used for the accuracy assessment should be well thought of i.e. they should logically coherent and consistent. Indiscriminate ignorant use of metrics like kappa (which actually do not measure accuracy) in land cover mapping research has resulted in subpar assessment of accuracies in a large number of papers, even those published in top reputed journals like *Nature*.

The following R script can be used for accuracy assessment of those land cover maps produced in the last section (after post-processing).  

<br>

#### Table of contents

 * [Extraction of the class from the rasters using the training data](#extraction-of-the-class-from-the-rasters-using-the-training-data)
  * [Create and save confusion matrix](#create-and-save-confusion-matrix)
  * [Calculate overall accuracy](#calculate-overall-accuracy)
  * [Calculate and save producer and consumer accuracy](#calculate-and-save-producer-and-consumer-accuracy)
  * [Calculate and save precision, recall and F1](#calculate-and-save-precision-recall-and-f1)
  * [Calculate quantity disagreement and allocation](#calculate-quantity-disagreement-and-allocation)
- [Optimised workflow:](#optimised-workflow)
  * [Accuracy assessment](#accuracy-assessment)
  * [Statistics aggregation](#statistics-aggregation)
    
<br>

## Extraction of the class from the rasters using the training data
```
#Set input directory (to where the raster files which has undergone post-processing is located_:
setwd("D:/GIS/LULC-SIANG/Individual output bands/processed")

#Get list of all capital-letter TIFF files
tif_files <- list.files(getwd(), pattern = "_processed\\.tif$", full.names = TRUE)

#(OPTIONAL) View the list
tif_files

#Load necessary packages:
library(raster)
library(sp)

# Input setup
input_dir <- getwd()  # Directory containing processed TIFFs

#Load necessary package:
library(terra)

# Prepare coordinates as spatial points with correct CRS (WGS84)
pts <- st_as_sf(as.data.frame(coordinates), coords = c("X", "Y"), crs = 4326)

# Process each TIFF file
final_results <- data.frame(
  Name = data$Name,
  Longitude = coordinates[, "X"],
  Latitude = coordinates[, "Y"]
)

for (tif_file in tif_files) {
  # Extract variable name from filename (e.g., "AJ" from "AJ_processed.tif")
  var_name <- sub("_processed\\.tif$", "", basename(tif_file))
  cat("\nProcessing", var_name, "...\n")
  
  # Load raster
  r <- rast(tif_file)
  
  # Transform points to match raster CRS if needed
  if (st_crs(pts) != crs(r)) {
    cat("Transforming points to match raster CRS...\n")
    pts_transformed <- st_transform(pts, crs(r))
  } else {
    pts_transformed <- pts
  }
  
  # Verify point locations within raster
  ext_intersects <- st_intersects(pts_transformed, st_as_sfc(st_bbox(r)))
  print(paste(sum(lengths(ext_intersects) > 0), "of", nrow(pts), "points fall within raster"))
  
  # Extract values
  vals <- terra::extract(r, vect(pts_transformed))[, 2]
  final_results[[var_name]] <- vals
  
  # Quick check of extracted values
  print(table(vals, useNA = 'always'))
}
```
## Create and save confusion matrix

```
----------OPTIONAL (START) --------------

#Check in the final_results if any rows have more than one class (which shouldn't be
#the case):

final_results$row_sum <- rowSums(final_results[, 4:15])
table(final_results$row_sum)

#results:
# 0    1    2 
# 611 1708   39 


#This indicates that there are 39 coordinates where there are overlaps

#(OPTIONAL) Check the class
class(final_results)
# Load necessary package:
library(dplyr)

#We should remove the rows containing zero or two in the row_sums (not useful for
#confusion matrix)
cmatrix <- final_results %>% filter(!(row_sum %in% c(0, 2)))


#Remove unnecessary columns
cmatrix <- cmatrix %>% 
            dplyr::select(-c(Longitude, Latitude,sum, row_sum))

#(OPTIONAL) Check a few rows of this matrix
head(cmatrix,20)

# Create confusion matrix table
conf_matrix <- table(Predicted = predicted_classes, Actual = true_classes)

# Add margin totals
conf_matrix <- addmargins(conf_matrix)

# Print the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)

#          Actual
# Predicted  AGR   AJ   BA   BU   CC  MGR   OF   SH   SN   SS   TF   WA  WRC  Sum
#       AGR   96    0    0    0    0    0    0    0    0    1    0    0    1   98
#       AJ     0   85    0    0    0    0    0    0    0    0    0    0    1   86
#       BU     0    0    0   87    0    0    0    0    0    0    0    0    0   87
#       CC     0    0    0    0   81    0    0    0    0    0    2    0    0   83
#       MGR    2    0    0    0    0   94    0    0    0    0    0    0    0   96
#       OF     0    0    0    0    0    0   72    0    0    0    2    0    0   74
#       SH     0    0    2    0    0    0    0  134    0    0    2    1    0  139
#       SN     0    0    0    0    0    1    1    1  216    0    0    0    0  219
#       SS     2    0    0    4    0    0    0    0    1  130    0    0    0  137
#       TF     0    0  126    0    5    3   14    7    0    0  247    1    0  403
#       WA     0    0    0    0    0    0    0    0    1    0    0  124    0  125
#       WRC    4    0    0    0    1    0    0    0    0    0    0    0  156  161
#       Sum  104   85  128   91   87   98   87  142  218  131  253  126  158 1708

#Create a plot fromm this table:
library(ggplot2)
p <- ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Confusion Matrix", 
       x = "Actual Class", 
       y = "Predicted Class")

p

# Save as PNG
ggsave(
  filename = "confusion_matrix_sent7.png",   # Output filename
  plot = p,                                # Plot object
  device = "png",                             # File format
  dpi = 300,                                  # Resolution (300 DPI)
  width = 12,                                 # Width in inches
  height = 7,                                 # Height in inches
)

----------OPTIONAL (END) --------------
```
Output:
![enter image description here][1]
```
#But since BA had to be subsumed under TF for this particular model,the above 
#have to be repeated with BA recoded as TF

# Process each TIFF file
final_results <- data.frame(
  Name = data$Name,
  Longitude = coordinates[, "X"],
  Latitude = coordinates[, "Y"]
)

#(OPTIONAL) check class
class(final_results)

library(dplyr)
#Recode BA as TF:
final_results <- final_results %>%
                mutate(Name = recode(Name,
                       "BA" = "TF"))

for (tif_file in tif_files) {
  var_name <- sub("_processed\\.tif$", "", basename(tif_file))
  cat("\nProcessing", var_name, "...\n")
  
  # Load raster
  r <- rast(tif_file)
  
  # Transform points to match raster CRS if needed
  if (st_crs(pts) != crs(r)) {
    cat("Transforming points to match raster CRS...\n")
    pts_transformed <- st_transform(pts, crs(r))
  } else {
    pts_transformed <- pts
  }
  
  # Verify point locations within raster
  ext_intersects <- st_intersects(pts_transformed, st_as_sfc(st_bbox(r)))
  print(paste(sum(lengths(ext_intersects) > 0), "of", nrow(pts), "points fall within raster"))
  
  # Extract values
  vals <- terra::extract(r, vect(pts_transformed))[, 2]
  final_results[[var_name]] <- vals
  
  # Quick check of extracted values
  print(table(vals, useNA = 'always'))
}

#The object 'final_results' will still have rows with more than one class i.e. 39 coordinates where there are overlaps

#We should remove the rows containing zero or two in the row_sums (not useful for
#confusion matrix)
class(final_results)
cmatrix <- final_results %>% filter(!(row_sum %in% c(0, 2)))

#Remove unnecessary columns
cmatrix <- cmatrix %>% 
  dplyr::select(-c(Longitude, Latitude, row_sum))

#(OPTIONAL) Check 20 records from the matrix
head(cmatrix,20)

# Create a contingency table (base confusion matrix)
true_classes <- cmatrix$Name
predicted_classes <- apply(cmatrix[, -1], 1, function(row) {
  colnames(cmatrix[, -1])[which.max(row)]  # Get column name with value 1
})

# Create confusion matrix table
conf_matrix <- table(Predicted = predicted_classes, Actual = true_classes)

# Add margin totals
conf_matrix <- addmargins(conf_matrix)

# Print the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)

#           Actual
# Predicted  AGR   AJ   BU   CC  MGR   OF   SH   SN   SS   TF   WA  WRC  Sum
#       AGR   96    0    0    0    0    0    0    0    1    0    0    1   98
#       AJ     0   85    0    0    0    0    0    0    0    0    0    1   86
#       BU     0    0   87    0    0    0    0    0    0    0    0    0   87
#       CC     0    0    0   81    0    0    0    0    0    2    0    0   83
#       MGR    2    0    0    0   94    0    0    0    0    0    0    0   96
#       OF     0    0    0    0    0   72    0    0    0    2    0    0   74
#       SH     0    0    0    0    0    0  134    0    0    4    1    0  139
#       SN     0    0    0    0    1    1    1  216    0    0    0    0  219
#       SS     2    0    4    0    0    0    0    1  130    0    0    0  137
#       TF     0    0    0    5    3   14    7    0    0  373    1    0  403
#       WA     0    0    0    0    0    0    0    1    0    0  124    0  125
#       WRC    4    0    0    1    0    0    0    0    0    0    0  156  161
#       Sum  104   85   91   87   98   87  142  218  131  381  126  158 1708

#Create a plot from this table:
library(ggplot2)
p <- ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red", name = "Count") +
  theme_minimal() +
  labs(title = "Confusion Matrix", 
       x = "Actual Class", 
       y = "Predicted Class")

p

# Save as PNG
ggsave(
  filename = "confusion_matrix_sent7_BA_subsumed.png",   # Output filename
  plot = p,                                # Plot object
  device = "png",                             # File format
  dpi = 300,                                  # Resolution (300 DPI)
  width = 12,                                 # Width in inches
  height = 5,                                 # Height in inches
)
```
Output:
![enter image description here][2]

<br>

## Calculate overall accuracy
```
#(OPTIONAL) Check class
class(conf_matrix)

#Convert to dataframe:
matrix <- as.data.frame.matrix(conf_matrix)
#Remove the column named sum
matrix <- matrix %>% dplyr::select(-c(Sum))
#Remove the row named sum
matrix <- matrix[-c(13),]

#(OPTIONAL) Check class
class(matrix)
matrix <- as.matrix(matrix)
accuracy <- sum(diag(matrix)) / sum(matrix)
#convert to per cent
accuracy <- accuracy*100
cat("\nAccuracy:", round(accuracy, 2))
OA <- accuracy / 100
```
```
#ONLY FOR EDUCATIONAL PURPOSE DO NOT USE KAPPA INDEX FOR ACCURACY ASSESSMENT
#Kappa index:

# Predicted cases per class
rowsums <- apply(matrix, 1, sum)
rowsums
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 98  86  87  83  96  74 139 219 137 403 125 161 
p <- rowsums / sum(matrix)
# Actual cases per class
colsums <- apply(matrix, 2, sum)
colsums
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 104  85  91  87  98  87 142 218 131 381 126 158 
q <- colsums / sum(matrix)
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa
#[1] 0.9604113
```
## Calculate and save producer and consumer accuracy
```
# Producer accuracy
PA <- diag(matrix) / colsums
# User accuracy
UA <- diag(matrix) / rowsums
#Produce a table to show both user and producer accuracy for all the classes:
(outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA))
str(outAcc)
outAcc <- outAcc %>%
  tibble::rownames_to_column(var = "Class") %>%
  as.data.frame()

#Class       producerAccuracy userAccuracy
# AGR        0.9230769    0.9795918
# AJ         1.0000000    0.9883721
# BU         0.9560440    1.0000000
# CC         0.9310345    0.9759036
# MGR        0.9591837    0.9791667
# OF         0.8275862    0.9729730
# SH         0.9436620    0.9640288
# SN         0.9908257    0.9863014
# SS         0.9923664    0.9489051
# TF         0.9790026    0.9255583
# WA         0.9841270    0.9920000
# WRC        0.9873418    0.9689441

# Save results as csv
write.csv(outAcc, "producer_and_user_accuracy.csv", row.names = FALSE)
cat("Saved results to", getwd())

#Save table in docx and png formats:
if (!require("flextable")) install.packages("flextable")
library(flextable)
if (!require("officer")) install.packages("officer")
library(officer)

# Convert to flextable
ft <- flextable(outAcc) %>%
  # Add table headers and formatting
  set_header_labels(
    Class = "Land Cover Class",
    producerAccuracy = "Producer's Accuracy",
    userAccuracy = "User's Accuracy"
  ) %>%
  # Additional styling
  #bg(part = "body", bg = "#F7F7F7") %>% # Light gray background 
  #theme_zebra(odd_header = "transparent", even_header = "transparent") %>%
  align(align = "center", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  autofit()

# Save to Word document
save_as_docx(ft, path = "producer_and_user_accuracy.docx")

# Optional: Save as PNG image
save_as_image(ft, path = "producer_and_user_accuracy.png", zoom = 3)
```
## Calculate and save precision, recall and F1
```
#Per-class precision, recall, and F-1
precision = diag(matrix) / colsums 
recall = diag(matrix) / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
prf1 <- data.frame(precision, recall, f1) 

prf1 <- prf1 %>%
  tibble::rownames_to_column(var = "Class") %>%
  as.data.frame()

#Class    precision    recall        f1
    # AGR 0.9230769 0.9795918 0.9504950
    # AJ  1.0000000 0.9883721 0.9941520
    # BU  0.9560440 1.0000000 0.9775281
    # CC  0.9310345 0.9759036 0.9529412
    # MGR 0.9591837 0.9791667 0.9690722
    # OF  0.8275862 0.9729730 0.8944099
    # SH  0.9436620 0.9640288 0.9537367
    # SN  0.9908257 0.9863014 0.9885584
    # SS  0.9923664 0.9489051 0.9701493
    # TF  0.9790026 0.9255583 0.9515306
    # WA  0.9841270 0.9920000 0.9880478
    # WRC 0.9873418 0.9689441 0.9780564

# Save results as csv
write.csv(prf1, "precision_recall_f1.csv", row.names = FALSE)
cat("Saved results to", getwd())

# Convert to flextable
ft <- flextable(prf1) %>%
  # Add table headers and formatting
  set_header_labels(
    Class = "Land Cover Class",
    precision = "Precision",
    recall = "Recall",
    f1 = "F1"
  ) %>%
  # Additional styling
  align(align = "center", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  autofit()

# Save to Word document
save_as_docx(ft, path = "precision_recall_f1.docx")

# Optional: Save as PNG image
save_as_image(ft, path = "precision_recall_f1.png", zoom = 3)

#The per-class metrics are averaged over all the classes to get macro-averaged 
#precision, recall and F-1.

(macroPrecision = mean(precision))
#[1] 0.9561876
(macroRecall = mean(recall))
#[1] 0.9734787
(macroF1 = mean(f1))
#[1] 0.9640565
```
## Calculate quantity disagreement and allocation
```
#Instead of kappa, use quantity disagreement and allocation disagreement instead:

#Quantity disagreement:
#Reference - Predicted i.e.
colsums - diag(matrix)

# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 8   0   4   6   4  15   8   2   1   8   2   2 

#Quantity disagreement in percentage
((colsums - diag(matrix)) / colsums)*100

# AGR         AJ         BU         CC        MGR         OF         SH         SN         SS 
# 7.6923077  0.0000000  4.3956044  6.8965517  4.0816327 17.2413793  5.6338028  0.9174312  0.7633588 
# TF         WA        WRC 
# 2.0997375  1.5873016  1.2658228 

#Test with unclassified pixels class:

#We should remove the rows containing two in the row_sums (not useful for
#confusion matrix)
cmatrix2 <- final_results %>% filter(!(row_sum %in% 2))

#Add a column for pixels/coordinates which were not classified into any class (NC)
cmatrix2 <- cmatrix2 %>% mutate(NC = case_when(row_sum == 0 ~ 1,
                                               row_sum == 1 ~ 0))

#Remove unnecessary columns
cmatrix2 <- cmatrix2 %>% 
  dplyr::select(-c(Longitude, Latitude, row_sum))

head(cmatrix,20)

# Create a contingency table (base confusion matrix)
true_classes <- cmatrix2$Name
predicted_classes <- apply(cmatrix2[, -1], 1, function(row) {
  colnames(cmatrix2[, -1])[which.max(row)]  # Get column name with value 1
})

# Create confusion matrix table
conf_matrix2 <- table(Predicted = predicted_classes, Actual = true_classes)

# Add margin totals
conf_matrix2 <- addmargins(conf_matrix2)

# Print the confusion matrix
print("Confusion Matrix:")
print(conf_matrix2)

#       Actual
# Predicted  AGR   AJ   BU   CC  MGR   OF   SH   SN   SS   TF   WA  WRC  Sum
#       AGR   96    0    0    0    0    0    0    0    1    0    0    1   98
#       AJ     0   85    0    0    0    0    0    0    0    0    0    1   86
#       BU     0    0   87    0    0    0    0    0    0    0    0    0   87
#       CC     0    0    0   81    0    0    0    0    0    2    0    0   83
#       MGR    2    0    0    0   94    0    0    0    0    0    0    0   96
#       NC    63   55   50   71   94   70   14   40   28   42   28   56  611
#       OF     0    0    0    0    0   72    0    0    0    2    0    0   74
#       SH     0    0    0    0    0    0  134    0    0    4    1    0  139
#       SN     0    0    0    0    1    1    1  216    0    0    0    0  219
#       SS     2    0    4    0    0    0    0    1  130    0    0    0  137
#       TF     0    0    0    5    3   14    7    0    0  373    1    0  403
#       WA     0    0    0    0    0    0    0    1    0    0  124    0  125
#       WRC    4    0    0    1    0    0    0    0    0    0    0  156  161
#       Sum  167  140  141  158  192  157  156  258  159  423  154  214 2319

#Create a plot fromm this table:
library(ggplot2)
p <- ggplot(as.data.frame(conf_matrix2), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red", name = "Count") +
  theme_minimal() +
  labs(title = "Confusion Matrix", 
       x = "Actual Class", 
       y = "Predicted Class")

p

# Save as PNG
ggsave(
  filename = "confusion_matrix_sent7_with_NC.png",   # Output filename
  plot = p,                                # Plot object
  device = "png",                             # File format
  dpi = 300,                                  # Resolution (300 DPI)
  width = 12,                                 # Width in inches
  height = 5,                                 # Height in inches
)
```
Output:
![enter image description here][4]
```
# Calculate overall accuracy
class(conf_matrix2)

#Convert to dataframe:
matrix2 <- as.data.frame.matrix(conf_matrix2)
#Remove the column named sum
matrix2 <- matrix2 %>% dplyr::select(-c(Sum))
#Remove the row named sum
matrix2 <- matrix2[-c(13),]


class(matrix2)
matrix2 <- as.matrix(matrix2)
accuracy <- sum(diag(matrix2)) / sum(matrix2)
#convert to per cent
accuracy <- accuracy*100
cat("\nAccuracy:", round(accuracy, 2))
#11.48
OA <- accuracy / 100

#Kappa index:

# Predicted cases per class
rowsums <- apply(matrix2, 1, sum)
rowsums
# AGR   AJ   BU   CC  MGR   NC   OF   SH   SN   SS   TF   WA  Sum 
# 98   86   87   83   96  611   74  139  219  137  403  125 2319 
p <- rowsums / sum(matrix2)
# Actual cases per class
colsums <- apply(matrix2, 2, sum)
colsums
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 330 280 282 315 384 314 312 516 318 846 308 272 
q <- colsums / sum(matrix2)
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa
#[1] 0.04115935

#Instead of kappa, use quantity disagreement and allocation disagreement instead:

colsums - diag(matrix2)
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 234 195 195 234 290 244 312 516 318 846 307 272 

colsums
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 330 280 282 315 384 314 312 516 318 846 308 272 

diag(matrix) 
# [1] 96 85 87 81 94 70  0  0  0  0  1  0

#Diagonal is incorrect

#Quantity disagreement in percentage
((colsums - diag(matrix2)) / colsums)*100

# AGR        AJ        BU        CC       MGR        OF        SH        SN        SS        TF 
# 70.90909  69.64286  69.14894  74.28571  75.52083  77.70701 100.00000 100.00000 100.00000 100.00000 
# WA       WRC 
# 99.67532 100.00000 

#From SH, it is clearly since the diagonal is incorrectly picked (as the matrix has
#uneven number of rows and columns)

#Instead do this:

#Extract just the row containing the non-classified category (nc)
nc <- as.data.frame.matrix(matrix2) %>% slice(6)

# Quantity disagreement and allocation disagreement
colsums - diag(matrix)  + apply(nc, 2, sum) #last term is NC
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 71  55  54  77  98  85  22  42  29  50  30  58 

#Quantity disagreement in percentage
((colsums - diag(matrix) + apply(nc, 2, sum)) / (colsums + apply(nc, 2, sum)))*100
# AGR       AJ       BU       CC      MGR       OF       SH       SN       SS       TF       WA 
# 42.51497 39.28571 38.29787 48.73418 51.04167 54.14013 14.10256 16.27907 18.23899 11.82033 19.48052 
# WRC 
# 27.10280


#Redo the entire process to check if the class exists within a 30 m radius buffer
#This is to choose a biologically meaningful information for the accuracy assessment:

# Load required libraries
library(terra)
library(sf)
library(dplyr)

#Set input directory:
setwd("D:/Paul_Pop_RS_GIS_files/LULC-SIANG/Individual output bands/sent_rfc_seven/processed")

#Get list of all capital-letter TIFF files
tif_files <- list.files(getwd(), pattern = "_processed\\.tif$", full.names = TRUE)
tif_files

# Input setup
input_dir <- getwd()  # Directory containing processed TIFFs

# Prepare coordinates as spatial points with correct CRS (WGS84)
pts <- st_as_sf(as.data.frame(coordinates), coords = c("X", "Y"), crs = 4326)

# Process each TIFF file
final_results <- data.frame(
  Name = data$Name,
  Longitude = coordinates[, "X"],
  Latitude = coordinates[, "Y"]
)

# Define India NSF LCC (EPSG:7755)
india_nsf_lcc <- st_crs(7755)  # WGS 84 / India NSF LCC

# Ensure points are in India NSF LCC
pts_projected <- st_transform(pts, india_nsf_lcc)

#Pre-loop tests
# Before the loop, verify the points
print(head(st_coordinates(pts)))
print(st_crs(pts))
# Check one sample raster
test_raster <- rast(tif_files[1])
print(crs(test_raster))
print(unique(values(test_raster)))
plot(test_raster)
plot(vect(pts), add=TRUE)
rm(test_raster)

# Load and prepare all rasters
class_rasters <- rast(tif_files)
if (crs(class_rasters) != india_nsf_lcc$wkt) {
  class_rasters <- project(class_rasters, india_nsf_lcc$wkt)
}
                                                                                                                                                                            
# Get reference point values (all classes)
point_allclasses <- terra::extract(class_rasters, vect(pts_projected))

# Initialize final_results with point values
final_results <- cbind(
  data.frame(Name = data$Name,
             Longitude = coordinates[, "X"],
             Latitude = coordinates[, "Y"]),
  point_allclasses[,-1]  # Exclude ID column
)

#Add a column indicating whether the target class is present or absent
# Get column names of final_results
column_names <- colnames(final_results)

#EXtract just the columns the class information
class_cols <- 4:ncol(final_results)

#Create row sums:
final_results$row_sum <- rowSums(final_results[,class_cols])

# Create a logical vector indicating if each point has a '2' or greater values under its named 
#class
result <- final_results$Name %in% column_names &
          final_results$row_sum > 1

# Convert to numeric and add as a new column to final_results
final_results$multi_class <- as.numeric(result)
#Remove the result list
rm(result)

# Identify points that need cleaning (target class = 1 at point, plus one or more other class = 1)
multi_class_rows <- which(final_results$multi_class == 1)

#For each problematic row, keep only the target class (if present)
for (i in multi_class_rows) {
  target_class <- final_results$Name[i]

  # Check if target class exists in this row's classes
  if (target_class %in% names(final_results[, class_cols])) {
    # Get the value for target class
    target_value <- final_results[i, target_class]

    # If target class is present (1), set all others to 0
    if (target_value == 1) {
      final_results[i, class_cols] <- 0  # First set all to 0
      final_results[i, target_class] <- 1  # Then restore target class
    }
  }
}

#Remove row_sum and multi_class columns (their values have changed due to the last
#for-loop)

final_results <- final_results %>% select (-c("row_sum","multi_class"))

#Add a column indicating whether the target class is present or absent
# Get column names of final_results
column_names <- colnames(final_results)

# Create a logical vector indicating if each point has a '1' under its named class
result <- final_results$Name %in% column_names &
  final_results[cbind(1:nrow(final_results), match(final_results$Name, column_names))] == 1

# Convert to numeric and add as a new column to final_results
final_results$class_present <- as.numeric(result)

# Verify the result
tail(final_results[c("Name", "class_present")])

# Identify points that need buffer check (target class = 0 at point)
points_to_check <- which(final_results$class_present == 0)

#Recode BA as TF:
final_results <- final_results %>%
  mutate(Name = recode(Name,
                       "BA" = "TF"))
# Get all class names
class_names <- names(class_rasters)

#  Process buffers for potential class replacements
for (tif_file in tif_files) {
  var_name <- sub("_processed\\.tif$", "", basename(tif_file))
  cat("\nProcessing", var_name, "...\n")
  
  r <- class_rasters[[var_name]]
  
  if (length(points_to_check) > 0) {
    # Create buffers only for these points
    subset_pts <- pts_projected[points_to_check,]
    buffer_30m <- st_buffer(subset_pts, dist = 30)
  
    # Extract buffer values
    buffer_values <- terra::extract(r, vect(buffer_30m))
    
    # Process each point that might need updating
    for (i in seq_along(points_to_check)) {
      pt_id <- points_to_check[i]
      
      # Check if target class exists in buffer
      if (any(buffer_values[buffer_values$ID == i, 2] == 1, na.rm = TRUE)) {
        # Found target class in buffer but not at point - apply replacement rules
        final_results[pt_id, var_name] <- 1  # Set target class to 1
        
        # Set all other classes to 0 for this point
        other_classes <- setdiff(class_names, var_name)
        final_results[pt_id, other_classes] <- 0
      }
    }
  }
  
  cat("Updated", sum(final_results[[var_name]] == 1), "presences for", var_name, "\n")
}

# Verification
final_results$row_sum <- rowSums(final_results[,4:15])
point_allclasses$row_sum <- rowSums(point_allclasses[,-1])

cat("\nOriginal point values:\n")
print(table(point_allclasses$row_sum))

# 0    1    2 
# 611 1708   39 

cat("\nFinal results after buffer enhancement:\n")
print(table(final_results$row_sum))

# 0    1 
# 109 2249 

head(final_results)

# Key Features:
#Two-Stage Process:
# First creates point_allclasses with exact point values
# Then only checks buffers for points where class was absent (value = 0)
# 
# Careful Updates:
# Only upgrades 0 to 1 when buffer confirms presence
# Downgrades 1 to 0 when the target class is not present in the point but is 
#present in the buffer
# 
# Efficient Processing:
#   
# Only creates buffers for points that need checking. Avoids redundant extractions 
#for already-confirmed presences
# 
# Transparent Verification:
# Shows before/after counts of class presences
# Compares row sums between point data and final results
# 
# Workflow Logic:
# For each class (AGR, AJ, BU, etc.):
# Identify all points where this class was absent (0) in point extraction
# For only these points: Create 30m buffers and Check if class exists in buffer area
# If yes, update from 0→1 in final results
# Points where class was already present (1) remain unchanged

# Write final results to CSV
write.csv(final_results, "extracted_values_with_buffer_check.csv", row.names = FALSE)

# point_allclasses$row_sum <- rowSums(point_allclasses[, 2:13])
# table(point_allclasses$row_sum)

#Results with all classes:
#   0    1    2    3    4    5 
# 109 1470  688   78   11    2 

#Results with only target class:
# 0    1 
# 365 1993

#results when only using points and not buffers:
# 0    1    2 
# 611 1708   39

#This indicates that there are 39 coordinates where there are overlaps

#We should remove the rows containing two or more, in the row_sums (not useful for
#confusion matrix)
class(final_results)
# cmatrix <- final_results %>% filter(!(row_sum %in% c(2, 3, 4, 5)))
#The processed dataset will most likely will only have 0 and 1, but keeping 3,4, &
#5 just in case. Actually, skip the above step
cmatrix <- final_results

#create a dataframe for pixels/coordinates which were not classified into any class (NC)
#This will be needed later on. Here 2, 3, 4, 5, and 6 (highest likely overlap) are 
#all not classified (NC) since the previous steps only kept 1s for the target class
#when 2 or more classes have 1s.
library(tidyverse)
nc <- cmatrix %>% 
          mutate(NC = case_when(row_sum == 0 ~ 1,
                                row_sum == 2 ~ 1,
                                row_sum == 3 ~ 1,
                                row_sum == 4 ~ 1,
                                row_sum == 5 ~ 1,
                                row_sum == 6 ~ 1,
                                row_sum == 1 ~ 0))  %>% 
          select("Name","NC") %>%
          table() %>%
          as.data.frame() %>%
          filter(NC == 1) %>%
          select(Name, Freq) %>%
          deframe()  

nc

# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
#   9  11   1  19  32  14   0   6   4   2   0  11 

#For the contigency table, filter out 0s,2s and other values above 2,
#and remove unnecessary columns
cmatrix <- cmatrix %>% 
    filter(!(row_sum %in% c(0,2,3,4,5,6))) %>%
    dplyr::select(-c(Longitude, Latitude, class_present, row_sum))
    
head(cmatrix,20)

# Create a contingency table (base confusion matrix)
true_classes <- cmatrix$Name
predicted_classes <- apply(cmatrix[, -1], 1, function(row) {
  colnames(cmatrix[, -1])[which.max(row)]  # Get column name with value 1
})

# Create confusion matrix table
conf_matrix <- table(Predicted = predicted_classes, Actual = true_classes)

# Add margin totals
conf_matrix <- addmargins(conf_matrix)

# Print the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)
class(conf_matrix)
str(conf_matrix)

#           Actual
# Predicted  AGR   AJ   BU   CC  MGR   OF   SH   SN   SS   TF   WA  WRC  Sum
#       AGR   96    0    0    0    0    0    0    0    1    0    0    1   98
#       AJ     0   85    0    0    0    0    0    0    0    0    0    1   86
#       BU     0    0   87    0    0    0    0    0    0    0    0    0   87
#       CC     0    0    0   81    0    0    0    0    0    2    0    0   83
#       MGR    2    0    0    0   94    0    0    0    0    0    0    0   96
#       OF     0    0    0    0    0   72    0    0    0    2    0    0   74
#       SH     0    0    0    0    0    0  134    0    0    4    1    0  139
#       SN     0    0    0    0    1    1    1  216    0    0    0    0  219
#       SS     2    0    4    0    0    0    0    1  130    0    0    0  137
#       TF     0    0    0    5    3   14    7    0    0  373    1    0  403
#       WA     0    0    0    0    0    0    0    1    0    0  124    0  125
#       WRC    4    0    0    1    0    0    0    0    0    0    0  156  161
#       Sum  104   85   91   87   98   87  142  218  131  381  126  158 1708


           # Actual (without including the multi-class)
# Predicted  AGR   AJ   BU   CC  MGR   OF   SH   SN   SS   TF   WA  WRC  Sum
      # AGR   58    0    0    0    2    0    0    0    1    0    0    3   64
      # AJ     0   72    0    1    0    0    0    0    0    0    0    0   73
      # BU     1    2   60    0    0    0    0    0    2    0    0    1   66
      # CC     0    0    0   62    0    1    0    0    0    0    0    0   63
      # MGR    5    4    4    2  112    3    0    0    0    0    0    0  130
      # OF     2    1    1    3    6   50    0    0    0    1    0    0   64
      # SH     0    1    0    2    0    2   88    0    0    1    0    0   94
      # SN     2    0    0    0    6    1    0  214    0    0    0    0  223
      # SS    23    1   61    0    2    0    0   16  137    0    1    0  241
      # TF    18   44   11   60   31   77   67    0    2  414    4    3  731
      # WA    20    0    0    0    0    6    2   22   11    1  147    0  209
      # WRC   30    4    3    9    1    4    0    0    2    4    3  197  257
      # Sum  159  129  140  139  160  144  157  252  155  421  155  204 2215

           # Actual (multi-class replaced with target class)
# Predicted  AGR   AJ   BU   CC  MGR   OF   SH   SN   SS   TF   WA  WRC  Sum
      # AGR  112    0    0    0    2    0    0    0    1    0    0    3  118
      # AJ     0  105    0    1    0    0    0    0    0    0    0    0  106
      # BU     0    1  107    0    0    0    0    0    2    0    0    1  111
      # CC     0    0    0  108    0    1    0    0    0    0    0    0  109
      # MGR    3    2    2    2  128    3    0    0    0    0    0    0  140
      # OF     2    0    0    1    5   97    0    0    0    1    0    0  106
      # SH     0    1    0    1    0    1  142    0    0    1    0    0  146
      # SN     1    0    0    0    3    0    0  247    0    0    0    0  251
      # SS     7    1   29    0    1    0    0    3  149    0    1    0  191
      # TF     9   20    3   24   24   44   15    0    1  417    4    3  564
      # WA     9    0    0    0    0    3    1    5    2    1  152    0  173
      # WRC   19    2    3    6    0    1    0    0    1    4    1  197  234
      # Sum  162  132  144  143  163  150  158  255  156  424  158  204 2249

#Create a plot from this table:
library(ggplot2)
p <- ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  scale_fill_gradient(low = "blue", high = "red", name = "Count") +
  theme_minimal() +
  labs(title = "Confusion Matrix", 
       x = "Actual Class", 
       y = "Predicted Class")

p

# Save as PNG
ggsave(
  filename = "confusion_matrix_sent7_BA_subsumed_target_classed_final.png",   # Output filename
  plot = p,                                # Plot object
  device = "png",                             # File format
  dpi = 300,                                  # Resolution (300 DPI)
  width = 12,                                 # Width in inches
  height = 5,                                 # Height in inches
)
```
![enter image description here][5]
```
# Calculate overall accuracy

#Convert to dataframe:
matrix <- as.data.frame.matrix(conf_matrix)
#Remove the column named sum
matrix <- matrix %>% dplyr::select(-c(Sum))
#Remove the row named sum
matrix <- matrix[-c(13),]

class(matrix)
matrix <- as.matrix(matrix)
accuracy <- sum(diag(matrix)) / sum(matrix)
#convert to per cent
accuracy <- accuracy*100
cat("\nAccuracy:", round(accuracy, 2))
#Accuracy: 72.73
#Accuracy: 87.19
#Both without considering the classes misclassified as nothing
OA <- accuracy / 100
#Don't use as Overall accuracy/Per cent accuracy is not a good metric




#Kappa index:

# Predicted cases per class
rowsums <- apply(matrix, 1, sum)
rowsums
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 98  86  87  83  96  74 139 219 137 403 125 161 
#Target-classed
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
#  64  73  66  63 130  64  94 223 241 731 209 257 
#Target-classed2
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 118 106 111 109 140 106 146 251 191 564 173 234 
p <- rowsums / sum(matrix)
# Actual cases per class
colsums <- apply(matrix, 2, sum)
colsums
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 104  85  91  87  98  87 142 218 131 381 126 158 
#target-classed
#AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
#159 129 140 139 160 144 157 252 155 421 155 204 
#target-classed2
# AGR  AJ  BU  CC MGR  OF  SH  SN  SS  TF  WA WRC 
# 162 132 144 143 163 150 158 255 156 424 158 204 

q <- colsums / sum(matrix)
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa  #Don't use
#[1] 0.9604113
#target-classed:
#[1] 0.6915764
#target-classed2:
#[1] 0.8568126

# Producer accuracy
# Producer's Accuracy is the map accuracy from the POV of the map maker (the producer). 
# It tells us whether real features on the ground is correctly shown on the classified map or 
# the probability that a certain land cover on the ground is classified as such.
#i.e. total correctly predicted class/total number of reference points used for 
#that class
PA <- diag(matrix) / (colsums + nc)
#Alternatively,
PA <- diag(matrix) / table(final_results$Name)
# User accuracy
# User's Accuracy is the accuracy from the point of view of a map user, not the 
# map maker. It tells us how often the classes on the classified map will actually
#be present on the ground (reliability of the map).
#i.e. total correctly predicted class/total number of predictions for that class 
#(correct+incorrect predictions)
UA <- diag(matrix) / rowsums
#Produce a table to show both user and producer accuracy for all the classes:
(outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA))
str(outAcc)
outAcc <- outAcc %>%
  tibble::rownames_to_column(var = "Class") %>%
  as.data.frame()

#Class producerAccuracy userAccuracy
# AGR        0.9230769    0.9795918
# AJ         1.0000000    0.9883721
# BU         0.9560440    1.0000000
# CC         0.9310345    0.9759036
# MGR        0.9591837    0.9791667
# OF         0.8275862    0.9729730
# SH         0.9436620    0.9640288
# SN         0.9908257    0.9863014
# SS         0.9923664    0.9489051
# TF         0.9790026    0.9255583
# WA         0.9841270    0.9920000
# WRC        0.9873418    0.9689441

#Target-classed:
#     producerAccuracy userAccuracy
# AGR        0.3647799    0.9062500
# AJ         0.5581395    0.9863014
# BU         0.4285714    0.9090909
# CC         0.4460432    0.9841270
# MGR        0.7000000    0.8615385
# OF         0.3472222    0.7812500
# SH         0.5605096    0.9361702
# SN         0.8492063    0.9596413
# SS         0.8838710    0.5684647
# TF         0.9833729    0.5663475
# WA         0.9483871    0.7033493
# WRC        0.9656863    0.7665370

#Target-classed2
#     producerAccuracy userAccuracy
# AGR        0.6913580    0.9491525
# AJ         0.7954545    0.9905660
# BU         0.7430556    0.9639640
# CC         0.7552448    0.9908257
# MGR        0.7852761    0.9142857
# OF         0.6466667    0.9150943
# SH         0.8987342    0.9726027
# SN         0.9686275    0.9840637
# SS         0.9551282    0.7801047
# TF         0.9834906    0.7393617
# WA         0.9620253    0.8786127
# WRC        0.9656863    0.8418803

#Target-classed2 corrected for zero classification
#     producerAccuracy userAccuracy
# AGR        0.6549708    0.9491525
# AJ         0.7342657    0.9905660
# BU         0.7379310    0.9639640
# CC         0.6666667    0.9908257
# MGR        0.6564103    0.9142857
# OF         0.5914634    0.9150943
# SH         0.8987342    0.9726027
# SN         0.9463602    0.9840637
# SS         0.9312500    0.7801047
# TF         0.9788732    0.7393617
# WA         0.9620253    0.8786127
# WRC        0.9162791    0.8418803

# Save results as csv
write.csv(outAcc, "producer_and_user_accuracy_final.csv", row.names = FALSE)
cat("Saved results to", getwd())

#Save table in docx and png formats:
if (!require("flextable")) install.packages("flextable")
library(flextable)
if (!require("officer")) install.packages("officer")
library(officer)

# Convert to flextable
ft <- flextable(outAcc) %>%
  # Add table headers and formatting
  set_header_labels(
    Class = "Land Cover Class",
    producerAccuracy = "Producer's Accuracy",
    userAccuracy = "User's Accuracy"
  ) %>%
  # Additional styling
  #bg(part = "body", bg = "#F7F7F7") %>% # Light gray background 
  #theme_zebra(odd_header = "transparent", even_header = "transparent") %>%
  flextable::align(align = "center", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  autofit()

# Save to Word document
save_as_docx(ft, path = "producer_and_user_accuracy_final.docx")

# Optional: Save as PNG image
save_as_image(ft, path = "producer_and_user_accuracy_final.png", zoom = 3)
```
Output:
![enter image description here][6]
```
#Per-class precision, recall, and F-1
precision = diag(matrix) / colsums 
recall = diag(matrix) / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

#The per-class metrics are averaged over all the classes to get macro-averaged 
#precision, recall and F-1.

(macroPrecision = mean(precision))
#[1] 0.9561876
#target-classed2
# [1] 0.8458956
(macroRecall = mean(recall))
#[1] 0.9734787
#target-classed2
# [1] 0.9100429
(macroF1 = mean(f1))
#[1] 0.9640565
#target-classed2
#[1] 0.8677329

#Create a dataframe for these metrics
prf1 <- data.frame(precision, recall, f1) 

prf1 <- prf1 %>%
  tibble::rownames_to_column(var = "Class") %>%
  as.data.frame()
prf1

#Class precision   recall        f1
# AGR 0.9230769 0.9795918 0.9504950
# AJ  1.0000000 0.9883721 0.9941520
# BU  0.9560440 1.0000000 0.9775281
# CC  0.9310345 0.9759036 0.9529412
# MGR 0.9591837 0.9791667 0.9690722
# OF  0.8275862 0.9729730 0.8944099
# SH  0.9436620 0.9640288 0.9537367
# SN  0.9908257 0.9863014 0.9885584
# SS  0.9923664 0.9489051 0.9701493
# TF  0.9790026 0.9255583 0.9515306
# WA  0.9841270 0.9920000 0.9880478
# WRC 0.9873418 0.9689441 0.9780564
#target-classed:
#    Class precision    recall        f1
# 1    AGR 0.3647799 0.9062500 0.5201794
# 2     AJ 0.5581395 0.9863014 0.7128713
# 3     BU 0.4285714 0.9090909 0.5825243
# 4     CC 0.4460432 0.9841270 0.6138614
# 5    MGR 0.7000000 0.8615385 0.7724138
# 6     OF 0.3472222 0.7812500 0.4807692
# 7     SH 0.5605096 0.9361702 0.7011952
# 8     SN 0.8492063 0.9596413 0.9010526
# 9     SS 0.8838710 0.5684647 0.6919192
# 10    TF 0.9833729 0.5663475 0.7187500
# 11    WA 0.9483871 0.7033493 0.8076923
# 12   WRC 0.9656863 0.7665370 0.8546638

#target-classed2:
#   Class precision    recall        f1
# 1    AGR 0.6913580 0.9491525 0.8000000
# 2     AJ 0.7954545 0.9905660 0.8823529
# 3     BU 0.7430556 0.9639640 0.8392157
# 4     CC 0.7552448 0.9908257 0.8571429
# 5    MGR 0.7852761 0.9142857 0.8448845
# 6     OF 0.6466667 0.9150943 0.7578125
# 7     SH 0.8987342 0.9726027 0.9342105
# 8     SN 0.9686275 0.9840637 0.9762846
# 9     SS 0.9551282 0.7801047 0.8587896
# 10    TF 0.9834906 0.7393617 0.8441296
# 11    WA 0.9620253 0.8786127 0.9184290
# 12   WRC 0.9656863 0.8418803 0.8995434

# Save results as csv
write.csv(prf1, "precision_recall_f1.csv", row.names = FALSE)
cat("Saved results to", getwd())

# Convert to flextable
ft <- flextable(prf1) %>%
  # Add table headers and formatting
  set_header_labels(
    Class = "Land Cover Class",
    precision = "Precision",
    recall = "Recall",
    f1 = "F1"
  ) %>%
  # Additional styling
  align(align = "center", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  autofit()

# Save to Word document
save_as_docx(ft, path = "precision_recall_f1.docx")

# Optional: Save as PNG image
save_as_image(ft, path = "precision_recall_f1.png", zoom = 3)
```
Output:
![enter image description here][7]
```

#Instead of kappa, use quantity disagreement and allocation disagreement instead.
# For these metrices. the False and True Positives and Negatives need to be 
# extracted from the confusion matrix:
# Extract TP, FP, FN, TN for each class
classes <- colnames(conf_matrix)[-ncol(conf_matrix)]  # Exclude 'Sum'
print(classes)
# [1] "AGR" "AJ"  "BU"  "CC"  "MGR" "OF"  "SH"  "SN"  "SS"  "TF"  "WA"  "WRC"
total_samples <- sum(matrix)  + sum(nc)
print(total_samples)
# 2249

metrics <- data.frame(
  Class = classes,
  TP = diag(matrix),
  FP = rowsums - diag(matrix),
  FN = colsums - diag(matrix) + nc, #nc must be added as they are the ones not
  #classified into anything incorrectly
  TN = total_samples - (diag(matrix) + (rowsums - diag(matrix)) + (colsums - diag(matrix)))
                       )

#Remove the extra rownames containing the class names:
rownames(metrics) <-NULL
print(metrics)

#    Class  TP  FP FN   TN
# 1    AGR 112   6 50 2081
# 2     AJ 105   1 27 2116
# 3     BU 107   4 37 2101
# 4     CC 108   1 35 2105
# 5    MGR 128  12 35 2074
# 6     OF  97   9 53 2090
# 7     SH 142   4 16 2087
# 8     SN 247   4  8 1990
# 9     SS 149  42  7 2051
# 10    TF 417 147  7 1678
# 11    WA 152  21  6 2070
# 12   WRC 197  37  7 2008

#Quantity disagreement:
  
# = |FN-FP/TP+FP+TN+FN|+|FP-FN/TP+FP+TN+FN|/2
# = |FN-FP/total_samples|+|FP-FN/total_samples|/2
#Through rules of fractions:
# = |(|FN - FP|+|FP-FN|)/total_samples|/2
# = |(|FN - FP|+|FP-FN|)/total_samples*2
# = 2*|FN - FP|/total_samples*2
# = |FN - FP|/total_samples

QD <- abs(metrics$FN - metrics$FP) / total_samples
QD

#Quantity disagreement in percentage
QD100 <- ( abs(metrics$FN - metrics$FP) / total_samples )*100
QD100

#Overall allocation disagreement:
sum(QD)/2
#0.1092027

#Allocation disagreement:
# = 2*min (FP/total_samples,FN/total_samples)
AD <- 2 * pmin(metrics$FP / total_samples, metrics$FN / total_samples)
AD
#Allocation disagreement in percentage
AD100 <- 2 * pmin(metrics$FP / total_samples, metrics$FN / total_samples) *100
AD100

#Overall allocation disagreement:
sum(AD)/2
#0.0360475
```


  [1]: https://i.ibb.co/mC5bQzRF/confusion-matrix-sent7.png
  [2]: https://i.ibb.co/sJXgjmNB/confusion-matrix-sent7-BA-subsumed.png
  [4]: https://i.ibb.co/FcJKW1s/confusion-matrix-sent7-with-NC.png
  [5]: https://i.ibb.co/Q3Yvb4zg/confusion-matrix-sent7-BA-subsumed-target-classed-final.png
  [6]: https://i.ibb.co/twn6Qb7P/producer-and-user-accuracy-final.png
  [7]: https://i.ibb.co/DgDQ2pdC/precision-recall-f1.png
  
  
# Optimised workflow:

## Accuracy assessment

```
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
```

## Statistics aggregation 


```
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
```  
