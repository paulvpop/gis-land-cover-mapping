#View the metadata for Landsat imagery (same workflow can be used for understanding 
#Sentinel imagery too, but with modifications as Sentinel as has different column names):

#Check working directory:
getwd()

#Ctrl+Shift+H for setting working directory using GUI
#Or use setwd("C:/Users/GIS/force")

library(data.table)

#For landsat
#Use fread() from the data.table package instead of read.csv from base R because
#the former is more memory-efficient
landsat_meta <- fread("meta/index.csv")
head(landsat_meta)

summary(landsat_meta$DATE_ACQUIRED)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "1972-07-25" "1992-05-09" "2005-11-23" "2003-09-26" "2015-08-22" "2021-12-31" 

max(landsat_meta$DATE_ACQUIRED)
#[1] "2021-12-31"

#So, the latest data for which Landsat imagery is available in the metadata is 31st December 2021

#Remove the Landsat metadata from the environment
rm(landsat_meta)

#WARNING: DON'T RUN THE NEXT LINE OF CODE UNLESS YOU HAVE AT LEAST A 32 GB RAM, 
#AND IDEALLY 64 OR ABOVE, AS THE SENTINEL METADATA TILL APRIL END, 2025 IS 11.5 GB
#WHICH TRANSLATES TO AROUND 22.6 GB IN R ENVIRONMENT

#Change the directory to where the Sentinel data is stored if required
sentinel_meta <- fread("meta_sentinel/index.csv")
head(sentinel_meta)

summary(sentinel_meta$SENSING_TIME)

# Min.                    1st Qu.                     Median                       Mean 
# "2015-06-28 08:13:27.9089" "2019-02-23 22:33:59.5000" "2021-01-15 15:13:51.0065" "2020-12-05 04:07:43.3391" 
# 3rd Qu.                       Max.                       NA's 
# "2022-11-09 08:41:13.6712" "2024-08-27 08:18:37.2850"                   "954964" 

#So, the latest data for which Sentinel imagery is available in the metadata is 27th August 2024

library(dplyr)

#Filter all the imagery to those containing less than 10% cloud cover:
sent_cl_filt <- sentinel_meta %>% 
                      filter(CLOUD_COVER < 10)
head(sent_cl_filt)

#Remove the main meta data file to save memory:
rm(sentinel_meta)

#Free unused R memory
gc()

#Filter again to the year 2024:
sent_yr_filt <- sent_cl_filt %>% 
          filter(grepl('2024', SENSING_TIME))
#Check to verify if years have been filtered
head(sent_yr_filt)

#Remove the cloud-filtered meta data file to save memory:
rm(sent_cl_filt)
#Free unused R memory
gc()

#View the full file containing cloud- and year-filtered Sentinel metadata:
View(sent_yr_filt)

#Check the number of Sentinel imagery in 2024 containing cloud-cover less than 10%:
nrow(sent_yr_filt)
#A: 852081 imagery
