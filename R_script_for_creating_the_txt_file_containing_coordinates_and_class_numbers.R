#Create the txt file containing the training data coordinates and class numbers

#Requires a csv file containing the longitude, latitude, and the class abbreviations (from 'Step A.2: Create the txt file containing the training data coordinates and class numbers' of '7. Sampling')

#Check the working directory
getwd()
#setwd by Ctrl+Shift+H or setwd("C:/Users/GIS")

#Load the csv with coordinates and class abbreviations
samples <- read.csv("2024 training data.csv")

#View the files in the folder (optional):
list.files()

#View few lines of observations of main (optional):
head(samples)

#See the number of points per each class:

table(samples$Name)
#AGR  AJ  BA  BU  CC MGR  OF  SH  SS  TF  WA WRC 
# 54  27  33  22  46  14  28   7  59  28  23  57

#Load the dplyr package after installing (if necessary)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

#Recode the names:
samples <- samples %>%
    select ("X","Y","Name") %>%
    mutate(Name = recode(Name,
                              "TF" = "1",
                              "OF" = "2",
                              "BA" = "3",
                              "CC" = "4",
                              "AJ" = "5",
                              "WRC" = "6",
                              "SH" = "7",
                              "BU" = "8",
                              "AGR" = "9",
                              "MGR" = "10",
                              "WA" = "11",
                              "SS"= "12"))

#If you want to filter out rows that containing any class, for example '11' (corresponding to the class of water) if they are not going to be modelled:
samples <- samples %>% filter(!grepl('11', Name))

#Delete the first row as column headers won't be used in the training file:
names(samples) <- NULL

#View a sample of the samples object:
head(samples)

#Check the class of the samples object:
class(samples)

#Save as a txt file (for Linux):
write.table(samples, "samples.txt", quote = FALSE, row.names = FALSE)

#Save as a txt file (for Windows) (uncomment and run the command):
#write.table(samples, "samples_crlf.txt", quote = FALSE, row.names = FALSE)

#The next step is necessary ONLY for Windows:

#Since the write.table function saves the file with DOS/Windows newline (CRLF),
#it has to be converted to Unix newline (LF) for use in FORCE (no direct option
#to do that with write.table). Carry out the following to do that
#(Ref: https://stackoverflow.com/questions/25775894/transforming-an-r-script-from-dos-line-endings-to-unix):
Text = paste(readLines("samples_crlf.txt"), collapse="\n")
conx = file("samples.txt", open="wb")
write(Text, conx)
close(conx)
