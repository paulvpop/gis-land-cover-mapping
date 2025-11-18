#Create the queue file after Sentinel imagery download using the CDSE downloader:

#Since the download of Sentinel imagery is done via the CDSE downloader, a queue file, like 
#that in FORCE is not created. So, create it using an R script:

#Press Ctrl+Shift+H to interactively choose the wanted directory
#OR (in Windows) copy (but don't paste) the directory path, which will look like this: 
#D:/GIS/force/level1_sentinel and 
#run the next line of code
FolderPath <- normalizePath(readClipboard(), "/")
#set the working directory
setwd(FolderPath) #Just replace 'FolderPath' with the file path in quotes in case of using Linux

getwd() #To know which is the current working directory (optional)

#Change the the path in quotes with your own directory path
#See the files in the folder (optional) 
list.files()

#Get the names (and extensions) of all the files present in the directory: 
file_names <- list.files()

#Get the text in the required format for the queue file:
names <- paste0("/home/docker/force/force/level1_sentinel/",file_names," QUEUED")

#View 10 rows of data from the created object:
head(names, 10)
class(names)

#Convert to data frame:
names <- as.data.frame(names)

#Delete the first row as column headers won't be used in the queue file:
names(names) <- NULL

#Change working directory to where the queue file is to be saved:
#(For Windows) Copy-paste the path url and then run the following:
FolderPath <- normalizePath(readClipboard(), "/")
#set the working directory
setwd(FolderPath) #Just replace 'FolderPath' with the file path in quotes in case of using Linux

#Save as a txt file:
write.table(names, "queue_names.txt", quote = FALSE, row.names = FALSE)

#ONLY needed for Windows/OSes where the EOL is not unix-style by default:

#Convert CRLF to LF EOL:
Text = paste(readLines("queue_names.txt"), collapse="\n")
conx = file("queue.txt", open="wb")
write(Text, conx)
close(conx)
