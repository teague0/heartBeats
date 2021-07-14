#Restack annotated files
#Edit 2021-07-14 to rebuild the split files b/c the index numbers are off on the final dataset.

#Files are split into folders by who did the annotations. J's are csv files, with a couple of annotation pdfs thrown in. M's are txt files with rounded seconds. These need to be rejoined with the original data set based on their index.

#2021-07-08 needed to fix some point mismatches.

library(tidyverse)
library(lubridate)

j.files <- list.files("./data/Completed Data Charts csv files/", full.names = TRUE)
j.files.sh <- list.files("./data/Completed Data Charts csv files/", full.names = FALSE)
j.files.csv <- which(str_detect(j.files, "pdf") == FALSE) #grab the index for the csv files

  
#read in 1 file at a time, add to a data frame

j.data <- read.csv(j.files[j.files.csv[1]])
j.data$filename <- j.files.sh[j.files.csv[1]]


for(i in 2:length(j.files.csv)){
  tmp <- read.csv(j.files[j.files.csv[i]])
  tmp$filename <- j.files.sh[j.files.csv[i]]
  j.data <- j.data %>% bind_rows(tmp)
}
j.data$X <- NULL

#Find duplicated entries
#Are there duplicates?
length(which(duplicated(j.data$index) == TRUE)) #Nope! Not here.

# Import & concatenate M's files from .txt files
m.files <- list.files("./data/Completed _COMPROMISED Seconds_ Data charts/", full.names = TRUE)
m.files.sh <- list.files("./data/Completed _COMPROMISED Seconds_ Data charts/", full.names = FALSE)

m.data <- read.csv(m.files[1], sep = "\t")
m.data$filename <- m.files.sh[1]

for(i in 2:length(m.files)){
  tmp <- read.csv(m.files[i], sep = "\t")
  tmp$filename <- m.files.sh[i]
  m.data <- m.data %>% bind_rows(tmp)
}
names(m.data)

#Find duplicated entries
#Are there duplicates?
length(which(duplicated(m.data$index) == TRUE)) #No

#Pull out the index and abBehav from both files, left join to the original data
#Both recorded Armando. Stack J on M so that M's are first and J's will be dropped.
jcols <- j.data %>% select(index, abBehav)
mcols <- m.data %>% select(index, abBehav)
index.dat <- jcols %>% bind_rows(mcols)
range(index.dat$index, na.rm=T)

#Find duplicated entries
#Are there duplicates?
length(which(duplicated(index.dat$index) == TRUE)) #Yes. 37498 duplicates. These come from Armando as the training file. Drop the second set.

dups <- which(duplicated(index.dat$index) == FALSE) #Keep non-duplicated values
index.dat <- index.dat[dups,]

#Restack up the split original files so the the index is correct.
load("./data/Uroderma Heart Rate_clean.Rdata")
orig.files <- list.files("./data/split HR files/", full.names = TRUE)

beats2 <- read.csv(orig.files[1])

for(i in 2:length(orig.files)){
  tmp <- read.csv(orig.files[i])
  beats2 <- beats2 %>% bind_rows(tmp)
}
beats2$timestamp <- ymd_hms(beats2$timestamp)
beats2 <- beats2 %>% arrange(timestamp) 
tail(beats2)


beats2 <- beats2 %>% left_join(index.dat)

table(index.dat$abBehav)
#Check to make sure that all of the abBehav levels were entered the same
table(beats2$abBehav) #Table will give a quick count of how many times each unique value is entered in the column
#             a      A      b      B      C      D      E  E\t\t 
# 422839      1    110      1     97     13     98    110      1 

#We need to reclassify the lower case letters to upper case
beats2$abBehav <- str_to_upper(beats2$abBehav) #this will convert lower case to upper case
table(beats2$abBehav)

#We're still left with that mysterious E\t\t. That means there are 2 hidden tabs in the cell that were converted to text. We can find those & replace it with E
beats2$abBehav <- str_replace(beats2$abBehav, "E\t\t", "E")
table(beats2$abBehav) #That's fixed. We now only have A, B, C, D, E
#             A      B      C      D      E 
# 422839    111     98     13     98    111 

#There are some things in here that we need to check further. 1) The number of A and E should be the same (number of starts == number of ends). Next, B + C should equal A. That checks out & looks good. Last, B should equal D (right?). 

#Where are the row & index mismatches
beats <- beats2
beats <- beats %>% arrange(timestamp)
ind.chck <- which(rownames(beats) != beats$index)

save(beats, file = "data/Uroderma Heart Rate 2021-07-14.Rdata")





 