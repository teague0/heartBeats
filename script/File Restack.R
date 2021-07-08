#Restack annotated files

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

load("./data/Uroderma Heart Rate_clean.Rdata") #beats, 554961 x 9
beats$index <- as.numeric(rownames(beats))

beats <- beats %>% left_join(index.dat)

#Check to make sure that all of the abBehav levels were entered the same
table(beats$abBehav) #Table will give a quick count of how many times each unique value is entered in the column
#             a      A      b      B      C      D      E  E\t\t 
# 470907      1    114      2     99     14     97    111      3 

#We need to reclassify the lower case letters to upper case
beats$abBehav <- str_to_upper(beats$abBehav) #this will convert lower case to upper case
table(beats$abBehav)
#             A      B      C      D      E  E\t\t 
# 470907    115    101     14     97    111      3 

#We're still left with that mysterious E\t\t. That means there are 2 hidden tabs in the cell that were converted to text. We can find those & replace it with E
beats$abBehav <- str_replace(beats$abBehav, "E\t\t", "E")
table(beats$abBehav) #That's fixed. We now only have A, B, C, D, E
#             A      B      C      D      E 
# 470907    115    101     14     97    114 

#There are some things in here that we need to check further. 1) The number of A and E should be the same (number of starts == number of ends). Next, B + C should equal A. That checks out & looks good. Last, B should equal D (right?). 

save(beats, file = "data/Uroderma Heart Rate 2021-07-08.Rdata")






 