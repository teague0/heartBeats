timeLag <- read_csv("timeLag_sheet.csv")

timeLag$timestampStart <- ymd_hms(timeLag$timestampStart) #change the timestamps into dates since they were not read in as dates
as.numeric(timeLag$timestampStart)
timeLag$timestampEnd <- ymd_hms(timeLag$timestampEnd) #changing the end timestamps into dates
as.numeric(timeLag$timestampEnd)
 
#INTERPOLATE: these are just notes I took on how interpolation variables work
#x,y is the points to be plotted
#xout = where to interpolate
#method is how the interpolation takes place (linear or constant)
#how many points the interpolation takes place between
#yleft and right are the min and max values you want

#CREATING OBSERVATIONS INBETWEEN LAG
#seek

 for(i in 1: length(timeLag$indexStart)){
  timeLag$rowStart[i] <- which(beats$index == timeLag$indexStart[i])
  timeLag$rowEnd[i] <- which(beats$index == timeLag$indexEnd[i]) #changing from the old index numbers to the real index number by rows
}

timeLagL <- split(timeLag, f = rownames(timeLag)) 

datL <- lapply(timeLagL, function(x){
  tmp <- beats[x$rowStart:x$rowEnd,]
  return(tmp)
})

x <- datL[[1]]

datL2 <- lapply(datL, function(x){
  time1 <- as.numeric(min(x$timestamp)) #find the first time
  time2 <- as.numeric(max(x$timestamp)) #find the second time
  tdiff <- round(time2 - time1, 0) -1 #number of seconds between these, minus 1 to keep fit within the two timestamps
  newTimes <- seq(from = time1, to = time2, length.out = tdiff) #create new times in seconds of length tdiff
  realTimes <- data.frame(timestamp = as.character(as.POSIXct(newTimes, origin = "1970-01-01", tz = "America/Panama")))#project to real time
  x$timestamp.char <- as.character(x$timestamp) #saving these observations as characters instead of numbers so that the code will work
  realTimes$timestamp.char <- as.character(realTimes$timestamp)
  tmp <- realTimes %>% left_join(x, by = c("timestamp.char" = "timestamp.char"))

  return(tmp)

})

#rename column
#merge data frames x and realTimes using left_join()
#once that is done run over all 26 data frames

# realTimes2 <- realTimes %>%
#   rename(
#       realTimestamps = "as.POSIXct(newTimes, origin = \"1970-01-01\", tz = \"America/Panama\")"
#   )
# colnames(realTimes2)

#ask which of the heartrates are not N/A or values > 1 
#if which heartrate is >1 if that is equal to 2 go next

if(length(which(x$heartRate.bpm > 1)) == 2){next} #check in order to not interpolate the data that I do not want interpolated




