#New interpolation testing 2022-01-11

#Create UniqueID in beats2 - no daisy
uniqueNames <- seq(1, length(beats2$timestamp), by = 1)
beats2$uniqueNames <- uniqueNames

#Daisy also has a very odd timelag in one location that was calculated across a day it looks like. Set this to 1
beats2$timeLag[153769] <- 1




batIDdayList <- split(beats2, f = beats2$batID_day)

x <- batIDdayList[[4]]
timeLagIDs_l <- lapply(batIDdayList, function(x){
  tryCatch({
    endLag <- which(x$timeLag > 2) #find timelags > 2
    startLag <- endLag - 1
    uniqueIDEnd <- x$uniqueNames[endLag]
    uniqueIDStart <- x$uniqueNames[startLag]
    timeEnd <- x$timestamp[endLag]
    timeStart <- x$timestamp[startLag]
    hrEnd <- x$heartRate.bpm[endLag]
    hrStart <- x$heartRate.bpm[startLag]
    timeDiff <- as.numeric(timeEnd - timeStart)
    hrDiff <- hrEnd - hrStart
    batID <-  rep(unique(x$batID), length(hrEnd))
    batID_day <- rep(unique(x$batID_day),length(hrEnd))
    tmp <- data.frame(batID, batID_day, timeEnd, timeStart, timeDiff, hrEnd, hrStart, hrDiff, uniqueIDEnd, uniqueIDStart)
    return(tmp)
  }, error=function(e) NULL)
})

#Havent run this yet but copied it from old timeLag just incase 

timeLagIDs <- do.call("rbind", timeLagIDs_l) #take timeLagIDs and replace timeLag dataframe

# timeLagIDs$timestampStart <- ymd_hms(timeIDs$timestampStart) #change the timestamps into dates since they were not read in as dates
# as.numeric(timeLagIDs$timestampStart)
# timeIDs$timestampEnd <- ymd_hms(timeIDs$timestampEnd) #changing the end timestamps into dates
# as.numeric(timeLag$timestampEnd)

#Creating Observations in between timeLags

for(i in 1: length(timeLagIDs$indexStart)){
  timeLagIDs$rowStart[i] <- which(beats$index == timeLagIDs$indexStart[i])
  timeLagIDs$rowEnd[i] <- which(beats$index == timeLagIDs$indexEnd[i]) #changing from the old index numbers to the real index number by rows
}


datL <- lapply(timeLagLV2, function(x){
  tmp <- beats[x$rowStart:x$rowEnd,]
  return(tmp)
})

x <- datL[[1]]

#Start here, but create the list of the individual rows that show what needs to be interpolated

#UniqueNumbers <- beats$NewColumn
#beats %>%
#rename(UniqueIDS = NewColumn)
#names(beats)[names(beats) == 'UniqueNames'] <- 'UniqueIDS'

datL <- split(timeLagIDs, f = rownames(timeLagIDs)) #create a list of all timeLag observations

#Now we want to create data frames that can hold all of the lagged observations.
x <- datL[[1]]
datL2 <- lapply(datL, function(x){
  expandTime <- seq(from = x$timeStart, to = x$timeEnd, length.out = x$timeDiff) #create new times in seconds of length tdiff
  expandTime_df <- data.frame(expandTime)
  names(expandTime_df) <- c("timestamp")
  expandTime_df$batID <- unique(x$batID)
  expandTime_df$batID <-  rep(unique(x$batID), length(expandTime))
  expandTime_df$batID_day <- rep(unique(x$batID_day),length(expandTime))
  expandTime_df$isInterpolated <- rep("yes")
  expandTime_df$isInterpolated[1] <- "no"
  expandTime_df$isInterpolated[length(expandTime)] <- "no"
  expandTime_df$heartRate.bpm <- NA
  expandTime_df$heartRate.bpm[1] <- x$hrStart
  expandTime_df$heartRate.bpm[length(expandTime)] <- x$hrEnd
  hrSlope <- x$hrDiff / x$timeDiff
  for(i in 2:(length(expandTime)-1)){
    k =i-1
    expandTime_df$heartRate.bpm[i] <- expandTime_df$heartRate.bpm[k]+hrSlope
  }
  return(expandTime_df)
})

interpolatedValues <- do.call("rbind", datL2)

#beats2$isInterpolated <- NULL

hrWithInterpol <- beats2 %>% full_join(interpolatedValues, by=c("batID_day" = "batID_day", "batID" = "batID", "timestamp"="timestamp", "heartRate.bpm" = "heartRate.bpm"))

notInter <- which(hrWithInterpol$isInterpolated == "no")

hrWithInterpol <- hrWithInterpol %>% arrange(batID_day, timestamp)


InterpolatedBeats <- InterpolatedBeats %>% 
  dplyr::mutate(vo2 = 0.042 * bodyMass^0.328 * heartMass^0.913 *heartRate.bpm^2.065, #vo2 in mL per min
                joulPerMin = vo2 * 21.11, #convert VO2 (ml per min) to Joules per min 1 mLO2 = 21.11 J 
                watts = joulPerMin / 60, #joules per minute to joules per second (W)
                joulPerHour = joulPerMin * 60)