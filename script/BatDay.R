## calculate a BatDay, goes from 18 h till 17:59next day (REMEMBER TIME IS LOCAL!!)
library(lubridate)


datList <- lapply(split(beats, f = beats$batID), function(myInd){
  datechange <- c(0, abs(diff(as.numeric(as.factor(date(myInd$timestamp-(18*60*60)))))))
  myInd$batDay <- cumsum(datechange)+1
  return(myInd)
})

dat_df <- do.call("rbind", datList)
dat_df$batID_day <- paste0(dat_df$batID, ".", dat_df$batDay)

beats <- dat_df
