#Estimate the cumulative daily energetic expenditure in kilojoules per day

#Recalculate the original energy expenditure estimates to get the units correct.
beats <- beats %>% mutate(vo2 = 0.042 * bodyMass^0.328 * heartMass^0.913 *heartRate.bpm^2.065, #vo2 in mL per min
                          joulPerMin = vo2 * 21.11, #convert VO2 (ml per min) to Joules per min 1 mLO2 = 21.11 J 
                          watts = joul.min / 60, #joules per minute to joules per second (W)
                          joulPerHour = joulPerMin * 60) #joules per hour

#get the estimate time elapse between measurements to go from rate to the actual expenditure. To do this, split up the data in a list by bat day to keep all of the days separated & avoid any really long time lags because of day / bat crossovers in the sequential data file. 

#This will create a list, then find the time between observations
batDay_l <- split(beats, f = beats$batID_day)
dat <- lapply(batDay_l, function(x){
  x$timeLag <-  1 #give everything a default time lag of 1 s
  for(i in 1:(length(x$timestamp)-1)){
    j = i + 1 #count up by 1
    x$timeLag[j] <-  difftime(x$timestamp[j], x$timestamp[i]) #time difference between second & first timestamps.
  }
  return(x) #give back the dataframe with the new column
})

dat_df <- do.call("rbind", dat) #bind those individual data frames in the list together
beats <- dat_df #back to beats

beats$ee.inst.joul <- beats$watts * beats$timeLag

daySums <- beats %>% group_by(batID_day) %>% 
  summarize(dee.KJ = sum(ee.inst.joul)/1000,
            minsRecorded = sum(timeLag)/60)

#NOTE: The bats weren't didn't have their heart rates recorded for the entire day (1440 minutes). The longest we see is 1304, which means we need to go back to the missed time periods and insert an average energy expenditure for that time, based on the time of day. These parts will need to be found OR we use a different way to estimate DEE.





