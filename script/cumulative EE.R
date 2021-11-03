#Estimate the cumulative daily energetic expenditure in kilojoules per day

#Recalculate the original energy expenditure estimates to get the units correct.
beats <- beats %>% mutate(vo2 = 0.042 * bodyMass^0.328 * heartMass^0.913 *heartRate.bpm^2.065, #vo2 in mL per min
                          joulPerMin = vo2 * 21.11, #convert VO2 (ml per min) to Joules per min 1 mLO2 = 21.11 J 
                          watts = joulPerMin / 60, #joules per minute to joules per second (W)
                          joulPerHour = joulPerMin * 60) #joules per hour

#get the estimate time elapse between measurements to go from rate to the actual expenditure. To do this, split up the data in a list by bat day to keep all of the days separated & avoid any really long time lags because of day / bat crossovers in the sequential data file. 

#This will create a list, then find the time between observations
batDay_l <- split(beats, f = beats$batID_day)
dat <- lapply(batDay_l, function(x){
  x$timeLag <-  1 #give everything a default time lag of 1 s
  for(i in 1:(length(x$timestamp)-1)){
    j = i + 1 #count up by 1
    x$timeLag[j] <-  as.numeric(x$timestamp[j]) - as.numeric(x$timestamp[i]) #time difference between second & first timestamps.
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

beats %>% group_by(batID_day) %>% summarize(minTime = min(timestamp), maxTime = max(timestamp), difftime = difftime(maxTime, minTime), nobs = n())

p1 <- breaks %>%
  ggplot() +
  geom_path(aes(x = timestamp, y = heartRate.bpm))
p1

p1 <- breaks %>%
  ggplot() +
  geom_path(aes(x = timestamp, y = heartRate.bpm, color = timeLag)) +
  scale_color_viridis_c(option = "C")
p1
ggplotly(p1)

  breaks <- beats %>% filter(hour(timestamp) %in% c(7, 8, 9), batID_day == "inga.1")


#ARMANDO
#smaller break armando.1 2014-12-06 11:24:02 - 11:31:42
#armando.1 2014-12-06 from 16:22:03 - 16:47:17 with 10 recorded points over this interval with a range of 733 BPM to 913 BPM
#less instense timeLag from  armando.2 18:08:19 - 18:14:09
#armando.2 18:28:03 - 18:41:05 then from 18:43:42 - 18:54:54
#massive break in armando.2 from 21:50:58 - 22:24:00 no data collected
#anonther massive hole of data in armando.2 from 01:14:57- 01:59:24
#armando.2 04:41:42- 05:16:41 long break in data
#possible lag during 6:00 of armando.2
#armando.3 loss of data from 18:40:46 - 18:55:05
#armando.3 loss of data from 20:26:58 - 21:04:05
#???? 0-3 on armando.3 may need to omit 

#DAISY
#?? daisy 20,21,22
#Daisy day 2 had a 16 hour timelag most likely omit day
#daisy.3 timeLag from 18:30:22 - 18:53:51
#daisy.5 small timelag from 1:47:06 - 1:53:10 with some data within

#EGBERT
#egbert.2 21:29:15 - 21:42:15 around 100 second time lag with 10 observations mixed in
#egbert.2 21:44:36 - 21:58:26 another 100 second time lag with 11-12 observations
#egbert.2 22:59:22 - 23:25:26 little variability with around 10 observations
#egbert.3 loss of data collection followed by a 300 bpm rise 01:17:43 - 1:30:00
#egbert.3 02:22:55 - 03:04:18   
#egbert.3 07:40:11 - 07:45:00
#egbert.4 04:05:02 - 04:36:00 thirty minutes loss of data
#egbert.4 04:58:59 - 05:24:53 twenty five minutes loss of data
#egbert.4 06:15:06 - 06:41:02 ten observations mixed into loss of data
#egbert.4 07:56:23 - 09:15:02 hour and a half of lost data 
#egbert.4 10am possible timeLag over 10 minutes
#egbert.8 04:41:42 - 05:16:41 loss of data
#egbert.9 18:40:53 - 18:55:05 small timeLag mixed with 10 or so observations
#egbert.9 20:28:12 - 21:04:05 loss of data

#INGA
#inga.1 possible small timelag around 8 am and 10 am (EXEMPT FOR NOW)
#inga.2 most of the first 3 hours are screwed up because of some weird time from 18pm the day before to 4am 
#inga.3 20:41:58 - 21:18:58 loss of data
#inga.9 18:33:22 - 18:56:49 typical loss of data with scattered observations every couple minutes
#inga.9 19:57:08 - 20:03:14 small lag







  


