#Pull sets of data that are designated by the Location Table and calculate some metrics

locDat$BDdur.s <- NA
for(i in 1:length(locDat$A.locs)){
  if(locDat$BCType[i] == "C"){next} #for C
  t1 <- beats$timestamp[locDat$BC.locs[i]]
  t2 <- beats$timestamp[locDat$DC.locs[i]]
  locDat$BDdur.s[i] <- as.numeric(t2) - as.numeric(t1)
}
for(i in 1:length(locDat$A.locs)){
  t3 <- beats$timestamp[locDat$A.locs[i]]
  t4 <- beats$timestamp[locDat$E.locs[i]]
  locDat$AEdur.s[i] <- as.numeric(t4) - as.numeric(t3)
}

ggplot(locDat)+
  geom_boxplot(aes(x = BCType, y = AEdur.s, fill = BCType))
            

beats %>%
  filter(batID == "inga", dayNight == "day") %>%
  select(batID, dayNight, heartRate.bpm)
  t5 <- beats$timestamp[locDat$E.locs [i]]
  t6 <- beats$timestamp[locDat$A.locs [i + 1]]
  within.data.frame([i +1] = locDat$A.locs)
  locDat$EAdur.s[i] <- as.numeric(t5) - as.numeric(t6)