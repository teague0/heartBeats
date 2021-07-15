
#Using the index numbers from the location table to pull out sections of the beats data

#A-B slope
locDat$ABslope <- NA
locDat$DCEslope <- NA
for(i in 1:length(locDat$A.locs)){
  AB.hr <- beats$heartRate.bpm[locDat$A.locs[i]:locDat$BC.locs[i]]
  AB.time <- beats$timestamp[locDat$A.locs[i]:locDat$BC.locs[i]]
  #plot(tmp.hr/60~as.numeric(AB.time))
  m1 <- cor(AB.hr/60, as.numeric(AB.time))
  locDat$ABslope[i] <- m1
  DCE.hr <- beats$heartRate.bpm[locDat$DC.locs[i]:locDat$E.locs[i]]
  DCE.time <- beats$timestamp[locDat$DC.locs[i]:locDat$E.locs[i]]
  m2 <- cor(DCE.hr/60,as.numeric(DCE.time))
  locDat$DCEslope[i] <- m2
  }

  

