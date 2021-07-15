library(tidyverse)
library(lubridate)
library(plotly)

#possibly using Map() function
 %>%
  map(abBehav)
  filter() %>%
  select(abBehav) %>%
  
# finding the MAX, Mean, MIN values of any fH data
  locDat[which.min(locDat$A.hr),] %>%
  select(A.hr)  #tells you the location of its data
mean(locDat$A.hr) #mean
median(locDat)

#trying "for" to find a way to make D column have C values
  map(locDat$D.locs, ifelse(is.na(), locDat$C.locs)
    
  for(i in 1:length(locDat$D.locs)){
  locDat$D.locs[i] = ifelse(is.na(locDat$D.locs[i]), locDat$C.locs[i], locaDat$D.locs[i])
    }

#setting up calculations for 100 percentage change between D and E 
#used this code for setting heart rate to points in A and B locations
locDat$A.hr <- beats$heartRate.bpm[locDat$A.locs]
  locDat$B.hr <- beats$heartRate.bpm[locDat$BC.locs]
  locDat$D.hr <- beats$heartRate.bpm[locDat$DC.locs]
  locDat$E.hr <- beats$heartRate.bpm[locDat$E.locs]
  pDrop.AB <- (locDat$A.hr - locDat$B.hr) / locDat$A.hr*100
  locDat$pDrop.AB <- (locDat$A.hr - locDat$B.hr) / locDat$A.hr*100
  pRise.DE <- (locDat$E.hr - locDat$D.hr) / locDat$E.hr*100
  locDat$pRiseDE <- (locDat$E.hr - locDat$D.hr) / locDat$E.hr*100
  
#distance between B and D. Accounting for presence of C
locDat$BDdur.s <- NA
  for(i in 1:length(locDat$A.locs)){
  if(locDat$BCType[i] == "C"){next} #for C
  t1 <- beats$timestamp[locDat$BC.locs[i]]
  t2 <- beats$timestamp[locDat$DC.locs[i]]
  locDat$BDdur.s[i] <- as.numeric(t2) - as.numeric(t1)
}
#distance between A and E
for(i in 1:length(locDat$A.locs)){
  t3 <- beats$timestamp[locDat$A.locs[i]]
  t4 <- beats$timestamp[locDat$E.locs[i]]
  locDat$AEdur.s[i] <- as.numeric(t4) - as.numeric(t3)
}
#distance between E and the next A
  for(i in 1:length(locDat$A.locs)){
    j = i + 1
    Etime <- beats$timestamp[locDat$E.locs [i]]
    Atime <- beats$timestamp[locDat$A.locs [j]]
    locDat$EAdur.s[i] <- as.numeric(Atime) - as.numeric(Etime)
  }

nullover <- which(locDat$EAdur.s >= 10000)
locDat$EAdur.s[nullover] <- NA

locDat_l <- split(locDat, f = locDat$batID)

dat <- lapply(locDat_l, function(x){
  nullover <- which(x$EAdur.s >= 10000)
  x$EAdur.s[nullover] <- NA
  return(x)
})
dat_df <- do.call("rbind", dat)


    
  