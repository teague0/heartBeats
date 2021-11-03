library(tidyverse)
library(lubridate)
library(plotly)

#reassign index value = row
beats$index <- rownames(beats)

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

#making a graph to show difference in A and E heart rate

#We need to take out the A & R heart rate data only, then combine them into a column that show the point name (A or E), heart rate, and the period ID

locDat$periodID <- paste0("per.", rownames(locDat))
newDat <- locDat %>% select(periodID, A.hr, E.hr)
newDat$AEdiff <- newDat$E.hr - newDat$A.hr
newDatL <- newDat %>% gather(startEnd, hr, A.hr:E.hr)

ggplot(data=newDatL, aes(x=startEnd, y=hr, group=periodID)) +
geom_line(alpha=.5) +
geom_point() +
geom_boxplot()

newDatL %>% 
  arrange(AEdiff) %>% 
ggplot() +
  #geom_boxplot(aes(x=startEnd, y=hr))+
  geom_line(aes(x=startEnd, y=hr, color= AEdiff, group=periodID), alpha=.5, size = 1) +
  geom_point(aes(x=startEnd, y=hr, color = AEdiff), size = 2, alpha = 0.6)+
  scale_color_viridis_c(option = "D")

#change over time Graph
locDat$periodID <- paste0("per.", rownames(locDat))
TchangeABDE <- locDat %>% select(periodID, ABchange.BPS, DEchange.BPS)
TchangeABDE_L <- TchangeABDE %>% gather(downUp, change, ABchange.BPS:DEchange.BPS)

ggplot(TchangeABDE_L) +
  geom_boxplot(aes(x = downUp, y = abs(change)))



