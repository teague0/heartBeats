#Pull sets of data that are designated by the Location Table and calculate some metrics

locDat$BDdur.s <- NA
for(i in 1:length(locDat$A.locs)){
  if(locDat$BCType == "C"){next}
  t1 <- beats$timestamp[locDat$BC.locs[i]]
  t2 <- beats$timestamp[locDat$DC.locs[i]]
  locDat$BDdur.s[i] <- as.numeric(t2) - as.numeric(t1)
}

