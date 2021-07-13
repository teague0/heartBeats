#possibly using Map() function
beats %>%
  map(abBehav)
filter(abBehav == "A", abBehav == "B", abBehav == "C", AbBehav == "D", abBehav == "E") %>%
select(abBehav) %>%

  
  
  # finding the maximum and minimum values that the start "A" of the drop can fluctuate
locDat %>%
  select(A.locs) %>%
map_dbl(~ max(.x)) %>%
  map2(~ min(A.locs))
  range()

  
  #trying "for" to find a way to make D column have C values
  for(BCType == "C") 
    print(string, D.locs)
  } else if (D.locs = NA) +
    seek(value -> BCType)

map(locDat$D.locs, ifelse(is.na(), locDat$C.locs)
    
for(i in 1:length(locDat$D.locs)){
  locDat$D.locs[i] = ifelse(is.na(locDat$D.locs[i]), locDat$C.locs[i], locaDat$D.locs[i])
}
    

locDat
rows.in.BC.locs.that.are.not.in.D.locs(BC.locs, D.locs)
within(locDat, D.locs[is.NA(n)] <- BC.locs[is.value(n)])

#setting up calculations for percentage change between D and E 
#used this code for setting heart rate to points in A and B locations
> locDat$A.hr <- beats$heartRate.bpm[locDat$A.locs]
> locDat$B.hr <- beats$heartRate.bpm[locDat$BC.locs]
pDrop.AB <- (locDat$A.hr - locDat$B.hr) / locDat$A.hr*100
locDat$pDrop.AB <- (locDat$A.hr - locDat$B.hr) / locDat$A.hr*100

growth_rate <- ()
for(i in 1:length(locDat$D.locs))

locDat %>%
  select(A.hr, B.hr) %>%
  rate percent = 
for((i in 1:)(A.hr, B.hr) *100)


