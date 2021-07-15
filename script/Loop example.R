
ing <- beats %>% filter(batID == "inga", date(timestamp) == "2014-12-04")

ggplot(ing)+
  geom_line(aes(x = timestamp, y = heartRate.bpm))

ing$moveAvg.100 <- NA
ing$moveVar.100 <- NA

#Let's create a 100 s moving window to calculate an average heart rate.

for(i in 1:(length(ing$heartRate.bpm)-99)){
  endPosition = i + 99
  avg100 = mean(ing$heartRate.bpm[i:endPosition])
  var100 = var(ing$heartRate.bpm[i:endPosition])
  ing$moveAvg.100[endPosition] = avg100
  ing$moveVar.100[endPosition] = var100
}

ggplot(ing)+
  geom_line(aes(x = timestamp, y = heartRate.bpm))+
  geom_line(aes(x = timestamp, y = moveAvg.100), color = "red", size = 1)

ggplot()+
  geom_point(data = ing, aes(x = moveAvg.100, y = move))



#We have A- E marked.

a <- which(ing$abBehav == "A")
b <- which(ing$abBehav == "B")
c <- which(ing$abBehav == "C")
d <- which(ing$abBehav == "D")
e <- which(ing$abBehav == "E")
