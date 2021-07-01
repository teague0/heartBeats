#Splitting original data file for manual annotation of lowered heart beats.

load("./data/Uroderma Heart Rate_clean.Rdata")
beatsL <- split(beats, f = beats$batID) #split by individual bat

#The following splits the file by date and day/night, assigns an index value, and saves a new data file with the appropriate name.
lapply(beatsL, function(x){
  xL <- split(x, date(x$timestamp)) #split by date
  lapply(xL, function(y){
    yL <- split(y, y$dayNight) #split by day / night designation
    lapply(yL, function(z){
      #build easy values to name files
      bat <- unique(z$batID)
      date <- unique(date(z$timestamp))
      dayNight <- unique(z$dayNight)
      z$index <- rownames(z)
      write.csv(z, file = paste0("./data/split HR files/", bat,"_", date, "_", dayNight, ".txt"), sep=",", row.names = FALSE)
    })
  })
})

