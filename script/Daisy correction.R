#deleted any E values that were replaced with new E values or just removed A-E because the points were too short duration
deleteE <- c(6106, 6119, 6142, 8650, 8662, 8696, 42500, 70666, 104320, 108607, 110479, 110506, 110614, 110629, 157141, 157159, 157165, 157178, 157200, 157230, 157252, 157263, 301186, 301387, 301507, 301576, 334236, 343066, 469732, 469915, 470262, 470312, 479202, 354168, 107528, 107580, 107597, 107624)
beats$abBehav[deleteE] <- NA

#Added new data points in to replace previous E or found new drop locations
newA <- c(69191, 479388, 483444, 473691, 479174, 311506, 374642,548652)
beats$abBehav[newA] <- "A"

newB <- c(69246, 479491, 483507, 473760, 311981, 374871, 548689)
beats$abBehav[newB] <- "B"

newD <- c(69315, 479476, 483507, 474082, 312581, 375141, 548704)
beats$abBehav[newD] <- "D"

newE <- c(42526, 69332, 70677, 479775, 483739, 104342, 108633, 334389, 343288, 474133, 354181, 312756, 375177, 548734)
beats$abBehav[newE] <- "E"

#Removed daisy day 8 because it is a duplicate
daisyDrop <- which(beats$batID == "daisy" & date(beats$timestamp) == "2014-12-08")
beats2 <- beats
beats2 <- beats2[-daisyDrop,]

save(beats2, file="./data/HeartBeatDataADJ2021-08-11.Rdata")
