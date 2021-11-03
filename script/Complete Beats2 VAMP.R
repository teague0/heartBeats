library(tidyverse)
library(lubridate)
library(plotly)
#OPTIONAL finding the MAX, Mean, MIN values of any fH data
locDat[which.min(locDat$A.hr),] %>%
  select(A.hr)  #tells you the location of its data
mean(locDat$A.hr)
median(locDat$A.hr)
max(locDat$BDdur.s, na.rm = TRUE)

#OPTIONAL trying "for" to find a way to make D column have C values
map(locDat$D.locs, ifelse(is.na(), locDat$C.locs)
    
    for(i in 1:length(locDat$D.locs)){
      locDat$D.locs[i] = ifelse(is.na(locDat$D.locs[i]), locDat$C.locs[i], locaDat$D.locs[i])
    }

#reassign index value = row
beats$index <- rownames(beats)

#deleted any E values that were replaced with new E values or just removed A-E because the points were too short duration
deleteE <- c(6106, 6119, 6142, 8650, 8662, 8696, 42500, 70666, 104320, 108607, 110479, 110506, 110614, 110629, 157141, 157159, 157165, 157178, 157200, 157230, 157252, 157263, 301186, 301387, 301507, 301576, 334236, 343066, 469732, 469915, 470262, 470312, 479202, 354168, 479476, 107528, 107580, 107597, 107624)
beats$abBehav[deleteE] <- NA

#Added new data points in to replace previous E or found new drop locations
newA <- c(69191, 483444, 473691, 479174, 311506, 374642,548652)
beats$abBehav[newA] <- "A"

newB <- c(69246, 483507, 473760, 311981, 374871, 548689)
beats$abBehav[newB] <- "B"

newD <- c(69315, 483711, 474082, 312581, 375141, 548704)
beats$abBehav[newD] <- "D"

newE <- c(42526, 69332, 70677, 483739, 104342, 108633, 334389, 343288, 474133, 354181, 312756, 375177, 548734)
beats$abBehav[newE] <- "E"

#Removing Daisy day 8 data
daisyDrop <- which(beats$batID == "daisy" & date(beats$timestamp) == "2014-12-08")
beats2 <- beats
beats2 <- beats2[-daisyDrop,]

#Locate the abBehav change points
A.locs <- which(beats2$abBehav == "A")
E.locs <- which(beats2$abBehav == "E")
B.locs <- which(beats2$abBehav == "B")
D.locs <- which(beats2$abBehav == "D")
C.locs <- which(beats2$abBehav == "C")

#Creating data frames for A and E. Naming BC points as bottom.point. Named D as 
#up.point. Made a  data fram with B and D locs 
locDat <- data.frame(A.locs, E.locs) 
bDat <- data.frame(bottom.point = B.locs, type = 'B')
dDat <- data.frame(up.point = D.locs, type = 'D')
bdDaT <- data.frame(B.locs, D.locs)
cDat <- data.frame(bottom.point = C.locs, type = "C")

#Bound Bdat and Cdat. Bound dDat to cDat. Bound low to up and arranged it from 
# the first bottom point. Got rid of "uneeded" columns. Renamed the columns. Made a dataframe
low <- bind_rows(bDat, cDat)
up <- bind_rows(dDat, cDat)
lowup <- bind_cols(low, up)
lowup <- lowup %>% arrange(bottom.point...1)

names(lowup) <- c("BC.locs", "BCType", "D.locs", "DCType", "C.locs")
locDat <- data.frame(A.locs, lowup, E.locs)

#made a period column
nums <- seq(1:length(locDat$A.locs))
locDat$periodNum <- paste0("p.", nums)

#Put the C&D locations together
locDat$DC.locs <- locDat$D.locs
Ccopy <- which(!is.na(locDat$C.locs))
locDat$DC.locs[Ccopy] <- locDat$C.locs[Ccopy]

#graph OPTIONAL
tmp <- beats[(locDat$A.locs[1]-500):(locDat$E.locs[1]+500),]
ggplot(tmp)+
  geom_line(aes(x = timestamp, y = heartRate.bpm))

#rearrange the columns
col_order <- c("A.locs", "BC.locs", "BCType", "DC.locs", "DCType", "E.locs", "D.locs", "C.locs")
locDat <- locDat[, col_order]

ABreg <- lm(A.locs~BC.locs, data= locDat)
RegAB <- lm(ABheart~as.numeric(ABTime))

ABheart <- beats2$heartRate.bpm[locDat$A.locs[1]:locDat$BC.locs[1]]
ABTime <- beats2$timestamp[locDat$A.locs[1]:locDat$BC.locs[1]]


#setting up calculations for 100 percentage change between D and E 
#used this code for setting heart rate to points in A and B locations
    locDat$A.hr <- beats2$heartRate.bpm[locDat$A.locs]
    locDat$B.hr <- beats2$heartRate.bpm[locDat$BC.locs]
    locDat$D.hr <- beats2$heartRate.bpm[locDat$DC.locs]
    locDat$E.hr <- beats2$heartRate.bpm[locDat$E.locs]
    pDrop.AB <- (locDat$A.hr - locDat$B.hr) / locDat$A.hr*100
    locDat$pDrop.AB <- (locDat$A.hr - locDat$B.hr) / locDat$A.hr*100
    pRise.DE <- (locDat$E.hr - locDat$D.hr) / locDat$E.hr*100
    locDat$pRiseDE <- (locDat$E.hr - locDat$D.hr) / locDat$E.hr*100
#new heart rate Difference for AB and DE
    ABval.D <- (locDat$A.hr - locDat$B.hr)
    locDat$ABval.D <- (locDat$A.hr - locDat$B.hr)
    DEval.D <- (locDat$E.hr - locDat$D.hr)
    locDat$DEval.D <- (locDat$E.hr - locDat$D.hr)

#distance between B and D. Accounting for presence of C
    locDat$BDdur.s <- NA
    for(i in 1:length(locDat$A.locs)){
      if(locDat$BCType[i] == "C"){next} #for C
      t1 <- beats2$timestamp[locDat$BC.locs[i]]
      t2 <- beats2$timestamp[locDat$DC.locs[i]]
      locDat$BDdur.s[i] <- as.numeric(t2) - as.numeric(t1)
    }
#distance between A and E
    for(i in 1:length(locDat$A.locs)){
      t3 <- beats2$timestamp[locDat$A.locs[i]]
      t4 <- beats2$timestamp[locDat$E.locs[i]]
      locDat$AEdur.s[i] <- as.numeric(t4) - as.numeric(t3)
    }
    
#distance between A-B and D-E
for(i in 1:length(locDat$A.locs)){
  if(locDat$BCType[i] == "C"){next}
  t3 <- beats2$timestamp[locDat$A.locs[i]]
  t1 <- beats2$timestamp[locDat$BC.locs[i]]
  locDat$ABdur.s[i] <- as.numeric(t3) - as.numeric(t1)
}
    
for(i in 1:length(locDat$A.locs)){
  t4 <- beats2$timestamp[locDat$E.locs[i]]
  t2 <- beats2$timestamp[locDat$DC.locs[i]]
  locDat$DEdur.s[i] <- as.numeric(t4) - as.numeric(t2)
}

#Change over time and HR
locDat$ABchange.BPS <- locDat$ABval.D / locDat$ABdur.s 
locDat$DEchange.BPS <- locDat$DEval.D / locDat$DEdur.s
    
#EXAMPLE
#    ggplot(locDat)+
 #     geom_boxplot(aes(x = BCType, y = AEdur.s, fill = BCType))
    
#distance between E and the next A
    for(i in 1:length(locDat$A.locs)){
      j = i + 1
      Etime <- beats2$timestamp[locDat$E.locs [i]]
      Atime <- beats2$timestamp[locDat$A.locs [j]]
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
    
#finding points of Joul comsumption
    for(i in 1:length(locDat$A.locs)){
      locDat$jVAL <- beats2$joul[locDat$A.locs [i]]
    }
#making a graph to show difference in A and E heart rate
    
#We need to take out the A & R heart rate data only, then combine them into a column that show the point name (A or E), heart rate, and the period ID
    #Graph of A start HR and E start HR
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
#Adding color and size to customize graph
      geom_line(aes(x=startEnd, y=hr, color= AEdiff, group=periodID), alpha=.5, size = 1) +
      geom_point(aes(x=startEnd, y=hr, color = AEdiff), size = 2, alpha = 0.6)+
      scale_color_viridis_c(option = "D")

#change over time Graph
locDat$periodID <- paste0("per.", rownames(locDat))
TchangeABDE <- locDat %>% select(periodID, ABchange.BPS, DEchange.BPS)
TchangeABDE_L <- TchangeABDE %>% gather(downUp, change, ABchange.BPS:DEchange.BPS)

ggplot(TchangeABDE_L) +
  geom_boxplot(aes(x = downUp, y = abs(change)))
    
    