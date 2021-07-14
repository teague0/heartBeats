
#Locate the abBehav change points
A.locs <- which(beats$abBehav == "A")
E.locs <- which(beats$abBehav == "E")
B.locs <- which(beats$abBehav == "B")
D.locs <- which(beats$abBehav == "D")
C.locs <- which(beats$abBehav == "C")

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

#graph
tmp <- beats[(locDat$A.locs[1]-500):(locDat$E.locs[1]+500),]
ggplot(tmp)+
  geom_line(aes(x = timestamp, y = heartRate.bpm))

str(mtcars)
m1 <- lm(mpg~disp, data = mtcars)


#rearrange the columns
col_order <- c("A.locs", "BC.locs", "BCType", "DC.locs", "DCType", "E.locs", "D.locs", "C.locs")
locDat <- locDat[, col_order]

ABreg <- lm(A.locs~BC.locs, data= locDat)
RegAB <- lm(ABheart~as.numeric(ABTime))

ABheart <- beats$heartRate.bpm[locDat$A.locs[1]:locDat$BC.locs[1]]
ABTime <- beats$timestamp[locDat$A.locs[1]:locDat$BC.locs[1]]

