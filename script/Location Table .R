
#Locate the abBehav change points
A.locs <- which(beats$abBehav == "A")
E.locs <- which(beats$abBehav == "E")
B.locs <- which(beats$abBehav == "B")
D.locs <- which(beats$abBehav == "D")
C.locs <- which(beats$abBehav == "C")

locDat <- data.frame(A.locs, E.locs) 
bDat <- data.frame(bottom.point = B.locs, type = 'B')
dDat <- data.frame(up.point = D.locs, type = 'D')
bdDaT <- data.frame(B.locs, D.locs)
cDat <- data.frame(bottom.point = C.locs, type = "C")

low <- bind_rows(bDat, cDat)
up <- bind_rows(dDat, cDat)
lowup <- bind_cols(low, up)
lowup <- lowup %>% arrange(bottom.point...1)
lowup$bottom.point...5 <- NULL
lowup$type...4 <- NULL
names(lowup) <- c("BC.locs", "BCType", "D.locs")
locDat <- data.frame(A.locs, lowup, E.locs)

nums <- seq(1:length(locDat$A.locs))
locDat$periodNum <- paste0("p.", nums)

tmp <- beats[(locDat$A.locs[1]-500):(locDat$E.locs[1]+500),]
ggplot(tmp)+
  geom_line(aes(x = timestamp, y = heartRate.bpm))
