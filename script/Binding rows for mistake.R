#Bindin rows to find mistake
showA <- beats %>%
  filter(abBehav == "A") %>%
  select(A.index = index)
showA <- as.data.frame(showA)
  
showE <- beats %>%
  filter(abBehav == "E")%>%
  select(E.index = index) 
showE <- as.data.frame(showE)
library(gdata)
ae.df <- cbindX(showE, showA)

showD <- beats %>%
  filter(abBehav == "D") %>%
  select(D.index = index)
showD <- as.data.frame(showD)

showB <- beats %>%
  filter(abBehav == "B")%>%
  select(B.index = index) 
showB <- as.data.frame(showB)
library(gdata)
bd.df <- cbindX(showB, showD)
