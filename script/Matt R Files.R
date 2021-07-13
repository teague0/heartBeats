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
 
  #repost
  A.locs <- which(beats$abBehav == "A")
  E.locs <- which(beats$abBehav == "E")
  B.locs <- which(beats$abBehav == "B")
  D.locs <- which(beats$abBehav == "D")
  C.locs <- which(beats$abBehav == "C")
  locDat <- data.frame(A.locs, E.locs) 
  bDat <- data.frame(bottom.point = B.locs, type = 'B')
  dDat <- data.frame(type = 'D')
  bdDaT <- data.frame(B.locs, D.locs)
  cDat <- data.frame(bottom.point = C.locs, type = "C")

  
  #trying "for" to find a way to make D column have C values
  for (BCType == "C") 
    print(string, D.locs)
  } else if (D.locs = NA) +
    seek(value -> BCType)
    }
rows.in.BC.locs.that.are.not.in.D.locs(BC.locs, D.locs)
within(locDat, D.locs[is.NA(n)] <- BC.locs[is.value(n)])

df <- sample(1:100, 33 * 332, replace = TRUE) %>%
  matrix(ncol = 33) %>%
  as.data.frame()
y <- map(3:27,
         function(i) {
           map(1:272,
               function(j, i) {
                 map_dbl(28:33,
                         function(k, j, i){lm(df[j:(60+j), i] ~ df[j:(60+j), k])[[1]][2]}, 
                         j = j, i = i)
               }, i = i)
         })