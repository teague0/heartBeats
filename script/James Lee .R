# The first test files from local computers.
# Ideas for pulling the data 
# Use the gap_dfs then using the map function we can maybe pluck the data or 
# pull multiple variables
# ex. pluck(coef(usa_mod2), "year") - pluck(coef(usa_mod1), "year")
# That is asking how much the year coefficient changed between the two models

gap_list %>% map("abBehav")
``