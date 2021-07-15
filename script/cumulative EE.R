dat_df %>% group_by(batID, batDay) %>% 
  filter(hour(timestamp) > 18 | hour(timestamp) < 6) %>% 
  summarize(ee.kJ = sum(joul)/1000)
