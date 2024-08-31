library(plm)
library(dplyr)
pp <- make.pbalanced(SeatBelt %>% na.omit(), balance.type = "shared.individuals",
                     index = c('state', 'year'))
SeatBelt$year %>% table

SeatBelt %>% 
  filter(is.na(usage)) %>% 
  group_by(year) %>% summarise(n_ = n())
