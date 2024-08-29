library(pder)
library(ggplot2)
library(dplyr)
library(tidyr)
data("SeatBelt")
head(SeatBelt)

SeatBelt %>% 
  select(year, farsocc, farsnocc) %>% 
  pivot_longer(cols = starts_with('fars'), 
               names_to = 'category', values_to = 'fatalities') %>% 
  group_by(year, category) %>% 
  summarise(fatalities = sum(fatalities)) %>% 
  ggplot(aes(x = year, y = fatalities, color = category)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1983, 1998, by = 1)) +
  scale_y_continuous(breaks = seq(0, 45e3, by = 5e3), 
                     limits = c(0, 45e3)) +
  labs(x = "Year", y = "Number of fatalities",
       title = "FIGURE 1.—TOTAL OCCUPANT AND NONOCCUPANT FATALITIES") +
  theme_minimal() +
  theme()

SeatBelt %>% 
  select(year, usage) %>% 
  group_by(year) %>% 
  summarise(usage = mean(usage, na.rm = T)) %>% 
  ggplot(aes(x = year, y = usage)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1983, 1998, by = 1)) +
  scale_y_continuous(breaks = seq(0, 0.9, by = 0.1), 
                     limits = c(0, 0.9)) +
  labs(x = "Year", y = "Usage rate",
       title = 'FIGURE 2.—AVERAGE SEAT BELT USAGE OVER TIME') +
  theme_minimal() +
  theme()

