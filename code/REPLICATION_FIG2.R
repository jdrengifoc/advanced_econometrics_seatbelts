library(pder)
library(ggplot2)
library(dplyr)
library(tidyr)
data("SeatBelt")
head(SeatBelt)

plot2 <- SeatBelt %>% 
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

# Guardar la gráfica
ggsave(filename = "Replication/figure2.png",
       plot = plot2,
       width = 10, height = 6, dpi = 300,  bg = "white")