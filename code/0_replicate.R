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


library(stringr)
SeatBelt %>% 
  mutate(V = vmtrural + vmturban,
         farsocc_norm = farsocc / V,
         farsnocc_norm = farsnocc / V) %>%
  select(year, farsocc, farsnocc, farsocc_norm, farsnocc_norm) %>% 
  pivot_longer(cols = c(starts_with('fars'), ends_with('_norm')), 
               names_to = 'category', values_to = 'fatalities') %>% 
  group_by(year, category) %>% 
  summarise(fatalities = sum(fatalities), .groups = 'drop') %>% 
  mutate(type = if_else(str_detect(category, "_norm"), "Normalized", "Raw")) %>%
  ggplot(aes(x = year, y = fatalities, color = category, linetype = type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1983, 1998, by = 1)) +
  scale_y_continuous(trans = 'log10',
                     labels = scales::label_number(accuracy = 0.1),
                     breaks = scales::breaks_log()) +
  labs(x = "Year", y = "Number of fatalities (log scale)",
       title = "FIGURE 1.—TOTAL OCCUPANT AND NONOCCUPANT FATALITIES",
       subtitle = "Raw and Normalized by Total Vehicle Miles Traveled") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("farsocc" = "blue", "farsnocc" = "red",
                                "farsocc_norm" = "lightblue", "farsnocc_norm" = "pink"),
                     labels = c("Occupant", "Non-occupant", 
                                "Occupant (Normalized)", "Non-occupant (Normalized)")) +
  scale_linetype_manual(values = c("Raw" = "solid", "Normalized" = "dashed"))


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

