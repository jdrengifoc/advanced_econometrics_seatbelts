library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)

data("SeatBelt")
head(SeatBelt)



# Primero, creamos un dataframe con todos los datos procesados
processed_data <- SeatBelt %>% 
  mutate(V = vmtrural + vmturban,
         farsocc_norm = farsocc / V,
         farsnocc_norm = farsnocc / V) %>%
  select(year, farsocc, farsnocc, farsocc_norm, farsnocc_norm) %>% 
  pivot_longer(cols = c(starts_with('fars'), ends_with('_norm')), 
               names_to = 'category', values_to = 'fatalities') %>% 
  group_by(year, category) %>% 
  summarise(fatalities = sum(fatalities), .groups = 'drop') %>% 
  mutate(type = if_else(str_detect(category, "_norm"), "Normalized", "Raw"))

# Ahora calculamos los factores de escala fuera de ggplot
max_raw <- max(processed_data$fatalities[processed_data$type == "Raw"])
max_normalized <- max(processed_data$fatalities[processed_data$type == "Normalized"])
scale_factor <- max_raw / max_normalized

# Ahora creamos el gráfico
ggplot(processed_data, aes(x = year, y = fatalities, color = category, linetype = type)) +
  geom_line(data = ~ filter(., type == "Raw")) +
  geom_line(data = ~ filter(., type == "Normalized"), aes(y = fatalities * scale_factor)) +
  scale_x_continuous(breaks = seq(1983, 1998, by = 1)) +
  scale_y_continuous(name = "Number of fatalities",
                     sec.axis = sec_axis(~ . / scale_factor, name = "Normalized fatalities")) +
  labs(x = "Year",
       title = "FIGURE 1.—TOTAL OCCUPANT AND NONOCCUPANT FATALITIES",
       subtitle = "Raw and Normalized by Total Vehicle Miles Traveled") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.y.right = element_text(color = "darkgreen"),
        axis.text.y.right = element_text(color = "darkgreen")) +
  scale_color_manual(values = c("farsocc" = "blue", "farsnocc" = "red",
                                "farsocc_norm" = "darkgreen", "farsnocc_norm" = "lightgreen"),
                     labels = c("Occupant", "Non-occupant", 
                                "Occupant (Normalized)", "Non-occupant (Normalized)")) +
  scale_linetype_manual(values = c("Raw" = "solid", "Normalized" = "dashed"))