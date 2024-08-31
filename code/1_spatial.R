library(sf)
library(gridExtra)

library(pder)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stargazer)

root_folder <- '1_panel_data/advanced_econometrics_seatbelts'
data("SeatBelt")
shp_STATES <- read_sf(file.path(root_folder, 'data/cb_2018_us_state_5m')) %>% 
  rename(state = STUSPS) %>% filter(! state %in% c('HI', 'AK'))
data <- shp_STATES %>% left_join(SeatBelt, by = 'state') %>% 
  mutate(vmt = vmtrural + vmturban,
         farsocc = farsocc / vmt,
         farsnocc = farsnocc / vmt) %>% select(-vmt)

outcomes <- c('farsocc', 'farsnocc', 'usage')
selected_years <- quantile(SeatBelt$year, c(0, 0.5, 1))
titles <- c('farsocc' = "Occupant fatalities per VMT",
            'farsnocc' = "Non-occupant fatalities per VMT",
            'usage' = 'Rate of seat belt usage')
legend_title <- c(
  'farsocc' = "Occupant fatalities per VMT",
  'farsnocc' = "Non-occupant fatalities per VMT",
  'usage' = 'Rate of seat belt usage')
colors_min <- c('farsocc' = '#FFFFFF',
               'farsnocc' = '#FFFFFF',
               'usage' = '#D10000')
colors_max <- c('farsocc' = '#D10000',
               'farsnocc' = '#D10000',
               'usage' = '#FFFFFF')

for (outcome in outcomes) {
  maps <- NULL
  for (selected_year in selected_years) {
    df_plot <- data %>% filter(year == selected_year)
    outcome_range <- range(df_plot[[outcome]], na.rm = T)
    outcome_min <- min(df_plot[[outcome]], na.rm = T)
    df_plot <- df_plot %>% 
      mutate(!!sym(outcome) := (!!sym(outcome) - outcome_min) / diff(outcome_range))
    print(range(df_plot[[outcome]]))
    params <- list(
      title = sprintf('%s in %d', titles[[outcome]], selected_year),
      subtitle = NULL,
      legend.title = NULL,
      # caption = sprintf("Fatalaties per VMT between %s.", outcome_range %>% 
      #                     round(4) %>% paste(collapse = ' and ')),
      caption = NULL,
      color_min = colors_min[[outcome]], color_max = colors_max[[outcome]], color_na = 'grey'
    )
    
    map <- df_plot %>% 
      ggplot() +
      geom_sf(aes(fill = !!sym(outcome)), lwd = 0, color = NA) +
      scale_fill_gradientn(colours = c(params$color_min, params$color_max),
                           name = params$legend.title, na.value = params$color_na) +
      # guides(fill = guide_colorbar(label = F)) +
      labs(title = params$title,
           subtitle = params$subtitle,
           caption = params$caption) +
      theme(
        panel.spacing.x = unit(8, 'mm'),
        panel.spacing.y = unit(5, 'mm'),
        text = element_text(family = "sans", color = "grey20"),
        panel.background = element_blank(),
        strip.text = element_text(size=10, face = 'bold', hjust = 0.5),
        plot.margin = unit(1*c(1,1,1,1),"cm"),
        plot.caption = element_text(size = 12, colour = "grey30", hjust = -0.1, vjust = -5),
        plot.subtitle = element_text(size = 12, face = "italic", colour = "grey40", hjust = 0.5),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 12, face="bold"),
        legend.text = element_text(size = 10),
        legend.position = 'None',
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()
      )
      if (selected_year == tail(selected_years, 1)) {
        map <- map + theme(legend.position = 'right')
      }
    map
    ggsave(filename = sprintf('%s/results/figures/map_%s-%d.png',
                              root_folder, outcome, selected_year),
           width = 10, height = 6, dpi = 300, bg = "white")
  }
}

SeatBelt %>% group_by(year) %>% summarise(sum(is.na(usage)))
