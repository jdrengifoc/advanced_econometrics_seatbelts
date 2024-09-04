library(pder)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(stargazer)

root_folder <- '1_panel_data/advanced_econometrics_seatbelts'
data("SeatBelt")

# FIGURE 1.—TOTAL OCCUPANT AND NONOCCUPANT FATALITIES ---------------------
processed_data <- SeatBelt %>% 
  mutate(vmt = vmtrural + vmturban,
         farsocc_norm = farsocc / vmt,
         farsnocc_norm = farsnocc / vmt) %>%
  select(year, farsocc, farsnocc, farsocc_norm, farsnocc_norm) %>% 
  pivot_longer(cols = starts_with('fars'), 
               names_to = 'category', values_to = 'fatalities') %>% 
  group_by(year, category) %>% 
  summarise(fatalities = sum(fatalities), .groups = 'drop') %>% 
  mutate(
    type = if_else(str_detect(category, "_norm"), "Normalized", "Raw"),
    category = str_replace(category, '_norm', ''))

# Calculamos los factores de escala fuera de ggplot
max_raw <- max(processed_data$fatalities[processed_data$type == "Raw"])
max_normalized <- max(processed_data$fatalities[processed_data$type == "Normalized"])
scale_factor <- max_raw / max_normalized

# Creamos el gráfico
ggplot(processed_data, aes(x = year, y = fatalities, color = category, linetype = type)) +
  geom_line(data = ~ filter(., type == "Normalized")) +
  geom_line(data = ~ filter(., type == "Raw"), aes(y = fatalities / scale_factor)) +
  scale_x_continuous(breaks = seq(1983, 1998, by = 1)) +
  scale_y_continuous(
    breaks = seq(0, 1.25, by = 0.25), limits = c(0, 1.25),
    name = "Fatalities per MVV",
    sec.axis = sec_axis(
      ~ . * scale_factor, name = "Number of fatalities",
      breaks = seq(0, 45e3, by = 5e3)
      )
    ) +
  labs(x = "Year") +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    ) +
  scale_color_manual(values = c("farsocc" = "#47D134", "farsnocc" = "#922FD6"),
                     labels = c("Non-occupant", "Occupant",
                                "Normalized", "Number"))



# FIGURE 2 ----------------------------------------------------------------

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
       title = 'FIGURE 2. AVERAGE SEAT BELT USAGE OVER TIME') +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  )


# FIgure 1 + 2 ------------------------------------------------------------

processed_data <- SeatBelt %>% 
  mutate(vmt = vmtrural + vmturban,
         farsocc_norm = farsocc / vmt,
         farsnocc_norm = farsnocc / vmt) %>%
  select(year, farsocc, farsnocc, farsocc_norm, farsnocc_norm) %>% 
  pivot_longer(cols = starts_with('fars'), 
               names_to = 'category', values_to = 'fatalities') %>% 
  group_by(year, category) %>% 
  summarise(fatalities = sum(fatalities), .groups = 'drop') %>% 
  mutate(
    type = if_else(str_detect(category, "_norm"), "Normalized", "Raw"),
    category = str_replace(category, '_norm', '')) %>% 
  bind_rows(
    SeatBelt %>% 
      select(year, usage) %>% 
      group_by(year) %>% 
      summarise(usage = mean(usage, na.rm = T)) %>% 
      mutate(type = 'usage', category = 'usage') %>% rename(fatalities = usage)
  )

# Calculamos los factores de escala fuera de ggplot
max_raw <- max(processed_data$fatalities[processed_data$type == "Raw"])
max_normalized <- max(processed_data$fatalities[processed_data$type == "Normalized"])
scale_factor <- max_raw / max_normalized

# Creamos el gráfico
ggplot(processed_data, aes(x = year, y = fatalities, color = category, linetype = type)) +
  geom_line(data = ~ filter(., type == "usage"), linetype = 'solid') +
  geom_line(data = ~ filter(., type == "Normalized")) +
  geom_line(data = ~ filter(., type == "Raw"), aes(y = fatalities / scale_factor)) +
  scale_x_continuous(breaks = seq(1983, 1998, by = 1)) +
  scale_y_continuous(
    breaks = seq(0, 1.25, by = 0.25), limits = c(0, 1.25),
    name = "Fatalities per MVV / ASU",
    sec.axis = sec_axis(
      ~ . * scale_factor, name = "Number of fatalities",
      breaks = seq(0, 45e3, by = 5e3)
    )
  ) +
  labs(x = "Year") +
  theme_minimal(14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c("farsocc" = "#47D134", "farsnocc" = "#922FD6", 'usage' = 'black'),
                     labels = c("Non-occupant", "Occupant", "Average seatbelt usage (ASU)")) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),    # Agrupa en dos filas
    linetype = guide_legend(nrow = 2, byrow = TRUE)  # Agrupa en dos filas
  )
  ggsave(filename = file.path(root_folder, 'results/figures/motivation_changes.png'),
         width = 10, height = 6, dpi = 300, bg = "white")


# FIGURE 3 ----------------------------------------------------------------

SeatBelt %>% 
  select(year, ds, dp, dsp) %>%
  group_by(year) %>% 
  summarise(ds = sum(ds - dsp), dp = sum(dp), dsp = sum(dsp)) %>% ungroup %>% 
  mutate(dnl = 51 - ds - dp - dsp) %>% 
  pivot_longer(cols = starts_with('d'), 
               names_to = 'category', values_to = 'value') %>% 
  mutate(category = recode(category, 
                           dnl = "No law",
                           dp = "Primary enforcement",
                           ds = "Secondary enforcement",
                           dsp = "Primary preeced by secondary"
                           )) %>% 
  ggplot(aes(x = as.factor(year), y = value, fill = category)) +
  geom_bar(stat = 'identity', width = 0.5) +
  scale_fill_manual(values = c(
    "No law" = "#CECECE",
    "Primary enforcement" = "#005AB4",
    "Primary preeced by secondary" = "#00C8E7",
    "Secondary enforcement" = "#FFD6A2"
  )) +
  scale_y_continuous(minor_breaks = seq(0, 51, 2)) +
  labs(x = "Year", y = "No. of States") +
  theme_minimal(14) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave(filename = file.path(root_folder, 'results/figures/motivation_legislation.png'),
       width = 10, height = 6, dpi = 300, bg = "white")

  # Panel regressions -------------------------------------------------------
processed_data <- SeatBelt %>% 
  mutate(vmt = vmtrural + vmturban,
         farsocc = farsocc / vmt,
         farsnocc = farsnocc / vmt) %>% 
  select(-vmt) %>% pdata.frame(c("state","year"))

# Tables 2-3 --------------------------------------------------------------

possible_covariates <- c(
  "Seat belt usage", "Log(seat belt usage)", "Log(median income)", 
  "Log(unemployment rate)", "Log(mean age)", "Log(% blacks)", 
  "Log(% Hispanics)", "Log(traffic density urban)", 
  "Log(traffic density rural)", "Log(violent crimes)", "Log(property crimes)", 
  "Log(VMT rural)", "Log(VMT urban)", "Log(fuel tax)", "65-mph speed limit", 
  "70-mph speed limit or above", "MLDA of 21 years", "BAC 0.08")
log_controls <- c('percapin', 'unemp', 'meanage', 'precentb', 'precenth', 
                  'densurb', 'densrur', 'viopcap', 'proppcap', 'vmtrural',
                  'vmturban', 'fueltax')
dummy_controls <- c('lim65', 'lim70p', 'mlda21', 'bac08')
instruments <- c('ds', 'dp', 'dsp')

outcomes <- c('farsocc', 'farsnocc')
logaritmics <- c('original' = F, log = T)
# Preallocate tests.
sig_levels <- c(0.01, 0.05, 0.1)
tests <- list(relevant_instruments = NULL, hausman = NULL)

for (outcome in outcomes) {
  for (i in 1:length(logaritmics)) {
    logaritmic <- logaritmics[i]
    # Process data for regression.
    reg_data <- processed_data %>%
      mutate(across(all_of(log_controls), log))
    if (logaritmic) {
      reg_data <- reg_data %>% 
        mutate(across(all_of(c('usage', outcome)), log))
    }
    # Write regression formulas.
    formula <- sprintf('%s ~ usage + %s + %s -1', 
                       outcome, 
                       paste(log_controls, collapse = ' + '),
                       paste(dummy_controls, collapse = ' + '))
    formula_iv <- sprintf('%s | %s + %s + %s', 
                          formula,
                          paste(instruments, collapse = ' + '),
                          paste(log_controls, collapse = ' + '),
                          paste(dummy_controls, collapse = ' + '))
    
    # OLS
    reg_ols <- plm(formula = formula, data = reg_data,
                   model = "within", effect = "time")
    # Random Effects
    reg_re <- plm(formula = formula, data = reg_data,
                  model = "random")
    # Fixed Effects
    reg_fe <- plm(formula = formula, data = reg_data,
                  model = "within", effect = "twoway")
    # IV
    reg_iv <- plm(formula = formula_iv, data = reg_data,
                  model = "within", effect = "twoway")
    
    # Random vs Fixed Effects.
    hausman_test <- phtest(reg_fe, reg_re)[['p.value']] <= sig_levels
    tests$hausman <- rbind(tests$hausman, hausman_test)
    # Relevant instruments.
    formula_instruments_test <- sprintf(
      'usage ~ %s + %s + %s', 
      paste(instruments, collapse = ' + '),
      paste(log_controls, collapse = ' + '),
      paste(dummy_controls, collapse = ' + ')
    )
    
    relevant_instruments_test <- car::linearHypothesis(
      lm(formula = formula_instruments_test, data = reg_data), 
      hypothesis.matrix = paste0(instruments, ' = 0')
    )[['Pr(>F)']][-1] <= sig_levels
    tests$relevant_instruments <- rbind(tests$relevant_instruments, 
                                        relevant_instruments_test)
    
    # Save results.
    stargazer(reg_ols, reg_fe, reg_iv,
              dep.var.labels = outcome,
              covariate.labels = possible_covariates[-which(logaritmic == logaritmics)],
              column.labels = c("OLS", "Fixed Effects", "IV"),
              omit.stat = c("f", "ser", 'rsq'),
              add.lines = list(c("Year FE", "Yes", "Yes", "Yes"),
                               c("State FE", "No", "Yes", "Yes")),
              digits = 4,
              type = "latex",
              out = sprintf('%s/results/tables/%s-%s.tex',
                            root_folder, outcome, names(logaritmic))
    ) 
  }
}


# TABLE 4 -----------------------------------------------------------------
reg_data <- processed_data %>%
  mutate(across(all_of(log_controls), log))
outcome <- 'usage'
formula <- sprintf('%s ~ %s + %s + %s -1', 
                   outcome,
                   paste(instruments, collapse = ' + '),
                   paste(log_controls, collapse = ' + '),
                   paste(dummy_controls, collapse = ' + '))

# OLS
reg_ols <- plm(formula = formula, data = reg_data,
               model = "within", effect = "time")
# Random Effects
reg_re <- plm(formula = formula, data = reg_data, model = "random")
# Fixed Effects
reg_fe <- plm(formula = formula, data = reg_data,
              model = "within", effect = "twoway")
reg_fe$coefficients %>% names %>% length
possible_covariates <- c(
  "Secondary enforcement", "Primary enforcement", 
  "Secondary to primary enforcement", "Log(median income)", 
  "Log(unemployment rate)", "Log(mean age)", "Log(% blacks)", 
  "Log(% Hispanics)", "Log(traffic density urban)", 
  "Log(traffic density rural)", "Log(violent crimes)", "Log(property crimes)", 
  "Log(VMT rural)", "Log(VMT urban)", "Log(fuel tax)", "65-mph speed limit", 
  "70-mph speed limit or above", "MLDA of 21 years", "BAC 0.08")

possible_covariates %>% length
stargazer(reg_ols, reg_re, reg_fe,
          dep.var.labels = outcome,
          covariate.labels = possible_covariates,
          column.labels = c("OLS", "Random Effects", "Fixed Effects"),
          omit.stat = c("f", "ser", 'rsq'),
          add.lines = list(c("Year FE", "Yes", "No", "Yes"),
                           c("State FE", "No", "No", "Yes"),
                           c("Random Effects", "No", "Yes", "No")),
          digits = 4,
          type = "latex",
          out = file.path(root_folder, 'results/tables/seatbelt_usage.tex')
) 

# Haussman test
phtest(reg_fe, reg_re)[['p.value']]


