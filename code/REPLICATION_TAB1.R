library(dplyr)
library(tidyr)
library(stringr)

data(SeatBelt)

calculate_usage_rate <- function(data, state, start_year, end_year) {
  rate <- data %>%
    filter(state == !!state, year >= start_year, year <= end_year) %>%
    summarize(usage_rate = mean(usage, na.rm = TRUE)) %>%
    pull(usage_rate)
  
  if (is.nan(rate) || is.na(rate)) return(NA)
  paste0(round(rate * 100), "%")
}

get_law_info <- function(data, state) {
  law_data <- data %>%
    filter(state == !!state) %>%
    select(year, ds, dp, dsp) %>%
    mutate(
      secondary = if_else(ds == 1 & dp == 0, year, NA_real_),
      primary = if_else(dp == 1 | dsp == 1, year, NA_real_)
    ) %>%
    summarise(
      secondary = min(secondary, na.rm = TRUE),
      primary = min(primary, na.rm = TRUE)
    )
  
  list(
    secondary = if_else(is.finite(law_data$secondary), as.character(law_data$secondary), NA_character_),
    primary = if_else(is.finite(law_data$primary), as.character(law_data$primary), NA_character_)
  )
}

generate_table <- function(data) {
  states <- unique(data$state)
  
  results <- lapply(states, function(state) {
    law_info <- get_law_info(data, state)
    
    first_law_year <- min(as.numeric(law_info$secondary), as.numeric(law_info$primary), na.rm = TRUE)
    
    tibble(
      State = state,
      `Secondary Enforcement` = law_info$secondary,
      `Primary Enforcement` = law_info$primary,
      `Before Law` = calculate_usage_rate(data, state, first_law_year - 2, first_law_year - 1),
      `Immediately After Law` = calculate_usage_rate(data, state, first_law_year, first_law_year + 1),
      `Before Change` = if (!is.na(law_info$secondary) && !is.na(law_info$primary)) {
        calculate_usage_rate(data, state, 
                             as.numeric(law_info$secondary), 
                             as.numeric(law_info$primary) - 1)
      } else {
        NA_character_
      },
      `Immediately After Change` = if (!is.na(law_info$secondary) && !is.na(law_info$primary)) {
        calculate_usage_rate(data, state, 
                             as.numeric(law_info$primary), 
                             as.numeric(law_info$primary) + 1)
      } else {
        NA_character_
      },
      `In 1997` = calculate_usage_rate(data, state, 1997, 1997)
    )
  })
  
  bind_rows(results) %>%
    arrange(State)
}

result_table <- generate_table(SeatBelt)
print(result_table, n = Inf)