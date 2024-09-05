
# Packages -----------------------------------------------------------------

packages <- c("sp", "splm", "spdep", "plm", "sf", "pder", "ggplot2","viridis","tools", 'dplyr', 'tidyr', 'stargazer')
              # "RColorBrewer", "classInt", "lattice", "summarytools", "rgeoda", 
              # "tidylog", "spatialreg", "ggpubr","ggplot2","viridis","tools","tidyr")

invisible(lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))


# Functions ---------------------------------------------------------------

get_formula <- function(outcome, controls, instruments = "") {
  if (outcome == 'usage') {
    formula <- sprintf('%s ~ %s + %s - 1', 
                       outcome,
                       paste(instruments, collapse = ' + '),
                       paste(controls, collapse = ' + '))
  } else {
    formula <- sprintf('%s ~ usage + %s - 1', 
                       outcome, paste(controls, collapse = ' + '))
  }
  return(as.formula(formula))
}

run_panel_regressions <- function(data, outcome, controls, We, 
                                  fe_effect = "twoway") {
  
  print("Starting function")
  formula <- get_formula(outcome, controls)
  # Create SDM and SDEM specific variables.
  for (control in controls) {
    new_column <- paste0("w", control)
    data[[new_column]] <- slag(data[[control]], We, 1)
  }
  # Modify formula for SDM and SDEM.
  sdm_formula <- get_formula(outcome, c(controls, paste0("w", controls)))
  
  # Preallocate.
  results <- list()
  
  # Función auxiliar para ejecutar modelos y manejar errores
  run_model <- function(model_name, model_function, args) {
    print(paste("Running", model_name))
    tryCatch({
      model <- do.call(model_function, args)
      results[[model_name]] <<- model
      print(paste(model_name, "completed successfully"))
    }, error = function(e) {
      print(paste("Error in", model_name, ":", e$message))
      results[[model_name]] <<- NULL
    })
  }
  
  # Regresiones de panel sin efectos espaciales
  run_model("pooling", plm, list(formula = formula, data = data, model = "pooling"))
  run_model("fe", plm, list(formula = formula, data = data, model = "within", effect = fe_effect))
  run_model("re", plm, list(formula = formula, data = data, model = "random", effect = "individual"))
  
  # Regresiones de panel con efectos espaciales (efectos fijos)
  # Añadimos un try-catch adicional para manejar errores de singularidad
  tryCatch({
    run_model( "sarfe", spml, list(formula = formula, data = data, listw = We, spatial.error = "none", 
                                   lag = T, model = "within", effect = fe_effect, method = "eigen"))
  }, error = function(e) {
    print(paste("Error in sarfe:", e$message))
  })
  
  tryCatch({
    run_model("semfe", spml, list(formula = formula, data = data, listw = We, spatial.error = "b", 
                                  lag = F, model = "within", effect = fe_effect, method = "eigen"))
  }, error = function(e) {
    print(paste("Error in semfe:", e$message))
  })
  
  tryCatch({
    run_model("sararfe", spml, list(formula = formula, data = data, listw = We, lag = TRUE, 
                                    spatial.error = "b", model = "within", effect = fe_effect, method = "eigen"))
  }, error = function(e) {
    print(paste("Error in sararfe:", e$message))
  })
  
  run_model("sdmfe", spml, list(formula = sdm_formula, data = data, listw = We, lag = TRUE,
                                spatial.error = "none", model = "within", effect = fe_effect, method = "eigen"))
  run_model("sdemfe", spml, list(formula = sdm_formula, data = data, listw = We, lag = FALSE,
                                 spatial.error = "b", model = "within", effect = fe_effect, method = "eigen"))
  run_model("slxfe", plm, list(formula = sdm_formula, data = data, model = "within", effect = fe_effect))
  
  # Regresiones de panel con efectos espaciales (efectos aleatorios)
  # Añadimos try-catch para manejar errores de singularidad
  tryCatch({
    run_model("sarre", spml, list(formula = formula, data = data, listw = We, spatial.error = "none", lag = TRUE, model = "random"))
  }, error = function(e) {
    print(paste("Error in sarre:", e$message))
  })
  
  tryCatch({
    run_model("semre", spml, list(formula = formula, data = data, listw = We, lag = FALSE, spatial.error = "b", model = "random"))
  }, error = function(e) {
    print(paste("Error in semre:", e$message))
  })
  
  tryCatch({
    run_model("sararre", spml, list(formula = formula, data = data, listw = We, lag = TRUE, spatial.error = "b", model = "random"))
  }, error = function(e) {
    print(paste("Error in sararre:", e$message))
  })
  
  run_model("sdmre", spml, list(formula = sdm_formula, data = data, listw = We, lag = TRUE, spatial.error = "none", model = "random"))
  run_model("sdemre", spml, list(formula = sdm_formula, data = data, listw = We, lag = FALSE, spatial.error = "b", model = "random"))
  run_model("slxre", plm, list(formula = sdm_formula, data = data, model = "random"))
  
  print("Function completed")
  return(results)
}

# Para ver todos los resúmenes de los modelos que se ejecutaron con éxito:
print_summary <- function(results, name) {
  cat("\n\n--- RESULTADOS", name, "---\n")
  for (model_name in names(results)) {
    if (!is.null(results[[model_name]])) {
      cat("\n\n--- Summary for", model_name, "---\n")
      print(summary(results[[model_name]]$model))
    }
  }
}


# Data --------------------------------------------------------------------
root_folder <- '1_panel_data/advanced_econometrics_seatbelts'
# Load US map data
us <- st_read(file.path(root_folder, "data/cb_2018_us_state_5m/cb_2018_us_state_5m.shp")) %>% 
  filter(!is.na(STUSPS) & !STUSPS %in% c('AK','HI','GU','AS','PR','VI', 'MP')) %>%
  mutate(fipsstat = as.numeric(STATEFP)) %>% 
  arrange(fipsstat) %>%  
  st_transform(crs = 4326) %>% 
  rename(state = STUSPS)

data("SeatBelt")

# Parameters
logaritmics <- c(TRUE, FALSE)
outcomes <- c('farsnocc', 'farsocc', 'usage')
log_controls <- c('percapin', 'unemp', 'densurb', 'densrur', 'proppcap', 'vmturban', 'fueltax')
dummy_controls <- c()
log_controls <- c('percapin', 'unemp', 'meanage', 'precentb', 'precenth', 
                  'densurb', 'densrur', 'viopcap', 'proppcap', 'vmtrural',
                  'vmturban', 'fueltax')
dummy_controls <- c('lim65', 'lim70p', 'mlda21', 'bac08')[-c(3)]
controls <- c(log_controls, dummy_controls)
instruments <- c('ds', 'dp', 'dsp')

# Process data.
datapanel <- SeatBelt %>% 
  mutate(code = as.integer(factor(state)),
         vmt = vmtrural + vmturban,
         farsocc = farsocc / vmt,
         farsnocc = farsnocc / vmt) %>% 
  filter(!is.na(usage)) %>%
  filter(year > 1989, !state %in% c('AK', 'HI')) %>%
  pdata.frame(c("state","year")) %>% 
  mutate(across(all_of(log_controls), log))
summary(datapanel)


# Regression models -------------------------------------------------------

# First stage instrumental variable.
model_fs <- lm(get_formula('usage', controls, instruments), data = datapanel)
datapanel$usage <- model_fs$fitted.values

# Summary Statistics
summary(model_fs)
stargazer(datapanel, type = "text")
stargazer(datapanel[, c("farsocc","farsnocc","usage")], type = "text", 
          covariate.labels = c("Fatalities Occupants", "Fatalities Non Occupants", "Seat Belt Usage"))


# Create spatial weights matrix
nb <- poly2nb(us, queen = T) %>% print()
We <- nb2listw(nb, style = "W") %>% print()
fe_effect <- 'twoways'
results <- list()
for (outcome in outcomes[-3]) {
  for (logaritmic in logaritmics) {
    if (logaritmic) {
      data_reg <- datapanel %>% 
        mutate(across(all_of(outcomes), log))
    } else {
      data_reg <- datapanel
    }
    results_name <- gsub(replacement = '', pattern = '^_', 
                         paste(c('log')[logaritmic], outcome, sep = '_'))
    results[[results_name]] <- run_panel_regressions(data_reg, outcome, controls, We, fe_effect = fe_effect)
    results[[results_name]][['outcome']] <- outcome
    results[[results_name]][['loglog']] <- logaritmic
  }
}

# Hausman -----------------------------------------------------------------

sig_levels <- c(1, 5, 10) / 100
models <- c('ols', 'sar', 'sem', 'sarar', 'sdm', 'sdem', 'slx')
df_hausman <- NULL
for (result_name in names(results)) {
  ls <- results[[result_name]]
  pvalues <- c(
    phtest(ls$re, ls$fe)[['p.value']],
    sphtest(x = ls$sarre, x2 = ls$sarfe)[['p.value']][1],
    sphtest(x = ls$semre, x2 = ls$semfe)[['p.value']][1],
    sphtest(x = ls$sararre, x2 = ls$sararfe)[['p.value']][1],
    sphtest(x = ls$sdmre, x2 = ls$sdmfe)[['p.value']][1],
    sphtest(x = ls$sdemre, x2 = ls$sdemfe)[['p.value']][1],
    phtest(x = ls$slxre, x2 = ls$slxfe)[['p.value']][1]
  ) 
  df0 <- data.frame(
    outcome = ls$outcome, loglog = ls$loglog, models, p_value = pvalues)
  df_hausman <- rbind(df_hausman, df0)
}
df_hausman <- df_hausman %>% 
  mutate(significance = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    p_value <= 0.1 ~ ".",
    .default = ""
  ))

# LM test -----------------------------------------------------------------

lm_tests <- c('lml', 'lme', 'rlml', 'rlme')
models <- c("within", "random")
effects <- c("individual", "time", "twoways")
df <- NULL
for (outcome in outcomes[-3]) {
  for (logaritmic in logaritmics) {
    if (logaritmic) {
      data_reg <- datapanel %>% 
        mutate(across(all_of(outcomes), log))
    } else {
      data_reg <- datapanel
    }
    for (model in models) {
      for (effect in effects) {
        if (model == 'random' & effect %in% c('time', 'twoways')){
          next
        }
        for (lm_test in lm_tests) {
          ls <- slmtest(get_formula(outcome, controls), data = data_reg, listw = We,
                        test = lm_test, model = model, effect = effect)
          df0 <- data.frame(outcome = outcome, loglog = logaritmic, 
                            model = model, effect = effect, test = lm_test, 
                            H1 = ls$alternative, p_value = ls$p.value[[1]])
          df <- rbind(df, df0)
        }
      }
    }
  }
}

df_lm_test <- df %>% 
  mutate(significance = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    p_value <= 0.1 ~ ".",
    .default = ""
  )) %>% filter(loglog) %>% arrange(model, effect)

results$log_farsnocc$semfe %>% summary
results$log_farsocc$fe %>% summary

# results -----------------------------------------------------------------
lr_test <- function(unrestricted_model, restricted_model) {
  get_df <- function(model) {
    length(model$residuals) - length(model$coefficients)
  }
  
  get_ll <- function(model) {
    logLik.plm <- function(object){
      # https://stackoverflow.com/questions/64186453/r-extract-log-likelihood-from-a-plm-object
      -plm::nobs(object) * log(2 * var(object$residuals) * pi)/2 - deviance(object)/(2 * var(object$residuals))
    }
   if ('logLik' %in% names(model)) {
     ll <- model$logLik
   } else {
     ll <- logLik.plm(model)
   }
  return(ll)
  }
  # H0: modelo no restringido
  # H1: modelo restringido
  ll_unrestricted <- get_ll(unrestricted_model)
  ll_restricted <- get_ll(restricted_model)
  lr_statistic <- - 2 * (ll_restricted - ll_unrestricted)
  freedom_degrees <- abs(get_df(restricted_model) - get_df(unrestricted_model))
  pchisq(lr_statistic, df = freedom_degrees, lower.tail = FALSE)
  
}

logaritmic <- T
if (logaritmic) {
  data_reg <- datapanel %>% 
    mutate(across(all_of(outcomes), log))
} else {
  data_reg <- datapanel
}
## FE
lr_test(results$log_farsnocc$sdmfe, results$log_farsnocc$sarfe)
lr_test(results$log_farsnocc$sdmfe, results$log_farsnocc$slxfe)
lr_test(results$log_farsnocc$sdmfe, results$log_farsnocc$semfe)
# Seleccionamos SDM para no ocupados
results$log_farsnocc$sdmfe %>% summary
impacts(results$log_farsnocc$sdmfe, listw = We, 
        time = length(unique(data_reg$year)))

lr_test(results$log_farsocc$sdmfe, results$log_farsocc$sarfe)
lr_test(results$log_farsocc$sdmfe, results$log_farsocc$slxfe)
lr_test(results$log_farsocc$sdmfe, results$log_farsocc$semfe)
lr_test(results$log_farsocc$slxfe, results$log_farsocc$fe)
# Seleccionamos SLX para ocupados
results$log_farsocc$slxfe %>% summary


# Results -----------------------------------------------------------------
library(writexl, include.only = 'write_xlsx')
df_hausman %>% filter(loglog) %>% select(-p_value, -loglog) %>%
  mutate(models = toupper(models),
         outcome = recode(outcome, 
                          farsocc = "Log(TFVO)", farsnocc = "Log(TFVN)")) %>% 
  write_xlsx(sprintf('%s/results/tables/spatial_hausman.xlsx', root_folder))

df_lm_test %>% filter(loglog, model == 'within', effect == 'twoways') %>% 
  select(-loglog, -model, -effect, -H1, -significance) %>% 
  mutate(test = toupper(test),
         outcome = recode(outcome, 
                          farsocc = "Log(TFVO)", farsnocc = "Log(TFVN)")) #%>% 
  

get_my_table <- function(model, n_digits = 2L, model_name = NULL){
  get_covariates <- function(model) {
    names(model$coefficients)
  }
  get_point_estimates <- function(model) {
    model$coefficients %>% unname
  }
  get_se <- function(model) {
    sqrt(diag(model$vcov) / length(length(model$residuals))) %>% unname
  }
  get_n_covariates <- function(model) {
    model$coefficients %>% length
  }
  
  get_stars <- function(model) {
    model <- summary(model)
    if ('CoefTable' %in% names(model)) {
      model <- model$CoefTable
    } else {
      model <- model$coefficients
    }
    p_values <- model[, 'Pr(>|t|)'] %>% unname
    stars <- sapply(p_values, function(p) {
      if (p < 0.001) {
        "***"
      } else if (p < 0.01) {
        "**"
      } else if (p < 0.05) {
        "*"
      } else if (p < 0.1) {
        "."
      } else {
        ""
      }
    })
    return(stars)
  }
  if (is.null(model_name)) {
    model_name <- 'estimate'
  }
  bind_rows(
    data.frame(id = 1:get_n_covariates(model),
               covariate = get_covariates(model),
               type = 'point', 
               estimate = get_point_estimates(model),
               stars = get_stars(model)),
    data.frame(id = 1:get_n_covariates(model),
               covariate = get_covariates(model),
               type = 'se',
               estimate = get_se(model),
               stars = "")
  ) %>% arrange(id, covariate) %>% 
    mutate(#covariate = if_else(type == 'se', "", covariate),
           estimate = paste0(round(estimate, n_digits), stars)) %>% 
    select(covariate, type, estimate) %>%
    rename(!!sym(model_name) := estimate)
}

covariates_labels <- c(
  "Log(seat belt usage)", "Log(median income)", 
  "Log(unemployment rate)", "Log(mean age)", "Log(% blacks)", 
  "Log(% Hispanics)", "Log(traffic density urban)", 
  "Log(traffic density rural)", "Log(violent crimes)", "Log(property crimes)", 
  "Log(VMT rural)", "Log(VMT urban)", "Log(fuel tax)", "65-mph speed limit", 
  "70-mph speed limit or above", "BAC 0.08")

ls_models <- list(
  log_farsnocc = c('fe', 'semfe', 'sdmfe'),
  log_farsocc = c('fe', 'slxfe'))
n_digits <- 4L
for (outcome in names(ls_models)) {
  df <- NULL
  for (model_name in ls_models[[outcome]]) {
    my_model <- results[[outcome]][[model_name]]
    df0 <- get_my_table(my_model, n_digits, toupper(model_name))
    if (is.null(df)) {
      df <- df0
    } else {
      df <- left_join(df, df0, by = c('covariate', 'type'))
    }
  }
  df %>%  mutate(covariate = rep(covariates_labels, each = 2)) %>% 
    mutate(covariate = if_else(type == 'se', "", covariate)) %>% 
    select(-type) %>%
    write_xlsx(sprintf('%s/results/tables/spatial_%s.xlsx', root_folder, outcome))
}