
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

run_panel_regressions <- function(data, outcome, controls, We) {
  
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
  run_model("fe", plm, list(formula = formula, data = data, model = "within", effect = "twoway"))
  run_model("re", plm, list(formula = formula, data = data, model = "random", effect = "individual"))
  
  # Regresiones de panel con efectos espaciales (efectos fijos)
  # Añadimos un try-catch adicional para manejar errores de singularidad
  tryCatch({
    run_model( "sarfe", spml, list(formula = formula, data = data, listw = We, spatial.error = "none", 
                                   lag = T, model = "within", effect = "twoway", method = "eigen"))
  }, error = function(e) {
    print(paste("Error in sarfe:", e$message))
  })
  
  tryCatch({
    run_model("semfe", spml, list(formula = formula, data = data, listw = We, spatial.error = "b", 
                                  lag = F, model = "within", effect = "twoway", method = "eigen"))
  }, error = function(e) {
    print(paste("Error in semfe:", e$message))
  })
  
  tryCatch({
    run_model("sararfe", spml, list(formula = formula, data = data, listw = We, lag = TRUE, 
                                    spatial.error = "b", model = "within", effect = "twoway", method = "eigen"))
  }, error = function(e) {
    print(paste("Error in sararfe:", e$message))
  })
  
  run_model("sdmfe", spml, list(formula = sdm_formula, data = data, listw = We, lag = TRUE,
                                spatial.error = "none", model = "within", effect = "twoway", method = "eigen"))
  run_model("sdemfe", spml, list(formula = sdm_formula, data = data, listw = We, lag = FALSE,
                                 spatial.error = "b", model = "within", effect = "twoway", method = "eigen"))
  run_model("slxfe", plm, list(formula = sdm_formula, data = data, model = "within", effect = "twoway"))
  
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
logaritmic <- FALSE
outcomes <- c('farsnocc', 'farsocc')
log_controls <- c('percapin', 'unemp', 'densurb', 'densrur', 'proppcap', 'vmturban', 'fueltax')
dummy_controls <- c()
log_controls <- c('percapin', 'unemp', 'meanage', 'precentb', 'precenth', 
                  'densurb', 'densrur', 'viopcap', 'proppcap', 'vmtrural',
                  'vmturban', 'fueltax')
dummy_controls <- c('lim65', 'lim70p', 'mlda21', 'bac08')[-c(3:4)]
controls <- c(log_controls, dummy_controls)
instruments <- c('ds', 'dp', 'dsp')

# Process data.
datapanel <- SeatBelt %>% 
  mutate(code = as.integer(factor(state)),
         vmt = vmtrural + vmturban,
         farsocc = farsocc / vmt,
         farsnocc = farsnocc / vmt) %>% 
  filter(year > 1989, !state %in% c('AK', 'HI')) %>%
  pdata.frame(c("state","year")) %>% 
  mutate(across(all_of(log_controls), log))

if (logaritmic) {
  datapanel <- datapanel %>% 
    mutate(across(all_of(outcomes), log))
}

summary(datapanel)


# Regression models -------------------------------------------------------

# First stage instrumental variable.
model_fs <- lm(get_formula('usage', controls, instruments),
               data = datapanel)
datapanel$usage <- model_fs$fitted.values

# Summary Statistics
summary(model_fs)
stargazer(datapanel, type = "text")
stargazer(datapanel[, c("farsocc","farsnocc","usage")], type = "text", 
          covariate.labels = c("Fatalities Occupants", "Fatalities Non Occupants", "Seat Belt Usage"))


# Create spatial weights matrix
nb <- poly2nb(us, queen = T) %>% print()
We <- nb2listw(nb, style = "W") %>% print()

for (outcome in outcomes) {
  assign(paste0('results_', outcome),
         run_panel_regressions(datapanel, outcome, controls, We))
}

print_summary(results_farsocc, "Log de Fatalidades Ocupantes por VMT")
print_summary(results_farsocc, "Log de Fatalidades No Ocupantes por VMT")



# Hypothesis tests --------------------------------------------------------

sig_levels <- c(1, 5, 10) / 100
models <- c('', 'sar', 'sem')
df_hausman <- NULL
for (outcome in outcomes) {
  results <- get(paste0('results_', outcome))
  hausman_test <- rbind(
    phtest(results$re, results$fe)[['p.value']] <= sig_levels,
    sphtest(x = results$sarre, x2 = results$sarfe)[['p.value']][1] <= sig_levels,
    sphtest(x = results$semre, x2 = results$semfe)[['p.value']][1] <= sig_levels
  ) 
  df_hausman <- rbind(df_hausman,
                      cbind(outcome, models, hausman_test) %>% as.data.frame)
}
names(df_hausman)[3:5] <- paste0(sig_levels * 100, '%')
df_hausman
# Los resultados del test de Hausman estándar y el test de Hausman robusto a la
# autocorrelación de los errores y lag lleva a rechazar la hipótesis nula de
# ausencia de correlación entre los efectos individuales y las variables explicatorias
# Para el resto del análisis empírico, el modelo de efectos fijos es escogido


# Tests LM
lm_tests <- c('lml', 'lme', 'rlml', 'rlme')
model <- "within"
effect <- 'twoway'
df <- NULL
for (outcome in outcomes) {
  for (lm_test in lm_tests) {
    ls <- slmtest(get_formula(outcome, controls), data = datapanel, listw = We,
                  test = lm_test, model = model, effect = effect)
    df0 <- data.frame(outcome = outcome, test = lm_test, H1 = ls$alternative, p_value = ls$p.value[[1]])
    df <- rbind(df, df0)
  }
}

df
pooltest(results$pooling, results$fe)
# Tests para efectos individuales y tiempo
plmtest(results$pooling, effect = "twoway", type = "bp")
# Comparando el pooling y el Within
pFtest(results$pooling, results$fe)

# Test de efectos inobservables
# Ho: Var_effijos=0 -> Pooling
# Ha: Var_effijos<>0 -> efectos aleatorios
pwtest(get_formula(outcome, controls), data = datapanel)

# Tests de Baltagi BH, Song SH, Koh W (2003). Testing Panel Data Regression Models with Spatial
# Error Correlation. Journal of Econometrics, 117, 123-150:
# LM1: Ho: Var_effijos=0: no efectos aleatorios asumiendo no correlaci?n espacial
# LM2: Ho: rho=0: no correlaci?n espacial asumiendo no efectos aleatorios
# SLM1: versi?n estandarizada de LM1
# SLM2: versi?n estandarizada de LM2
# LMH: Ho: rho=Var_effijos=0: no existen efectos espaciales ni efectos aleatorios
# CLMlambda: Ho: rho=0: no correlaci?n espacial asumiendo la posible existencia
# de efectos aleatorios (Var_effijos puede o no puede ser cero)
# CLMmu: Ho: Var_effijos=0: no efectos aleatorios asumiendo la posibilidad de correlaci?n espacial
# (rho puede o no puede ser cero)
formula <- get_formula(outcome, controls)
bsktest(x = formula, data = datapanel, listw = We, test = "LM1")

bsktest(x = formula, data = datapanel, listw = We, test = "LM1", standardize=TRUE)

bsktest(x = formula, data = datapanel, listw = We, test = "LM2")

bsktest(x = formula, data = datapanel, listw = We, test = "LM2", standardize=TRUE)

bsktest(x = formula, data = datapanel, listw = We, test = "LMH")

bsktest(x = formula, data = datapanel, listw = We, test = "CLMlambda")

bsktest(x = formula, data = datapanel, listw = We, test = "CLMmu")
#####
slmtest(get_formula(outcome, controls), data=datapanel,
        listw = We, test="lml", model = model, effect = effect) ### TENEMOS DEPENDENCIA ESPACIAL EN LA VARIABLE ENDOGENA

slmtest(get_formula(outcome, controls), data=datapanel,
        listw = We, test="lme", model="within", effect = 'twoway') ### NO TENEMOS DEPENDENCIA ESPACIAL EN EL ERROR

slmtest(get_formula(outcome, controls), data=datapanel,
        listw = We, test="rlml", model="within", effect = 'twoway') 
### TENEMOS DEPENDENCIA ESPACIAL EN LA VARIABLE ENDOGENA ROBUSTA A LA PRESENCIA DE DEPENDENCIA ESPACIAL EN EL ERROR
slmtest(get_formula(outcome, controls), data=datapanel,
        listw = We, test="rlme", model="within", effect = 'twoway')


# Los test LM en un modelo de efectos fijos favorecen una espeficación SAR
# Los test1 (SAR) y test2 (SEM) confirman el rechazo de la hipótesis que
# estos dos términos (tomados independientemente) son nulos. Sin embargo,
# se puede notar que el estadístico para el SAR es mayor que para el SEM.
# Para poder concluir de una forma más creíble, los test robustos son usados
# en la presencia de la especificación de la autocorrelación espacial.
# Se observa que RLMlag y RLMerr son altamente significantes por lo que no es
# posible discriminar la estructura de la autocorrelación espacial. Qué hacer?
# - Estimar el SARAR
# - Elegir aquel con es estadístico robusto más alto
# - Compara criterios de información
# - Seguir la propuesta de Elhorst (2010): Si el modelo MCO es rechazado en favor 
# del spatial lag model, spatial error model o en favor de ambos modelos, entonces
# el modelo Durbin espacial debería ser estimado

# Si H0: theta = 0 y H0: theta + rho*beta = 0 son rechazadas =) modelo Durbin espacial
