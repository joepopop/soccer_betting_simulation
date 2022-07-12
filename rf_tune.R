library(tidymodels)
library(tidyverse)
library(doMC)

# register cores/threads for parallel processing
registerDoMC(cores = detectCores(logical = T))

tidymodels_prefer()
set.seed(3012)

# usemodels::use_rf(result ~ . , data = football_train, verbose = T, clipboard = T)

# load required objects ----
load("data/initial_setup.rda")
load("data/initial_split.rda")

rf_recipe <-
  recipe(formula = result ~ b365h + b365d + b365a + bwh + bwd + bwa + iwh + iwd + iwa + psh + psd + psa + whh + whd + wha + vch + vcd + vca, data = football_train) %>%
  step_normalize(all_numeric_predictors()) 

# check recipe
rf_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>% 
  view()

# model specification ----
rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("ranger") 


# workflow ----
rf_wflow <- 
  workflow() %>% 
  add_recipe(rf_recipe) %>% 
  add_model(rf_spec) 

# tuning parameter grid ----
rf_params <- hardhat::extract_parameter_set_dials(rf_wflow) %>% 
  update(
    min_n = min_n(range = c(5, 20)),
    mtry = mtry(range = c(2, 10))
  )
grid_info <- grid_regular(rf_params, levels = 3)
grid_info %>% view()
# fit to resamples ----
rf_res_1 <- 
  rf_wflow %>% 
  tune_grid(
    resamples = football_fold,
    control = stacks::control_stack_grid(),
    grid = grid_info,
    metrics = football_metric
  )

# save results ----
save(rf_res_1, rf_wflow, file = "results/rf_res_1.rda")
