# load packages, deal with conflicts, set seed, and prepare for parallel processing ----
library(tidymodels)
library(tidyverse)
library(doMC)

tidymodels_prefer()
set.seed(3012)

# register cores/threads for parallel processing
registerDoMC(cores = detectCores(logical = T))

# usemodels::use_knn(result ~ . , data = football_train, verbose = T, clipboard = T)

# load required objects ----
load("data/processed/initial_setup.rda")
load("data/processed/initial_split.rda")

# recipe ---- 
knn_recipe <-
  recipe(formula = result ~ b365h + b365d + b365a + bwh + bwd + bwa + iwh + iwd + iwa + psh + psd + psa + whh + whd + wha + vch + vcd + vca, data = football_train) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 2)

# check recipe
knn_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>% 
  view()

# model specification ----
knn_spec <- 
  nearest_neighbor(neighbors = tune(),
                   dist_power = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kknn") 

# workflow ----
knn_wflow <- 
  workflow() %>% 
  add_recipe(knn_recipe) %>% 
  add_model(knn_spec) 

# tuning parameter grid ----
knn_params <- hardhat::extract_parameter_set_dials(knn_wflow) %>% 
  update(
    neighbors = neighbors(range = c(5, 40)),
    dist_power = dist_power(range = c(2, 5))
  )
grid_info <- grid_regular(knn_params, levels = 3)
grid_info %>% view()

# fit to resamples ----
knn_res_1 <- 
  knn_wflow %>% 
  tune_grid(
    resamples = football_fold,
    control = stacks::control_stack_grid(),
    grid = grid_info,
    metrics = football_metric
  )

# save results ----
save(knn_res_1, knn_wflow, file = "results/models/knn_res_1.rda")
