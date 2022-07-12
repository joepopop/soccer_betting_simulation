library(tidymodels)
library(tidyverse)
library(doMC)

# register cores/threads for parallel processing
registerDoMC(cores = detectCores(logical = T))

tidymodels_prefer()
set.seed(3012)

# usemodels::use_xgboost(result ~ . , data = football_train, verbose = T, clipboard = T)

# load required objects ----
load("data/initial_setup.rda")
load("data/initial_split.rda")

xgboost_recipe <-
  recipe(formula = result ~ b365h + b365d + b365a + bwh + bwd + bwa + iwh + iwd + iwa + psh + psd + psa + whh + whd + wha + vch + vcd + vca, data = football_train) %>%
  step_normalize(all_numeric_predictors()) 

# check recipe
xgboost_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>% 
  view()

# model specification ----
xgboost_spec <- 
  boost_tree(mtry = tune(), learn_rate = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost") 


# workflow ----
xgboost_wflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec) 

# tuning parameter grid ----
xgboost_params <- hardhat::extract_parameter_set_dials(xgboost_wflow) %>% 
  update(
    learn_rate = min_n(range = c(-5, 0)),
    mtry = mtry(range = c(2, 10))
  )
grid_info <- grid_regular(xgboost_params, levels = 3)
grid_info %>% view()
# fit to resamples ----
xgboost_res_1 <- 
  xgboost_wflow %>% 
  tune_grid(
    resamples = football_fold,
    control = stacks::control_stack_grid(),
    grid = grid_info,
    metrics = football_metric
  )

# save results ----
save(xgboost_res_1, xgboost_wflow, file = "results/xgboost_res_1.rda")
