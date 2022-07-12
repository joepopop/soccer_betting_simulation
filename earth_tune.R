library(tidymodels)
library(tidyverse)
library(doMC)

# register cores/threads for parallel processing
registerDoMC(cores = detectCores(logical = T))

tidymodels_prefer()
set.seed(3012)

# usemodels::use_earth(result ~ . , data = football_train, verbose = T, clipboard = T)

# load required objects ----
load("data/initial_setup.rda")
load("data/initial_split.rda")

earth_recipe <-
  recipe(formula = result ~ b365h + b365d + b365a + bwh + bwd + bwa + iwh + iwd + iwa + psh + psd + psa + whh + whd + wha + vch + vcd + vca, data = football_train) %>%
  step_normalize(all_numeric_predictors()) 

# check recipe
earth_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>% 
  view()

# model specification ----
earth_spec <- 
  mars(num_terms = tune(), prod_degree = tune(), prune_method = "none") %>% 
  set_mode("classification") %>% 
  set_engine("earth") 


# workflow ----
earth_wflow <- 
  workflow() %>% 
  add_recipe(earth_recipe) %>% 
  add_model(earth_spec) 

# tuning parameter grid ----
earth_params <- hardhat::extract_parameter_set_dials(earth_wflow) %>% 
  update(num_terms = num_terms(range = c(2, 10)),
         prod_degree = prod_degree(range = c(1, 4))
  )
grid_info <- grid_regular(earth_params, levels = 3)
grid_info %>% view()
# fit to resamples ----
earth_res_1 <- 
  earth_wflow %>% 
  tune_grid(
    resamples = football_fold,
    control = stacks::control_stack_grid(),
    grid = grid_info,
    metrics = football_metric
  )

# save results ----
save(earth_res_1, file = "results/earth_res_1.rda")
