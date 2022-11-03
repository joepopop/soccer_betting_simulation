# load packages, deal with conflicts, set seed, and prepare for parallel processing ----
library(tidymodels)
library(tidyverse)
library(doMC)

# register cores/threads for parallel processing
registerDoMC(cores = detectCores(logical = T))

tidymodels_prefer()
set.seed(3012)

# usemodels::use_glmnet(result ~ . , data = football_train, verbose = T, clipboard = T)

# load required objects ----
load("data/processed/initial_setup.rda")
load("data/processed/initial_split.rda")


# recipe ---- 
# glmnet_recipe <-
#   recipe(formula = result ~ ., data = football_train) %>%
#   step_string2factor(all_nominal_predictors()) %>%
#   step_impute_knn(attendance, impute_with = imp_vars(day, venue, season)) %>%
#   step_rm(wk, day, date, time, home, away, venue, referee, match_report, season, starts_with("mean")) %>%
#   # step_novel(all_nominal_predictors()) %>%
#   # step_dummy(all_nominal_predictors()) %>%
#   # step_zv(all_predictors()) %>%
#   step_normalize(all_numeric_predictors()) 
#   # step_interact(~attendance:mean_score_away:b365a)
  
# tried using season, attendance, mean_stuff, odds (combinations) but variable-wise odds were the best and makes sense
glmnet_recipe <-
  recipe(formula = result ~ b365h + b365d + b365a + bwh + bwd + bwa + iwh + iwd + iwa + psh + psd + psa + whh + whd + wha + vch + vcd + vca, data = football_train) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 3)

# check recipe
glmnet_recipe %>%
  prep() %>%
  bake(new_data = NULL) %>% 
  view()

# model specification ----
glmnet_spec <- 
  multinom_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet") 


# workflow ----
glmnet_wflow <- 
  workflow() %>% 
  add_recipe(glmnet_recipe) %>% 
  add_model(glmnet_spec) 

# tuning parameter grid ----
glmnet_params <- hardhat::extract_parameter_set_dials(glmnet_wflow) %>% 
  update(
    penalty = penalty(range = c(-2, 0)),
    mixture = mixture(range = c(0.25, 0.75))
  )
grid_info <- grid_regular(glmnet_params, levels = 3)
grid_info %>% view()
# fit to resamples ----
glmnet_res_1 <- 
  glmnet_wflow %>% 
  tune_grid(
    resamples = football_fold,
    control = stacks::control_stack_grid(),
    grid = grid_info,
    metrics = football_metric
  )

# save results ----
save(glmnet_res_1, glmnet_wflow, file = "results/models/glmnet_res_1.rda")
