library(tidyverse)
library(tidymodels)
library(lubridate)

tidymodels_prefer()
set.seed(3012)

full_data <- read_rds("data/full_data.rds")

# split & fold ----

football_split <- full_data %>% 
  initial_split(prop = 0.8, strata = result)

football_train <- football_split %>% training()
football_test <- football_split %>% testing()

football_fold <- 
  football_train %>% 
  vfold_cv(v = 5, repeats = 5, strata = result)

# other ----
keep_pred <- control_grid(save_pred = T, save_workflow = T)

precision_micro <- function(data, truth, estimate, estimator, na_rm = TRUE, event_level = yardstick_event_level(), ...) {
  precision(
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    estimator = "micro",
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}

precision_micro <- new_class_metric(precision_micro, "maximize")

football_metric <- metric_set(precision_micro)

# save initial setup ----
save(football_fold, keep_pred, football_metric, file = "data/initial_setup.rda")
save(football_test, football_train, file = "data/initial_split.rda")
