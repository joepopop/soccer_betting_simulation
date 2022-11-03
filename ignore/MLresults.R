# load packages, deal with conflicts, and set seed ----
library(tidymodels)
library(probably)
library(tidyverse)
library(patchwork)

tidymodels_prefer()
conflicted::conflict_prefer("lag", "dplyr")
set.seed(3012)

# load required objects ----
load("data/processed/initial_setup.rda")
load("data/processed/initial_split.rda")
models_list <- c("glmnet", "knn", "rf", "xgboost")
training_precision <- tibble(model = character(),
                             mean = double()
)
testing_predictions <- tibble(id = 1:1654)
profit <- tibble(id = 1:1654)

for (i in 1:length(models_list)){
  
  # load tuned results and workflows
  load(str_c("results/models/", models_list[i], "_res_1.rda"))

  # compile table with model and respective precision on training set
  training_precision <- training_precision %>%
    bind_rows(get(str_c(models_list[i], "_res_1")) %>%
              show_best(n = 1) %>%
              mutate(model = models_list[i]) %>%
              select(model, mean)
    )

  # get predictions on testing set
  testing_predictions <- testing_predictions %>%
    cbind(get(str_c(models_list[i], "_wflow")) %>%
          finalize_workflow(select_best(get(str_c(models_list[i], "_res_1")))) %>%
          fit(football_train) %>%
          predict(new_data = football_test, type = "class") %>%
          rename(!! str_c(models_list[i], ".pred_class") := .pred_class)
    )

}

training_precision %>%
  bind_rows(football_train %>%
              group_by(result) %>%
              count() %>%
              ungroup() %>%
              mutate(percentage = n / sum(n)) %>%
              arrange(desc(percentage)) %>%
              slice(1:1) %>%
              mutate(model = "null") %>%
              rename(mean = percentage) %>%
              select(model, mean)) %>%
  arrange(desc(mean))

temp <- testing_predictions %>%
  bind_cols(football_test) %>%
  mutate(
    # max_h = pmax(b365h, bwh, iwh, psh, whh, vch, na.rm = T),
    # max_d = pmax(b365d, bwd, iwd, psd, whd, vcd, na.rm = T),
    # max_a = pmax(b365a, bwa, iwa, psa, wha, vca, na.rm = T),
    id = row_number()
  ) %>%
  drop_na(max_h, max_a, max_d) %>% 
  select(id, result, contains(".pred_class"), max_h, max_a, max_d)

profit <- testing_predictions %>%
  bind_cols(football_test) %>%
  mutate(id = row_number()) %>% 
  select(id, date, result)

for (i in 1:length(models_list)){
# calculate mean and cumulative profit

profit <- profit %>%
  full_join(
    temp %>%
      filter(get(str_c(models_list[i], ".pred_class")) != "H") %>%
      mutate(
        !! str_c(models_list[i], "_profit")  := case_when(
          result == get(str_c(models_list[i], ".pred_class")) & result == "H" ~ (max_h) - 1,
          result == get(str_c(models_list[i], ".pred_class")) & result == "D" ~ (max_d) - 1,
          result == get(str_c(models_list[i], ".pred_class")) & result == "A" ~ (max_a) - 1,
          TRUE ~ -1),
        !! str_c(models_list[i], "_cum_profit")  := cumsum(get(str_c(models_list[i], "_profit")))
      ) %>%
      select(id, str_c(models_list[i], ".pred_class"), str_c(models_list[i], "_profit"), str_c(models_list[i], "_cum_profit"))
  )
}

# for H
# knn: -1.94 after 579
profit %>% 
  select(knn_cum_profit) %>% 
  drop_na() %>% 
  pull() %>% 
  length()
# rf: -26.84 after 574
profit %>% 
  select(rf_cum_profit) %>% 
  drop_na() %>% 
  pull() %>% 
  length()
# glmnet: -20.54 after 714
profit %>% 
  select(glmnet_cum_profit) %>% 
  drop_na() 
# xgboost: NA
profit %>% 
  select(xgboost_cum_profit) %>% 
  drop_na() 

# for A
# knn: 21.28 after 324
profit %>% 
  select(knn_cum_profit) %>% 
  drop_na() 
# rf: 13.37 after 318
profit %>% 
  select(rf_cum_profit) %>% 
  drop_na() 
# glmnet: 24.3 after 296
profit %>% 
  select(glmnet_cum_profit) %>% 
  drop_na() %>% view()
# xgboost: -19.8 after 1010
profit %>% 
  select(xgboost_cum_profit) %>% 
  drop_na() %>% view()
# for D
# $4.47 after 107
profit %>% 
  select(knn_cum_profit) %>% 
  drop_na() %>% view()

# 15.66 after 118
profit %>% 
  select(rf_cum_profit) %>% 
  drop_na() %>% view()




ALL_pred <- profit %>% 
  pivot_longer(ends_with("cum_profit"),names_to = "cum_profit_names", values_to = "cum_profit_values") %>% 
  pivot_longer(ends_with("profit"),names_to = "profit_names", values_to = "profit_values") %>% 
  mutate(cum_profit_names =
           case_when(
             cum_profit_names == "glmnet_cum_profit" ~ "Elastic Net",
             cum_profit_names == "knn_cum_profit" ~ "Nearest Neighbors",
             cum_profit_names == "rf_cum_profit" ~ "Random Forest",
             cum_profit_names == "xgboost_cum_profit" ~ "Boosted Trees" 
           )) %>% 
  filter(date > ymd("2019-7-1")) %>% 
  ggplot(aes(date, cum_profit_values, color = cum_profit_names)) +
  geom_point(size = 0.5, alpha = 0.15) +
  labs(
    title = "Results from betting on all games",
    y = "Cumulative profit ($)",
    x = "Date",
    color = "Models"
  ) +
  scale_color_manual(values = c("red", "blue", "purple", "green")) +
  scale_y_continuous(breaks=seq(-60,60,15)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Optima")
  )

ALL_pred + H_pred + A_pred + D_pred + plot_layout(guides = "collect") + plot_annotation(
  title = "Cumulative profits from $1 bets",
  theme = theme(text = element_text(family = "Optima", size = 16)))

profit %>% 
  pivot_longer(ends_with("cum_profit"),names_to = "cum_profit_names", values_to = "cum_profit_values") %>% 
  pivot_longer(ends_with("profit"),names_to = "profit_names", values_to = "profit_values") %>% 
  ggplot(aes(date, profit_values, color = profit_names)) +
  geom_point(size = 0.5)


glmnet_result %>% 
  bind_cols(football_test %>% select(result)) %>% 
  football_metric(truth = as.factor(result), estimate = .pred_class)
glmnet_result %>% 
  bind_cols(football_test %>% select(result)) %>% 
  conf_mat(truth = result, estimate = .pred_class, dnn = c("Prediction", "Truth"))


knn_precision <- bind_cols(knn_result, football_test %>% select(result))
football_metric(knn_precision, truth = as.factor(result), estimate = .pred_class)
precision(knn_precision, truth = as.factor(result), estimate = .pred_class)
knn_precision %>% 
  conf_mat(truth = result, estimate = .pred_class, dnn = c("Prediction", "Truth"))

rf_precision <- bind_cols(rf_result, football_test %>% select(result))
football_metric(rf_precision, truth = as.factor(result), estimate = .pred_class)
precision_macro(rf_precision, truth = as.factor(result), estimate = .pred_class)
precision(rf_precision, truth = as.factor(result), estimate = .pred_class)
rf_precision %>% 
  conf_mat(truth = result, estimate = .pred_class, dnn = c("Prediction", "Truth"))

xgboost_precision <- bind_cols(xgboost_result, football_test %>% select(result))
football_metric(xgboost_precision, truth = as.factor(result), estimate = .pred_class)
precision_macro(xgboost_precision, truth = as.factor(result), estimate = .pred_class)
precision(xgboost_precision, truth = as.factor(result), estimate = .pred_class)
xgboost_precision %>% 
  conf_mat(truth = result, estimate = .pred_class, dnn = c("Prediction", "Truth"))

precision_macro <- function(data, truth, estimate, estimator, na_rm = TRUE, event_level = yardstick_event_level(), ...) {
  precision(
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    estimator = "macro_weighted",
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}

precision_macro <- new_class_metric(precision_macro, "maximize")

precision_macro <- metric_set(precision_macro)
