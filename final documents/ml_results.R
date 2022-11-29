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

# analysis ----

# create list of models for loop
models_list <- c("glmnet", "knn", "rf", "xgboost")

# create tibble to display precisions for models
training_precision <- tibble(model = character(),
                             mean = double()
)

# create empty tibbles
testing_predictions <- tibble(id = 1:1654)
profit <- tibble(id = 1:1654)

for (i in 1:length(models_list)){
  
  # load tuned results and workflows
  load(str_c("model tuning/results/", models_list[i], "_res_1.rda"))

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

# return table with precisions for model
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

# combine predictions with market max odds
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

# create dataframe with id, date, and result to loop over in cum_profit function
profit <- testing_predictions %>%
  bind_cols(football_test) %>%
  mutate(id = row_number()) %>% 
  select(id, date, result)


# function to generate plot: takes prediction as input and returns plot with only games with the prediction
cum_profit <- function(pred) {
  
if (pred %in% c("H", "D", "A")) {
  
  # loop over different models 
  for (i in 1:length(models_list)){
  
  # prepare data for plotting (for specified prediction)
  profit <- profit %>%
    full_join(
      temp %>%
        filter(get(str_c(models_list[i], ".pred_class")) == pred) %>%
        mutate(
          !! str_c(models_list[i], "_profit")  := case_when(
            result == get(str_c(models_list[i], ".pred_class")) & result == "H" ~ 10*(max_h-1),
            result == get(str_c(models_list[i], ".pred_class")) & result == "D" ~ 10*(max_d-1),
            result == get(str_c(models_list[i], ".pred_class")) & result == "A" ~ 10*(max_a-1),
            TRUE ~ -10),
          !! str_c(models_list[i], "_cum_profit")  := cumsum(get(str_c(models_list[i], "_profit")))
        ) %>%
        select(id, str_c(models_list[i], ".pred_class"), str_c(models_list[i], "_profit"), str_c(models_list[i], "_cum_profit"))
    )
  }
  
  # generate plot (for specified prediction)
  profit %>%
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
      title = paste0("Bet only when '", pred, "' is predicted"),
      y = "Cumulative profit",
      x = "Date",
      color = "Models"
    ) +
    scale_color_manual(values = c("red", "blue", "purple", "green")) +
    scale_y_continuous(breaks=seq(-600,600,150)) +
    theme_minimal() +
    theme(
      text = element_text(family = "Tw Cen MT")
    )
}
  else
  {
    
    # same as above, preparing data and generating plot, but for all predictions
    for (i in 1:length(models_list)){
      
      profit <- profit %>%
        full_join(
          temp %>%
            mutate(
              !! str_c(models_list[i], "_profit")  := case_when(
                result == get(str_c(models_list[i], ".pred_class")) & result == "H" ~ 10*(max_h-1),
                result == get(str_c(models_list[i], ".pred_class")) & result == "D" ~ 10*(max_d-1),
                result == get(str_c(models_list[i], ".pred_class")) & result == "A" ~ 10*(max_a-1),
                TRUE ~ -10),
              !! str_c(models_list[i], "_cum_profit")  := cumsum(get(str_c(models_list[i], "_profit")))
            ) %>%
            select(id, str_c(models_list[i], ".pred_class"), str_c(models_list[i], "_profit"), str_c(models_list[i], "_cum_profit"))
        )
    }
    
    profit %>% 
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
        title = "Bet on all games",
        y = "Cumulative profit ($)",
        x = "Date",
        color = "Models"
      ) +
      scale_color_manual(values = c("red", "blue", "purple", "green")) +
      scale_y_continuous(breaks=seq(-600,600,150)) +
      theme_minimal() +
      theme(
        text = element_text(family = "Tw Cen MT")
      )
  }
}

# output plot
cum_profit("ALL") + cum_profit("H") + cum_profit("A") + cum_profit("D") + 
  plot_layout(guides = "collect") + plot_annotation(
  title = "ML: cumulative profits from $10 bets",
  theme = theme(text = element_text(family = "Tw Cen MT", size = 16)))





# draft below (ignore) ----
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
