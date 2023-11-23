library(tidymodels)
library(finetune)

# data setup --------------------------------------------------------------

design_df <- pin_read(.board, "design_df") %>%
  group_by(g = (as.numeric(time) - 1) %/% (60 * 5)) %>%
  summarise(
    time = last(time),
    price = last(price),
    across(- (time:price), sum)
  ) %>%
  select(- g) %>%
  mutate(
    ld_price = log(price) - log(lag(price)), # logdiff
    ld_price_l1 = lag(ld_price, 1),
    ld_price_l2 = lag(ld_price, 2),
    ld_price_l3 = lag(ld_price, 3),
    ld_price_l4 = lag(ld_price, 4),
    ld_price_l5 = lag(ld_price, 5),
  ) %>%
  drop_na()

folds <- design_df |>
  head(2500) %>%
  rolling_origin(
    initial = 2100,
    assess = 300,
    cumulative = FALSE
  )

# spec --------------------------------------------------------------------

svm_linear_LiblineaR_spec <-
  svm_linear(cost = tune(), margin = tune()) %>%
  set_engine('LiblineaR') %>%
  set_mode('regression')

linear_reg_glmnet_spec <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine('glmnet')

mars_earth_spec <-
  mars(prod_degree = tune()) %>%
  set_engine('earth') %>%
  set_mode('regression')

xgb_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    mtry = tune(),
    learn_rate = 0.01
  ) |>
  set_engine("xgboost") |>
  set_mode("regression")

# wf ---------------------------------------------------------------------

rec <- recipe(ld_price ~ ., data = design_df) |>
  step_rm(time, price) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = .8)

rec_lags <- recipe(ld_price ~ ld_price_l1 + ld_price_l2 + ld_price_l3 + ld_price_l4 + ld_price_l5, data = design_df) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = .8)


all_workflows <- workflow_set(
  preproc = list(
    text = rec,
    only_lags = rec_lags
  ),
  models = list(
    lasso = linear_reg_glmnet_spec,
    mars = mars_earth_spec,
    svm = svm_linear_LiblineaR_spec,
    xgb = xgb_spec
  ),
  cross = TRUE
)

# metrics

source("metrics.R")

# anova -------------------------------------------------------------------

doParallel::registerDoParallel(cores = 9)

race_results <- all_workflows %>%
  workflow_map(
    "tune_race_anova",
    seed = 1503,
    resamples = folds,
    grid = 25,
    control = control_race(verbose_elim = TRUE, randomize = TRUE, allow_par = TRUE, burn_in = 10),
    metrics = metric_set(lss, rmse, rsq, mda)
  )

best_lss <- race_results |>
  transmute(
    wflow_id,
    map(result, show_best, metric = "lss", n = 1) |>
      list_rbind()
  )


best_rmse <- race_results |>
  transmute(
    wflow_id,
    map(result, show_best, metric = "rmse", n = 1) |>
      list_rbind()
  )

best_mda <- race_results |>
  transmute(
    wflow_id,
    map(result, show_best, metric = "mda", n = 1) |>
      list_rbind()
  )

bind_rows(
  best_lss, best_rmse, best_mda, best_mda
) |>
  pin_write(
    board = .board,
    name = "5min_tune"
  )

