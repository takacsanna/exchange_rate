exchage_text_df <- pin_read(.board, "exchage_text_df")

set.seed(123)

linear_reg_glm_spec <- linear_reg() %>%
  set_engine('lm')

lm_horizon <- function(horizon) {
  exchage_split <- exchage_text_df |>
    mutate(e_rate = case_when(
      horizon > 0 ~ lead(e_rate, abs(horizon)),
      horizon < 0 ~ lag(e_rate, abs(horizon)),
      horizon == 0 ~ e_rate
    )) |>
    na.omit() |>
    select(- time) |>
    initial_split(strata = e_rate)

  exchage_train <- training(exchage_split)
  folds <- vfold_cv(exchage_train, strata = e_rate)

  rec <- recipe(e_rate ~ ., data = exchage_train) |>
    step_normalize(all_numeric_predictors())

  wf <- workflow(rec, linear_reg_glm_spec)

  rs <- fit_resamples(wf, resamples = folds)
  collect_metrics(rs) |>
    mutate(horizon, .before = 1)
}

lm_horizon_df <- cp_map_dfr(-3:3, lm_horizon, name = "lm_horizon")

pin_write(.board, lm_horizon_df, "lm_horizon_df")
