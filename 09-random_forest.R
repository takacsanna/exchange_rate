exchage_text_df <- pin_read(.board, "exchage_text_df")
eurhuf_df <- pin_read(.board, "eurhuf_df")

leaded_close_df <- eurhuf_df |>
  transmute(
    time = lag(time, 2), close # for easier calculation
  )

set.seed(123)

linear_reg_glm_spec <- linear_reg() %>%
  set_engine('lm')

rand_forest_lm_spec <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression") %>%
  translate()

design_df <- exchage_text_df |>
  mutate(e_rate = lead(e_rate, 2)) |>
  na.omit()

folds <- design_df |>
  rolling_origin(
    initial = 3000,
    assess = 1000,
    cumulative = FALSE
  )

rec <- recipe(e_rate ~ ., data = design_df) |>
  step_rm(time) |>
  step_zv() |>
  step_normalize(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors(), .8)

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(rand_forest_lm_spec)

mda_vec <- function(truth, estimate, na_rm = TRUE, lag = 1) {

  MDirAcc <- function(truth, estimate, lag=1) {
    return( mean(sign(diff(truth, lag=lag))==sign(diff(estimate, lag=lag))) )
  }


  metric_vec_template(
    metric_impl = MDirAcc,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    lag = 1,
    cls = "numeric"
  )

}

fit_resamples(
  wf,
  resamples = folds,
  metrics = metric_set(mape, rmse, rsq, mda_vec)
)
