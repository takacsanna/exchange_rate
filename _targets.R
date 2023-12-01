# Load packages required to define the pipeline:
suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(tidyverse)
  library(tidymodels)
  library(finetune)
  library(pins)
  library(textrecipes)
  library(stopwords)
  library(LiblineaR)
  library(earth)
  library(glmnet)
  library(xgboost)
})

tar_source()

# Replace the target list below with your own:
list(
  tar_target(name = news, command = {
    pin_read(
      board = board_folder(tar_config_get("store")),
      name = "clean_news_df",
    ) |>
      select(time, medium, text) |>
      mutate(time = lubridate::ymd_hms(time)) |>
      filter(time >= "2020-10-01")
  }
  ),
  tar_target(
    name = bux,
    command = {
      read_csv("BUX_MLD.csv", show_col_types = FALSE) |>
        filter(`Date-Time` >= "2020-10-01") |>
        transmute(
          time = `Date-Time`,
          price = Last
        ) |>
        drop_na()
    }
  ),
  tar_target(recipe_steps, command = {
    list(
      text = function(training_data) {
        recipe(ld_price ~ text, data = training_data) |>
          text_steps(text) |>
          general_steps()
      },
      lag_text = function(training_data) {
        recipe(ld_price ~ text, data = training_data) |>
          step_lag(text, lag = 1, keep_original_cols = FALSE) |>
          text_steps(lag_1_text) |>
          general_steps()
      }
    )
  }),
  tar_target(models, command = {
    linear_reg_glmnet_spec <-
      linear_reg(penalty = tune(), mixture = tune()) %>%
      set_engine('glmnet')

    svm_linear_LiblineaR_spec <-
      svm_linear(cost = tune(), margin = tune()) %>%
      set_engine('LiblineaR') %>%
      set_mode('regression')

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

    list(
      lasso = linear_reg_glmnet_spec,
      mars = mars_earth_spec,
      svm = svm_linear_LiblineaR_spec,
      xgb = xgb_spec
    )
  }),
  tar_map(
    list(minutes = c(1, 5, 15, 30)),
    tar_target(name = design, command = {
      bux_agg <- bux |>
        group_by(g = (as.numeric(time) - 1) %/% (60 * minutes)) %>%
        summarise(
          time = last(time),
          price = last(price)
        )  |>
        select(- g) |>
        mutate(
          ld_price = log(price) - log(lag(price))
        ) |>
        filter(as.numeric(time - lag(time)) == minutes) # exclude openings

      news_agg <- news |>
        group_by(time = (as.numeric(time) - 1) %/% (60 * minutes)) %>%
        summarise(
          text = str_flatten(text, " "),
          n_article = n()
        ) |>
        mutate(
          time = ((time + 1) * 60 * minutes) +
            lubridate::ymd_hms("1970-01-01 00:00:00"),
        )

      inner_join(bux_agg, news_agg, by = join_by(time)) |>
        arrange(time) |>
        drop_na()
    }),
    tar_target(bux_training, command = filter(design, time < "2022-02-01")),
    tar_target(bux_testing, command = filter(design, time >= "2022-02-01")),
    tar_target(workflows, command = {

      bux_recipes <- map(recipe_steps, ~ .x(bux_training_1))
      folds <- sliding_period(
        bux_training,
        lookback = 60 * 24 * 7,
        assess_start = 1,
        assess_stop = 60 * 24 * 3,
        period = "minute",
        step = 8 * 60 / minutes / 4,
        index = time
      )

      doParallel::registerDoParallel(cores = parallel::detectCores() - 2)

      workflow_race_tune <- workflow_set(
        preproc = bux_recipes,
        models = models,
        cross = TRUE
      ) |>
        workflow_map(
          "tune_race_anova",
          seed = 1503,
          resamples = folds,
          grid = 30,
          control = control_race(
            verbose = TRUE,
            verbose_elim = TRUE,
            randomize = TRUE,
            allow_par = TRUE,
            burn_in = 3
          ),
          metrics = metric_set(lss, rmse, rsq, mda)
        )

      list(
        metrics = pull(workflow_race_tune, result, wflow_id) |>
          map(select, id, .metrics),
        select_best = pull(workflow_race_tune, result, wflow_id) |>
          map(select_best, "lss"),
        show_best = best_tuning_params(workflow_race_tune)
      )
    })
  )
)

