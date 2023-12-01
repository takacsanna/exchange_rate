library(rlang)

mda_impl <- function(truth, estimate, case_weights = NULL) {
  mean((truth > 0) == (estimate > 0))
}

mda_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  mda_impl(truth, estimate, case_weights = case_weights)
}


mda <- function(data, ...) {
  UseMethod("mda")
}

mda <- new_numeric_metric(mda, direction = "minimize")

mda.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {

  numeric_metric_summarizer(
    name = "mda",
    fn = mda_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}

lss_impl <- function(truth, estimate, case_weights = NULL) {
  sum(abs(truth[(truth > 0) == (estimate > 0)])) - sum(abs(truth[(truth > 0) != (estimate > 0)]))
}

lss_vec <- function(truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {
  check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
    case_weights <- result$case_weights
  } else if (yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  lss_impl(truth, estimate, case_weights = case_weights)
}


lss <- function(data, ...) {
  UseMethod("lss")
}

lss <- new_numeric_metric(lss, direction = "minimize")

lss.data.frame <- function(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...) {

  numeric_metric_summarizer(
    name = "lss",
    fn = lss_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    case_weights = !!enquo(case_weights)
  )
}


best_tuning_params <- function(race_results) {

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

  best_rsq <- race_results |>
    transmute(
      wflow_id,
      map(result, show_best, metric = "rsq", n = 1) |>
        list_rbind()
    )

  best_mda <- race_results |>
    transmute(
      wflow_id,
      map(result, show_best, metric = "mda", n = 1) |>
        list_rbind()
    )

  bind_rows(
    best_lss, best_rmse, best_rsq, best_mda
  )
}


