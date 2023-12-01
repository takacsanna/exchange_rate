text_steps <- function(recipe_steps, ...,  max_tokens = 1e3, exclude_stopwords = TRUE) {
  recipe_steps <- recipe_steps |>
    step_tokenize(..., options = list(
      strip_punct = TRUE,
      lowercase = TRUE,
      strip_numeric = TRUE
    )) |>
    step_stem(..., options = list(language = "hungarian"))

  if (exclude_stopwords) {
    recipe_steps <- recipe_steps |>
      step_stopwords(..., custom_stopword_source = stopwords(language = "hu"))
  }

  recipe_steps |>
    textrecipes::step_tokenfilter(...) |>
    step_tokenfilter(..., max_tokens = max_tokens) |>
    step_tfidf(...)
}

general_steps <- function(recipe_steps) {
  recipe_steps |>
    step_zv(all_predictors()) |>
    step_naomit(all_predictors()) |>
    step_normalize(all_predictors())
}
