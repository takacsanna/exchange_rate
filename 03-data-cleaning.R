raw_news_df <- pin_read(.board, "raw_news_df")

set.seed(1)

sm1 <- raw_news_df |>
  group_by(medium) |>
  sample_n(100)

trigram_tf_idf <- sm1 |>
  unnest_ngrams("ngram", text, n = 3) |>
  count(medium, ngram) |>
  bind_tf_idf(ngram, medium, n)

trigram_tf_idf |>
  slice_max(n, n = 5) |>
  left_join(sm1, multiple = "all") |>
  filter(str_detect(text, ngram)) |>
  distinct(medium, ngram, .keep_all = TRUE) |>
  mutate(text = str_replace_all(text, ngram, str_c("**", ngram, "**"))) |>
  group_by(medium) |>
  select(medium, n, text) |>
  gt() |>
  fmt_markdown(text, rows = everything())

