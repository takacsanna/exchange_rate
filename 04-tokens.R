clean_news_df <- pin_read(.board, "clean_news_df")
eurhuf_df <- pin_read(.board, "eurhuf_df")

info("Find most frequent words")

most_freq_words <- clean_news_df |>
  group_by(medium) |>
  slice_sample(n = 1000) |>
  pull(text) |>
  tokens() |>
  tokens_tolower() |>
  tokens_remove(pattern = stopwords("hungarian")) |>
  tokens_wordstem(language = "hungarian") |>
  as.list() |>
  map(keep, .p = \(x) str_length(x) > 2 | x == "eu") |>
  reduce(c) |>
  table() |>
  sort() |>
  rev() |>
  head(100) |>
  names()

info("Unnest tokens")

news_tokens <- clean_news_df |>
  pull(text) |>
  tokens() |>
  tokens_tolower() |>
  tokens_remove(pattern = stopwords("hungarian")) |>
  tokens_wordstem(language = "hungarian") |>
  as.list() |>
  map(keep, .p = \(x) x %in% most_freq_words)

info("Feature assignment")

text_features <- clean_news_df |>
  mutate(
    time = floor_date(ymd_hms(time), unit = "hour"),
    text = news_tokens
  ) |>
  unnest(text) |>
  count(time, medium, text) |>
  unite("term", medium:text) |>
  pivot_wider(names_from = term, values_from = n, values_fill = 0) |>
  select_if(~ n_distinct(.) > 1)

info("Export")

exchage_text_df <- eurhuf_df |>
  transmute(time, e_rate = c(NA, diff(log(close)))) |>
  left_join(text_features)

pin_write(.board, most_freq_words, "most_freq_words") # useful for wordcloud
pin_write(.board, exchage_text_df, "exchage_text_df")
