clean_news_df <- pin_read(.board, "clean_news_df")

clean_news_df %>%
  group_by(medium) %>%
  summarise(min(time))

most_freq_words <- clean_news_df |>
  filter(time >= "2022-01-01") %>%
  filter(time < "2022-02-01") %>%
  arrange(time) %>%
  group_by(medium) |>
  slice_head(n = 1000) |>
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
    time = ceiling_date(ymd_hms(time), unit = "minute"),
    text = news_tokens
  ) |>
  filter(time >= "2022-01-01") %>%
  unnest(text) |>
  count(time, medium, text) |>
  unite("term", medium:text) |>
  pivot_wider(names_from = term, values_from = n, values_fill = 0) |>
  select_if(~ n_distinct(.) > 1)

pin_write(.board, text_features, "text_features")
