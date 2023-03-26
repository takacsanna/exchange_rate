raw_news_df <- pin_read(.board, "raw_news_df")
set.seed(1)

sm1 <- raw_news_df |>
  group_by(medium) |>
  sample_n(100)

trigram_tf_idf <- sm1 |>
  unnest_ngrams("ngram", text, n = 5) |>
  count(medium, ngram) |>
  bind_tf_idf(ngram, medium, n)

trigram_tf_idf |>
  slice_max(n, n = 10) |>
  left_join(sm1, multiple = "all") |>
  filter(str_detect(text, ngram)) |>
  distinct(medium, ngram, .keep_all = TRUE) |>
  mutate(text = str_replace_all(text, ngram, str_c("**", ngram, "**"))) |>
  group_by(medium) |>
  select(medium, n, text) |>
  gt() |>
  fmt_markdown(text, rows = everything())

raw_news_df$time <- as.character(raw_news_df$time)

raw_3 <- raw_news_df %>%
  filter(medium == "telex") %>%
  mutate(text = str_remove(text, "Erről a témáról ide kattintva angol nyelven is olvashat a Telex English oldalán. Nagyon kevés az olyan magyarországi lap, amelyik politikától független, és angol nyelvű híreket is kínál. A Telex viszont ilyen, naponta többször közöljük minden olyan anyagunkat angolul is, amelynek nemzetközi relevanciája van, és az angolul olvasó közönségnek is érdekes lehet: hírek, politikai elemzések, tényfeltárások, színes riportok. Vigye hírét a Telex English rovatnak, Twitterünknek és angol nyelvű heti hírlevelünknek az angolul olvasó ismerősei között!"))


raw_4 <- raw_news_df %>%
  filter(medium == "origo") %>%
  mutate(text = str_remove(text, "Ha szeretne még több érdekes techhírt olvasni, akkor kövesse az Origo Techbázis Facebook-oldalát, kattintson ide!"))

raw_5 <- raw_news_df |>
  filter(medium == "blikk") |>
  mutate(
    text = str_remove(text, "Ez is érdekelheti.*$") |>
      str_remove("Ezeket látta már*$")
  )

raw_6 <- raw_news_df %>%
  filter(medium == "huszonnegy" | medium == "portfolio" | medium == "index")

clean_news_df <- rbind(raw_3, raw_4, raw_5, raw_6) |>
  filter(medium %in% c("telex", "origo", "blikk", "huszonnegy", "portfolio", "index"))

pin_write(.board, clean_news_df, "clean_news_df")

sm2 <- clean_news_df |>
  group_by(medium) |>
  sample_n(100, replace = T)

df <- sm2 |>
  unnest_ngrams("ngram", text, n = 5) |>
  count(medium, ngram) |>
  bind_tf_idf(ngram, medium, n)

df |>
  slice_max(n, n = 10) |>
  left_join(sm2, multiple = "all") |>
  filter(str_detect(text, ngram)) |>
  distinct(medium, ngram, .keep_all = TRUE) |>
  mutate(text = str_replace_all(text, ngram, str_c("**", ngram, "**"))) |>
  group_by(medium) |>
  select(medium, n, text) |>
  gt() |>
  fmt_markdown(text, rows = everything())
