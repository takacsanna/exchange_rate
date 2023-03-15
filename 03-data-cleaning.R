raw_news_df <- pin_read(.board, "raw_news_df")

set.seed(1)

sm1 <- raw_news_df |>
  group_by(medium) |>
  sample_n(500)

trigram_tf_idf <- sm1 |>
  unnest_ngrams("ngram", text, n = 5) |>
  count(medium, ngram) |>
  bind_tf_idf(ngram, medium, n)

trigram_df_idf <- read_csv("C:/Users/Anna/Documents/egyi/MNB/tdk/proba/adatok/tfidf0312.csv")

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
?filter
#Erről a témáról ide kattintva angol nyelven is olvashat a Telex English oldalán. Nagyon kevés az olyan magyarországi lap, amelyik politikától független, és angol nyelvű híreket is kínál. A Telex viszont ilyen, naponta többször közöljük minden olyan anyagunkat angolul is, amelynek nemzetközi relevanciája van, és az angolul olvasó közönségnek is érdekes lehet: hírek, politikai elemzések, tényfeltárások, színes riportok. Vigye hírét a Telex English rovatnak, Twitterünknek és angol nyelvű heti hírlevelünknek az angolul olvasó ismerősei között!
#Ha szeretne még több érdekes techhírt olvasni, akkor kövesse az Origo Techbázis Facebook-oldalát, kattintson ide!

raw_2 <- raw_news_df %>% 
  filter(medium == "blikk") %>% 
  mutate(text = str_extract(text, "Ezeket látta már.*?[.]"))
unique(raw_2$text)
raw_3 <- raw_news_df %>% 
  filter(medium == "telex") %>% 
  mutate(text = str_remove(text, "Erről a témáról ide kattintva angol nyelven is olvashat a Telex English oldalán. Nagyon kevés az olyan magyarországi lap, amelyik politikától független, és angol nyelvű híreket is kínál. A Telex viszont ilyen, naponta többször közöljük minden olyan anyagunkat angolul is, amelynek nemzetközi relevanciája van, és az angolul olvasó közönségnek is érdekes lehet: hírek, politikai elemzések, tényfeltárások, színes riportok. Vigye hírét a Telex English rovatnak, Twitterünknek és angol nyelvű heti hírlevelünknek az angolul olvasó ismerősei között!"))

raw_4 <- raw_news_df %>% 
  filter(medium == "origo") %>% 
  mutate(text = str_remove(text, "Ha szeretne még több érdekes techhírt olvasni, akkor kövesse az Origo Techbázis Facebook-oldalát, kattintson ide!"))

unique(raw_news_df$medium)

raw_5 <- raw_news_df %>% 
  filter(medium == "huszonnegy" | medium == "portfolio" | medium == "index")

tiszta <- rbind(raw_2, raw_3, raw_4, raw_5)

sm2 <- tiszta |>
  group_by(medium) |>
  sample_n(100)

proba <- sm2 |>
  unnest_ngrams("ngram", text, n = 5) |>
  count(medium, ngram) |>
  bind_tf_idf(ngram, medium, n)

proba %>% 
  write_csv("probatf3.csv")

proba |>
  slice_max(n, n = 10) |>
  left_join(sm2, multiple = "all") |>
  filter(str_detect(text, ngram)) |>
  distinct(medium, ngram, .keep_all = TRUE) |>
  mutate(text = str_replace_all(text, ngram, str_c("**", ngram, "**"))) |>
  group_by(medium) |>
  select(medium, n, text) |>
  gt() |>
  fmt_markdown(text, rows = everything())
