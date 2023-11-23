text_features <- pin_read(.board, "text_features")


bux <- read_csv(file = "BUX_MLD.csv") %>%
  filter(`Date-Time` > "2022-01-01") %>%
  select(time = `Date-Time`, price = Last) %>%
  drop_na(price)


design_df <- bux %>%
  mutate(l_time = lag(time)) %>%
  mutate(
    map2(time, l_time, ~ filter(text_features, time < .x, time >= .y), .progress = TRUE) %>%
      map(select, - 1) %>%
      map(\(x) {
        if (nrow(x) == 0) {
          # replace all columns with a single 0
          reframe(x, across(everything(), ~ 0))
        } else {
          summarise(x, across(everything(), ~ sum(.x)))
        }
      }, .progress = TRUE) %>%
      list_rbind()
  ) %>%
  slice(-1) %>%
  select(- l_time)

pin_write(.board, design_df, "design_df")
