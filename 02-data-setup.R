
# general -----------------------------------------------------

month_names_hun <- c("január", "február", "március", "április", "május", "június", "július",
                     "augusztus", "szeptember", "október", "november", "december")

# exchange_rate -----------------------------------------------

eurhuf_df <- pin_read(.board, "eurhuf_df")

date_filter <- function(.data) {
  .data |>
    select(time, text) |>
    drop_na() |>
    filter(time >= min(eurhuf_df$time) - 5, time <= max(eurhuf_df$time) + 5)
}

# index -------------------------------------------------------


index <- pin_read(.board, "index_meta") |>
  pluck(2) |>
  tibble() |>
  left_join(pin_read(.board, "index_text"), by = "url", multiple = "all") |>
  transmute(
    time = str_replace_all(time, set_names(month.name, month_names_hun)),
    time = str_replace(time, "tegnap", as.character(as.Date(pin_read(.board, "index_meta")[[1]]))),
    time = ifelse(str_starts(time, "20"), time, str_c("2023 ", time)),
    time = ymd_hm(time),
    text
  ) |>
  date_filter()


# telex -------------------------------------------------------

telex <- pin_read(.board, "telex") |>
  transmute(
    time = str_replace_all(date, set_names(month.name, c("januar", "februar", "marcius", "aprilis", "majus", "junius", "julius", "augusztus", "szeptermber", "oktober", "november", "december"))),
    time = ymd_hm(time),
    text
  ) |>
  date_filter()

# blikk -------------------------------------------------------

blikk <- pin_read(.board, "blikk_meta") |>
  pluck(2) |>
  left_join(pin_read(.board, "blikk_text_df"), multiple = "all", by = "url") |>
  tibble() |>
  distinct(url, .keep_all = TRUE) |>
  transmute(time = ymd_hm(time), text) |>
  date_filter()


# 24 ----------------------------------------------------------

huszonnegy <- pin_read(.board, "huszonnegy") |>
  transmute(time = ymd_hm(date), text = szoveg) |>
  drop_na(text) |>
  date_filter()

# portfolio ---------------------------------------------------

portfolio <- pin_read(.board, "portfolio") |>
  first() |>
  tibble() |>
  transmute(
    time = str_replace_all(time, set_names(month.name, month_names_hun)),
    time = ymd_hm(time),
    text
  ) |>
  date_filter()

# origo -------------------------------------------------------

origo <- pin_read(.board, "origo_text") |>
  mutate(time = ymd_hm(time)) |>
  date_filter()

# reduce ------------------------------------------------------

raw_news_df <- map_dfr(c("index", "telex", "blikk", "huszonnegy", "portfolio", "origo"), \(x) {
  get(x) |>
    mutate(medium = x, .after = 1)
}) |>
  arrange(medium, time)

pin_write(.board, raw_news_df, "raw_news_df")
