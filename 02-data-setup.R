
# general -----------------------------------------------------

month_names_hun <- c("január", "február", "március", "április", "május", "június", "július",
                     "augusztus", "szeptember", "október", "november", "december")

# index -------------------------------------------------------

index_meta <- pin_read(.board, "index_meta")
index_raw_df <- pin_read(.board, "index_text")

index_meta |>
  pluck(2) |>
  tibble() |>
  left_join(index_raw_df, by = "url") |>
  mutate(
    time = str_replace_all(time, set_names(month.name, month_names_hun)),
    time = str_replace(time, "tegnap", as.character(as.Date(index_meta[[1]]))),
    time = ifelse(str_starts(time, "20"), time, str_c("2023 ", time)),
    time = ymd_hm(time)
  ) |>
  pin_write(
    board = .board,
    name = "index"
  )

