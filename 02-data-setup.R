
# index -------------------------------------------------------

index_raw_df <- pin_read(.board, "index") |>
  pluck("data") |>
  tibble()

index_raw_df
