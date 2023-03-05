origo_landing_urls <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-03-5"), by = "days") |>
  (\(x) paste0("https://www.origo.hu/hir-archivum/", lubridate::year(x), "/", str_remove_all(x, "-"), ".html")) ()


origo_article_urls <- origo_landing_urls |>
  cp_map(name = "origo_meta", \(x) read_html(x) |>
           html_nodes(".archive-cikk a") |>
           html_attr("href")
  ) |>
  reduce(c)

news_text_df <- origo_article_urls |>
  cp_map_dfr(name = "origo_text", function(x) {
    n_times_try({
      Sys.sleep(1)
      closeAllConnections()
      p <- read_html(x)

      time <- p |>
        html_nodes(".article-date") |>
        html_text()

      text <- p |>
        html_nodes("p") |>
        html_text() |>
        str_flatten(" ")

      tibble(time, url = x, text)
    }, otherwise = tibble(time = NA_character_, url = x, text = NA_character_))
  })

news_text_df |>
  pin_write(
    board = .board,
    name = "origo_text"
  )
