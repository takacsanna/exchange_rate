accesed_time <- Sys.time()

search_url <- paste0("https://index.hu/24ora/gazdasag/?tol=",
                     "2020-01-01", # start day
                     "&ig=",
                     format(as.Date(Sys.Date()), "%Y-%m-%d"),
                     "&rovat=gazdasag&pepe=1&word=1"
)


scrape_urls <- read_html(search_url) %>%
  html_elements(".found") %>%
  html_text %>%
  str_extract("\\d+") %>%
  as.numeric |>
  (\(x) str_c(search_url, '&p=', 0:(x %/% 60))) ()

get_news_meta <- function(x) {
  n_times_try({
    Sys.sleep(1)
    closeAllConnections()
    page <- read_html(x)
    tibble(
      time = html_nodes(page, ".cikk-date-label") |>
        html_text() |>
        reduce(c),
      url = html_nodes(page, ".cim a") |>
        html_attr("href") |>
        reduce(c)
    ) |>
      (\(x) if (is_tibble(x)) x else stop("Not tibble")) ()
  }, sleep_times = c(7, rep(10, 15)))
}

news_meta_df <- progress_map(scrape_urls, get_news_meta) |>
  bind_rows() |>
  mutate(
    time = str_squish(time)
  )

.board |>
  pin_write(
    list(
      accesed_time = accesed_time,
      data = news_meta_df
    ),
    "index_meta"
  )

get_text <- function(x) {
  text <- n_times_try({
    closeAllConnections()
    Sys.sleep(.2)
    read_html(x) %>%
      html_nodes(".cikk-torzs>p, .cikk-torzs>blockquote>p") %>%
      html_text() %>%
      str_flatten(" ")
  },
  sleep_times = c(rep(c(3, 3, 15), 5), rep(180, 3), rep(15, 4)),
  otherwise = as.character(NA)
  )

  tibble(url = x, text)
}

news_text_df <- pin_read(.board, "index_meta") |>
  pluck("data") |>
  pull(url) |>
  cache_map(get_text, .id = "index_text") |>
  bind_rows()

news_text_df |>
  pin_write(
    board = .board,
    name = "index_text"
  )

