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
    Sys.sleep(.5)
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

get_text <- function(x) {
  text <- n_times_try({
    closeAllConnections()
    Sys.sleep(.3)
    read_html(x) %>%
      html_nodes(".cikk-torzs>p, .cikk-torzs>blockquote>p") %>%
      html_text() %>%
      str_flatten(" ")
  },
  sleep_times = c(rep(1, 3), rep(15, 4), rep(180, 3), rep(15, 4)),
  otherwise = as.character(NA)
  )
  tibble(url = x, text)
}

news_text_df <- progress_map(news_meta_df$url, get_text)

list(
  accesed_time = accesed_time,
  data = full_join(news_meta_df, bind_rows(news_text_df), by = "url")
) |>
  pin_write(
    board = .board,
    name = "index"
  )

