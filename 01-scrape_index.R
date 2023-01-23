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
    )
  }, sleep_times = c(5, 10, 20, 30, 30), print_warning = TRUE)
}


news_meta_df <- safely_map(scrape_urls, get_news_meta) |>
  bind_rows() |>
  mutate(
    time = str_squish(time)
  )

get_text <- function(x) {
  text <- n_times_try({
    read_html(x) %>%
      html_nodes(".cikk-torzs>p, .cikk-torzs>blockquote>p") %>%
      html_text %>%
      str_flatten(" ")
  }, sleep_times = c(5, 10, 20, 30, 30), otherwise = NA_character_, print_warning = TRUE)

  tibble(url = x, text)
}

news_text_df <- safely_map(news_meta_df$url[1:5], get_text) |>
  bind_rows()


list(
  accesed_time = accesed_time,
  data = full_join(news_meta_df, news_text_df, by = "url")
) |>
  pin_write(
    board = .board,
    name = "index"
  )

