accesed_time <- Sys.time()

dates <- format(seq(as.Date("2020-01-01"), Sys.Date(), by="days"), format="%Y-%m-%d")

get_news_meta <- function(date) {
  url_base <- date |>
    as.character() |>
    (\(x) str_c("https://www.blikk.hu/archivum/online?date=", x, "&page=")) ()

  links_on_date <- as.character()
  article_time_total <- as.character()

  for (i in 0:100) {
    granatlib::n_times_try({

      page <- str_c(url_base, i) |>
        read_html()

      article_links <- page |>
        html_nodes(".mb-0 a") |>
        html_attr("href")

      article_time <- page |>
        html_nodes(".pr-3") |>
        html_text()

    }, sleep_times = c(2, 2, 2, 2, 2)) # TODO

    links_on_date <- c(links_on_date, article_links)
    article_time_total <- c(article_time_total, article_time)

    if (length(article_links) < 30) {
      break
    }
  }

  tibble(date, links_on_date, time = article_time_total)
}

options(currr.wait = 10, currr.folder = ".currr")

news_meta_df <- cp_map_dfr(rep(dates), get_news_meta, name = "blikk_meta") |>
  transmute(
    time = str_squish(time),
    time = paste(date, time),
    url = links_on_date
  )

.board |>
  pin_write(
    list(
      accesed_time = accesed_time,
      data = news_meta_df
    ),
    "blikk_meta"
  )

s_times <- c(rep(2, 2), 1) # avoid update caused restart at cp_map

get_text <- function(x) {
  text <- n_times_try({
    read_html(x) %>%
      html_nodes(".detail p") %>%
      html_text() %>%
      str_flatten(" ")
  },
  sleep_times = s_times,
  otherwise = as.character(NA)
  )

  tibble(url = x, text)
}

options(currr.wait = 1, currr.n_checkpoint = 100, currr.workers = 1)

text_df <- cp_map(rev(news_meta_df$url), get_text, name = "blikk_text")

text2<-text_df |>
  keep(~ !is.logical(.$text) & !is.na(.$text)) |>
  bind_rows() |>
  pin_write(
    board = .board,
    name = "blikk_text_df"
  )

text2 %>% 
  write_csv("blikktext.csv")
news_meta_df %>% 
  write_csv("meta.csv")

blikk_2 <- merge(text2, news_meta_df)
?merge
