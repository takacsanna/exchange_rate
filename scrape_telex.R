library(tidyverse)
library(rvest)

url_start <- "https://telex.hu/legfrissebb?oldal="
url_ending <- 1:5228
telex_add_df <- tibble(url = str_c(url_start, url_ending))

telex_add_df <- telex_add_df %>% 
  mutate(
    page = map(url, read_html),
    nodes = map(page, ~ html_nodes(., ".list__item__title")),
    cim = map(nodes, html_text),
    url_to_cim = map(nodes, html_attr, "href")
  )

telex_add_df <- telex_add_df %>% 
  select(url_to_cim, cim) %>% 
  unnest(cols = c(url_to_cim, cim)) %>% 
  na.omit() %>%
  unique() 

telex_add_df<- telex_add_df %>% 
  mutate(
    url_to_cim = ifelse(substr(url_to_cim,1,1) == "/", str_c("https://telex.hu", url_to_cim), url_to_cim)
  )


cikkek <- function(link) {
  link %>% 
    read_html() %>% 
    html_nodes(".article-html-content p") %>% 
    html_text() %>% 
    #.[-1] %>% 
    paste(., collapse = " ")
}

get_text <- function(x) {
  text <- n_times_try({
    closeAllConnections()
    Sys.sleep(.2)
    read_html(x) %>%
      html_nodes(".article-html-content p") %>% 
      html_text() %>% 
      paste(., collapse = " ")
  },
  sleep_times = c(rep(c(0, 3, 15), 5), rep(180, 3), rep(15, 4)),
  otherwise = as.character(NA)
  )
  
  tibble(url = x, text)
}

cikkek("https://telex.hu/gazdasag/2023/01/14/csaladtamogatas-csok-falusi-otthonfelujitas-babavaro-anya-gyerek")

telex_add_df<-telex_add_df %>% 
  mutate(
    szoveg = map(url_to_cim, get_text)
  )

proba <- read_html("https://telex.hu/gasztro/2023/01/17/energiavalsag-etterem-vendeglatas-csod-bezaras-inflacio") %>% 
  html_nodes(".history--original span") %>% 
  html_text() %>% 
  str_extract("202\\d. .* \\d\\d. – \\d\\d:\\d\\d")
proba

datum <- function(link) {
  link %>% 
    read_html() %>% 
    html_nodes(".history--original span") %>% 
    html_text() %>% 
    str_extract("202\\d. .* \\d\\d. – \\d\\d:\\d\\d")
}

datum("https://telex.hu/szorakozas/2023/01/17/marcius-1-mandalorian-uj-evada-star-wars-disney-elozetes")

get_date <- function(x) {
  text <- n_times_try({
    closeAllConnections()
    Sys.sleep(.2)
    read_html(x) %>%
      html_nodes(".history--original span") %>% 
      html_text() %>% 
      str_extract("202\\d. .* \\d\\d. – \\d\\d:\\d\\d")
  },
  sleep_times = c(rep(c(0, 3, 15), 5), rep(180, 3), rep(15, 4)),
  otherwise = as.character(NA)
  )
  
  tibble(url = x, text)
}
telex_add_df<-telex_add_df %>% 
  mutate(
    date = map(url_to_cim, get_date)
  )

telex_add_df<-telex_add_df %>% 
  unnest(date)
telex_add_df2 <- subset(telex_add_df, select = -c(url))
telex_add_df2<-telex_add_df2 %>% 
  rename(date = text)
telex_add_df<-telex_add_df2 %>% 
  unnest(szoveg)

df <- as_data_frame(telex_add_df)
df$text <- as.character(df$text)
df$date <- as.character(df$date)

telex_add_df<-telex_add_df %>% 
  #drop_na() %>% 
  unique()

df$cim <- iconv(df$cim,from="UTF-8",to="ASCII//TRANSLIT")
df$text <- iconv(df$text,from="UTF-8",to="ASCII//TRANSLIT")
df$date <- iconv(df$date,from="UTF-8",to="ASCII//TRANSLIT")

df %>% 
  write_csv(file = str_c("telex_0208.csv"))
