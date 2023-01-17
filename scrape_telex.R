library(tidyverse)
library(rvest)

url_start <- "https://telex.hu/legfrissebb?oldal="
#5102
url_ending <- 1:10
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
  unnest() %>% 
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

cikkek("https://telex.hu/gazdasag/2023/01/14/csaladtamogatas-csok-falusi-otthonfelujitas-babavaro-anya-gyerek")

telex_add_df<-telex_add_df %>% 
  mutate(
    szoveg = map(url_to_cim, cikkek)
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

telex_add_df<-telex_add_df %>% 
  mutate(
    date = map(url_to_cim, datum)
  )

telex_add_df <- as_data_frame(telex_add_df)
telex_add_df$szoveg <- as.character(telex_add_df$szoveg)
telex_add_df$date <- as.character(telex_add_df$date)

telex_add_df<-telex_add_df %>% 
  drop_na() %>% 
  unique()

telex_add_df$cim <- iconv(telex_add_df$cim,from="UTF-8",to="ASCII//TRANSLIT")
telex_add_df$szoveg <- iconv(telex_add_df$szoveg,from="UTF-8",to="ASCII//TRANSLIT")
telex_add_df$date <- iconv(telex_add_df$date,from="UTF-8",to="ASCII//TRANSLIT")

df %>% 
  write_csv(file = str_c("telex_17.csv"))
