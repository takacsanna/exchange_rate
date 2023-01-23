library(tidyverse)
library(rvest)

#linkek generálása az oldalakhoz
url_start <- "https://telex.hu/legfrissebb?oldal="
#5102
url_ending <- 1:10

#tibble létrehozása
telex_add_df <- tibble(url = str_c(url_start, url_ending))

#cikkcímek és a rájuk mutató link kimentése és kibontása tibble-ben 
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

#linkek tisztítása (van olyan, ami közvetlenül a linkre mutat)
telex_add_df<- telex_add_df %>% 
  mutate(
    url_to_cim = ifelse(substr(url_to_cim,1,1) == "/", str_c("https://telex.hu", url_to_cim), url_to_cim)
  )

#cikkek szövege
cikkek <- function(link) {
  link %>% 
    read_html() %>% 
    html_nodes(".article-html-content p") %>% 
    html_text() %>% 
    #.[-1] %>% 
    paste(., collapse = " ")
}


telex_add_df<-telex_add_df %>% 
  mutate(
    szoveg = map(url_to_cim, cikkek)
  )

#dátum
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

#kisebb átalakítások és tisztítás
telex_add_df <- as_data_frame(telex_add_df)
telex_add_df$szoveg <- as.character(telex_add_df$szoveg)
telex_add_df$date <- as.character(telex_add_df$date)

telex_add_df<-telex_add_df %>% 
  drop_na() %>% 
  unique()

#ékezetek
telex_add_df$cim <- iconv(telex_add_df$cim,from="UTF-8",to="ASCII//TRANSLIT")
telex_add_df$szoveg <- iconv(telex_add_df$szoveg,from="UTF-8",to="ASCII//TRANSLIT")
telex_add_df$date <- iconv(telex_add_df$date,from="UTF-8",to="ASCII//TRANSLIT")

#mentés csv-be
df %>% 
  write_csv(file = str_c("telex_17.csv"))
