library(tidyverse)
library(rvest)

#linkek létrehozása az oldalakhoz
alap <- "https://www.origo.hu/hir-archivum/"
#forma: https://www.origo.hu/hir-archivum/2022/20220101.html

datumok2020 <- format(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="days"), format="%Y-%m-%d") %>% 
  as.character() %>% 
  gsub("-", "", .)

url2020 <- str_c(alap, "2020/", datumok2020, ".html")
url2020

origo_add_df <- tibble(url2020)

origo_add_df <- origo_add_df %>% 
  mutate(
    page = map(url2020, read_html),
    nodes = map(page, ~ html_nodes(., ".archive-cikk a")),
    cim = map(nodes, html_text),
    url_to_cim = map(nodes, html_attr, "href")
  )

origo_add_df <- origo_add_df %>% 
  select(url_to_cim, cim) %>% 
  unnest(cols = c(url_to_cim, cim)) %>% 
  na.omit() %>%
  unique() 

cikkek <- function(link) {
  link %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[-1] %>% 
    paste(., collapse = " ")
}

origo_add_df<-origo_add_df %>% 
  mutate(
    szoveg = map(url_to_cim, cikkek)
  )

origo_add_df

#dátum
proba <- read_html("https://24.hu/kulfold/2023/01/15/london-lovoldozes-camden/") %>% 
  html_nodes(".a-date") %>% 
  html_text() %>% 
  str_extract("2023. \\d\\d. \\d\\d. \\d\\d:\\d\\d")

datum <- function(link) {
  link %>% 
    read_html() %>% 
    html_nodes(".a-date") %>% 
    html_text() %>% 
    str_extract("2023. \\d\\d. \\d\\d. \\d\\d:\\d\\d")
}

datum("https://24.hu/szorakozas/2023/01/15/pamela-anderson-sosem-olvasta-el-lily-james-levelet/")

df<-df %>% 
  mutate(
    date = map(url, datum)
  ) %>% 
  set_names("id", "cim", "url", "szoveg", "date")

df <- as_data_frame(df)
df$szoveg <- as.character(df$szoveg)
df$date <- as.character(df$date)
df <- df %>% 
  drop_na()
df <- subset(df, date!="character(0)")
df

#ékezetek
df$cim <- iconv(df$cim,from="UTF-8",to="ASCII//TRANSLIT")
df$szoveg <- iconv(df$szoveg,from="UTF-8",to="ASCII//TRANSLIT")

df %>% 
  write_csv(file = str_c("abrakadabra.csv"))
