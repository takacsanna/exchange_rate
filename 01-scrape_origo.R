library(tidyverse)
library(rvest)
############################# Még szépítem, de az adatbázis megvan ##############################


#linkek létrehozása az oldalakhoz
alap <- "https://www.origo.hu/hir-archivum/"
#forma: https://www.origo.hu/hir-archivum/2022/20220101.html

datumok2020 <- format(seq(as.Date("2020-10-01"), as.Date("2020-12-31"), by="days"), format="%Y-%m-%d") %>% 
  as.character() %>% 
  gsub("-", "", .)

datumok2021 <- format(seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by="days"), format="%Y-%m-%d") %>% 
  as.character() %>% 
  gsub("-", "", .)

datumok2022 <- format(seq(as.Date("2022-01-01"), as.Date("2022-12-31"), by="days"), format="%Y-%m-%d") %>% 
  as.character() %>% 
  gsub("-", "", .)


url2020 <- str_c(alap, "2020/", datumok2020, ".html")
url2021 <- str_c(alap, "2021/", datumok2021, ".html")
url2022 <- str_c(alap, "2022/", datumok2022, ".html")

url <- c(url2020, url2021, url2022)
  
origo_add_df <- tibble(url)

origo_add_df <- origo_add_df %>% 
  mutate(
    page = map(url, read_html),
    nodes = map(page, ~ html_nodes(., ".archive-cikk a")),
    cim = map(nodes, html_text),
    url_to_cim = map(nodes, html_attr, "href")
  )

origo_add_df <- origo_add_df %>% 
  select(url_to_cim, cim) %>% 
  unnest(cols = c(url_to_cim, cim)) %>% 
  na.omit() %>%
  unique() 

get_text <- function(x) {
  text <- n_times_try({
    closeAllConnections()
    Sys.sleep(.2)
    read_html(x) %>%
      html_nodes("p") %>%
      html_text() %>%
      .[-1] %>% 
      paste(., collapse = " ")
  },
  sleep_times = c(rep(c(0, 3, 15), 5), rep(180, 3), rep(15, 4)),
  otherwise = as.character(NA)
  )
  
  tibble(url = x, text)
}

cikkek <- function(link) {
  link %>% 
    read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[-1] %>% 
    paste(., collapse = " ")
}

granatlib::script_background("01-scrape_index.R")


origo_add_df<-origo_add_df %>% 
  mutate(
    szoveg = map(url_to_cim, get_text)
  )

origo_add_df<-origo_add_df %>% 
  unnest(szoveg)

#dátum
proba <- read_html("https://www.origo.hu/auto/20201201-leallitottak-a-megyei-autopalyamatricak-eladasat.html") %>% 
  html_nodes(".article-date") %>% 
  html_text() %>% 
  str_extract("20\\d\\d.\\d\\d.\\d\\d. \\d\\d:\\d\\d")
proba
datum <- function(link) {
  link %>% 
    read_html() %>% 
    html_nodes(".article-date") %>% 
    html_text() %>% 
    str_extract("20\\d\\d.\\d\\d.\\d\\d. \\d\\d:\\d\\d")
}

get_date <- function(x) {
  text <- n_times_try({
    closeAllConnections()
    Sys.sleep(.2)
    read_html(x) %>%
      html_nodes(".article-date") %>% 
      html_text() %>% 
      str_extract("20\\d\\d.\\d\\d.\\d\\d. \\d\\d:\\d\\d")
  },
  sleep_times = c(rep(c(0, 3, 15), 5), rep(180, 3), rep(15, 4)),
  otherwise = as.character(NA)
  )
  
  tibble(url = x, text)
}

origo_add_df<-origo_add_df %>% 
  mutate(
    date = map(url_to_cim, get_date)
  )
origo_add_df3 = subset(origo_add_df, select = -c(url))
origo_add_df3<-origo_add_df3 %>% 
  unnest(date)
df2 <- rename(origo_add_df3, szoveg = text)

df2<-df2 %>% 
  unnest(date)
df2<-rename(df2, date = text)
df2 <- as_data_frame(df2)
df2$szoveg <- as.character(df2$szoveg)
df2$date <- as.character(df2$date)


#ékezetek
df2$cim <- iconv(df2$cim,from="UTF-8",to="ASCII//TRANSLIT")
df2$szoveg <- iconv(df2$szoveg,from="UTF-8",to="ASCII//TRANSLIT")
df2 = subset(df2, select = -c(url))

df2 %>% 
  write_csv(file = str_c("origo_0212.csv"))

df5<-read_csv("origo_0212.csv")
origo_add_df
