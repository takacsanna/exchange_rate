library(tidyverse)
library(rvest)

page <- read_html("https://telex.hu")

#címsorok kiválasztása
my_node <- page %>% 
  html_nodes(".item__title")

#címek szöveggé alakítása
cim <- my_node %>% 
  html_text() %>% 
  as_data_frame(.) %>% 
  mutate(
    id = row_number()
  )

#url-ek
url <- my_node %>% 
  html_attr("href") %>% 
  as_data_frame(.) %>% 
  mutate(
    id = row_number()
  )
df <- merge(cim, url, by = "id") %>% 
  na.omit() %>% 
  unique() %>% 
  set_names("id", "cim", "url")

df<- df %>% 
  mutate(
    url2 = ifelse(substr(url,1,1) == "/", str_c("https://telex.hu", url), url)
  )

df

cikkek <- function(link) {
  link %>% 
    read_html() %>% 
    html_nodes(".article-html-content p") %>% 
    html_text() %>% 
    #.[-1] %>% 
    paste(., collapse = " ")
}

cikkek("https://telex.hu/gazdasag/2023/01/14/csaladtamogatas-csok-falusi-otthonfelujitas-babavaro-anya-gyerek")

df<-df %>% 
  mutate(
    szoveg = map(url2, cikkek)
  )

df

proba <- read_html("https://telex.hu/gasztro/2023/01/17/energiavalsag-etterem-vendeglatas-csod-bezaras-inflacio") %>% 
  html_nodes(".history--original span") %>% 
  html_text() %>% 
  str_extract("2023. január \\d\\d. – \\d\\d:\\d\\d")
proba

datum <- function(link) {
  link %>% 
    read_html() %>% 
    html_nodes(".history--original span") %>% 
    html_text() %>% 
    str_extract("2023. január \\d\\d. – \\d\\d:\\d\\d")
}

datum("https://telex.hu/szorakozas/2023/01/17/marcius-1-mandalorian-uj-evada-star-wars-disney-elozetes")

df<-df %>% 
  mutate(
    date = map(url2, datum)
  ) %>% 
  set_names("id", "cim", "url", "url2", "szoveg", "date")

df <- as_data_frame(df)
df$szoveg <- as.character(df$szoveg)
df$date <- as.character(df$date)

df<-df %>% 
  drop_na()
df$cim <- iconv(df$cim,from="UTF-8",to="ASCII//TRANSLIT")
df$szoveg <- iconv(df$szoveg,from="UTF-8",to="ASCII//TRANSLIT")

df %>% 
  write_csv(file = str_c("telex_17.csv"))
