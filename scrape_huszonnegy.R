library(tidyverse)
library(rvest)

page <- read_html("https://24.hu")

#címsorok kiválasztása
my_node <- page %>% 
  html_nodes(".m-articleWidget__link")

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

#df a címekkel és linkekkel
df <- merge(cim, url, by = "id") %>% 
  na.omit() %>% 
  unique() %>% 
  set_names("id", "cim", "url")

df

#szöveg
proba <- read_html("https://24.hu/kulfold/2023/01/15/london-lovoldozes-camden/") %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  .[-1] %>% 
  paste(., collapse = " ")

cikkek <- function(link) {
  link %>% 
  read_html() %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    .[-1] %>% 
    paste(., collapse = " ")
}

cikkek("https://24.hu/kulfold/2023/01/15/london-lovoldozes-camden/")

df<-df %>% 
  mutate(
    szoveg = map(url, cikkek)
  )
df

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
