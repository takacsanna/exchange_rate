library(tidyverse)
library(rvest)

#linkek generálása az oldalakhoz
url_eredeti <- "https://24.hu/"

oldalak <- c("belfold/", "kulfold/", "fn/gazdasag/", "kultura/", "tech/", "elet-stilus/", "szorakozas/", "kozelet/",
             "europoli/", "fn/uzleti-tippek/", "tudomany/", "sport/", "otthon/", "velemeny/")

url_label <- paste0(url_eredeti, oldalak)
url_ending <- str_c("page/", 2:5)
url_ending <- c("", url_ending)

url_vegleges <- c("")

for (label in url_label) {
  url <- str_c(label, url_ending)
  url_vegleges = append(url_vegleges,url)
}
url_vegleges <- url_vegleges[-1]

#tibble létrehozása
husz_add_df <- tibble(url_vegleges)

#cikkcímek és a rájuk mutató link kimentése és kibontása tibble-ben
husz_add_df <- husz_add_df %>%
  mutate(
    page = map(url, read_html),
    nodes = map(page, ~ html_nodes(., ".m-articleWidget__link")),
    cim = map(nodes, html_text),
    url_to_cim = map(nodes, html_attr, "href")
  )

husz_add_df <- husz_add_df %>%
  select(url_to_cim, cim) %>%
  unnest(cols = c(url_to_cim, cim)) %>%
  na.omit() %>%
  unique()

#helytelen linkek kidobása
husz_add_df <- subset(husz_add_df, substr(url_to_cim,1,1) == "h")


#cikkek szövege

cikkek <- function(link) {
  link %>%
  read_html() %>%
    html_nodes("p") %>%
    html_text() %>%
    .[-1] %>%
    paste(., collapse = " ")
}


husz_add_df<-husz_add_df %>%
  mutate(
    szoveg = map(url_to_cim, cikkek)
  )

husz_add_df

#dátum

datum <- function(link) {
  link %>%
    read_html() %>%
    html_nodes(".a-date") %>%
    html_text() %>%
    str_extract("2023. \\d\\d. \\d\\d. \\d\\d:\\d\\d")
}

husz_add_df<-husz_add_df %>%
  mutate(
    date = map(url_to_cim, datum)
  ) %>%
  set_names("id", "cim", "url", "szoveg", "date")

#kisebb átalakítások és tisztítás
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

pin_write(.board, df, "huszonnegy")

