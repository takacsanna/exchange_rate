listing_page <- function(topic, page) {
  paste0("https://www.portfolio.hu/", topic , "?page=", page)
}

topics <- c("cimke/orosz-ukrán%20háború", "cimke/befektetes", "cimke/bank", "cimke/deviza", "cimke/gazdasag", "global", "cimke/tudomány", "uzlet", "cimke/ingatlan", "unios-forrasok")

listing_urls <- map(topics, \(topic) {
  n_page <- listing_page(topic, 2) |>
    read_html() |>
    html_nodes(".page-link") |>
    html_text() |>
    pluck(-2) |>
    as.numeric()

  listing_page(topic, seq(n_page))
}) |>
  reduce(c)


options(currr.workers = 1, currr.wait = 100)

article_urls <- cp_map(listing_urls, \(u) {
  if (str_ends(u, "=1")) {
    node <- ".extratop a"
  } else {
    node <- ".article-lists .col-md-12 a"
  }

  u |>
    read_html() |>
    html_nodes(node) |>
    html_attr("href")
}) |>
  reduce(c) |>
  unique()

get_text <- function(x) {

p <- x |>
  read_html()

time <- p |>
  html_nodes(".d-block") |>
  html_text() |>
  first()

text <- p |>
  html_nodes(".language-hu p , .pfarticle-section-lead") |>
  html_text() |>
  str_flatten(" ")

tibble(url = x, time, text)
}

portfolio_text_df <- cp_map_dfr(article_urls, get_text, name = "portfolio_text")

pin_write(
    .board,
    list(portfolio_text_df,
         accesed_time = Sys.time()),
    name = "portfolio"
)
