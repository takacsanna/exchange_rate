suppressPackageStartupMessages({
  library(magrittr)
  library(tidyverse)
  library(lubridate)
  library(rvest)
  library(pins)
  library(janitor)
  library(tidymodels)
  library(knitr)
  library(broom)
  library(DT)
  library(granatlib) # < github
  library(patchwork)
  library(furrr)
  options(dplyr.summarise.inform = FALSE)
  options(future.globals.maxSize = 891289600)
  options(todor_extra = c("qmd", "md", "txt", "r"))
})

theme_set(
  theme_minimal() +
    theme(
      legend.position = "bottom"
    )
)

granatlib::create_pin("exchange-rate")