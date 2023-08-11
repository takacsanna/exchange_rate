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
  library(currr)
  library(tidytext)
  library(quanteda)
  library(gt)
  library(parallel)
  library(doParallel)
  options(dplyr.summarise.inform = FALSE)
  options(future.globals.maxSize = 891289600)
  options(todor_extra = c("qmd", "md", "txt", "r"))
  options(currr.folder = ".currr")
})

theme_set(
  theme_minimal() +
    theme(
      legend.position = "bottom"
    )
)

granatlib::create_pin("exchange-rate")
