# load-packages ----
library(tidyverse)
library(rvest)
library(janitor)


# read page
mbbcoaches <- "https://www.sports-reference.com/cbb/seasons/men/2026-coaches.html"
mbbcoachespage <- read_html(mbbcoaches)

mbbcoachnames<- mbbcoachespage |>
  html_elements("#coaches") |>
  html_table()

mbbcoachnames <- as.data.frame(mbbcoachnames) |>
  select("Var.1")

mbbcoachnames <- clean_names(mbbcoachnames)

mbbcoachnames <- mbbcoachnames |>
  filter(var_1 != "Coach") |>
  filter(var_1 != "") |>
  rename("coach" = "var_1")
