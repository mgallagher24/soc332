# load-packages ----
library(tidyverse)
library(rvest)
library(janitor)


# read page
wbbcoaches <- "https://www.sports-reference.com/cbb/seasons/women/2026-coaches.html"
wbbcoachespage <- read_html(wbbcoaches)

wbbcoachnames<- wbbcoachespage |>
  html_elements("#coaches") |>
  html_table()

wbbcoachnames <- as.data.frame(wbbcoachnames) |>
  select("Var.1", "Var.2")

wbbcoachnames <- clean_names(wbbcoachnames)

wbbcoachnames <- wbbcoachnames |>
  filter(var_1 != "Coach") |>
  filter(var_1 != "") |>
  rename("coach" = "var_1") |>
  rename("school" = "var_2")
