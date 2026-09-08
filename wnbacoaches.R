

# load-packages ----
library(tidyverse)
library(rvest)
library(janitor)


# read page
wnbacoaches <- "https://www.basketball-reference.com/wnba/years/2026_coaches.html"
wnbacoachespage <- read_html(wnbacoaches)

wnbacoachnames<- wnbacoachespage |>
  html_elements("#coaches") |>
  html_table()

wnbacoachnames <- as.data.frame(wnbacoachnames) |>
  select("Var.1", "Var.2")

wnbacoachnames <- clean_names(wnbacoachnames)

wnbacoachnames <- wnbacoachnames |>
  filter(var_1 != "Coach") |>
  filter(var_1 != "") |>
  rename("coach" = "var_1") |>
  rename("team" = "var_2")
