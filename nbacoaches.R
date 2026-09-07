# load-packages ----
library(tidyverse)
library(rvest)
library(janitor)


# read page

nbacoaches <- "https://www.basketball-reference.com/coaches/NBA_stats.html"
nbapage<- read_html("https://www.basketball-reference.com/coaches/NBA_stats.html")

nbacoachnames<- nbapage |>
  html_elements("#coaches") |>
  html_table()


nbacoachnames <- as.data.frame(nbacoachnames) |>
  select("Var.2")

nbacoachnames <- clean_names(nbacoachnames)

nbacoachnames <- nbacoachnames |>
  filter(var_2 != "Coach") |>
  filter(var_2 != "") |>
  rename("coach" = "var_2")
