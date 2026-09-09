

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

wnbacoachnames <- wnbacoachnames |>
  mutate(
    gender = case_when(
      coach %in% c(
        "Karl Smesko", "Tyler Marsh", "Rachid Meziane", "Jose Fernandez"
        ) ~ "M")
  )

# assistant coaches ----

wnba_asstsnames <- tibble(
  coach = c("LaToya Sanders", "Brandi Poole", "Chelsea Lyles",
  "Camryn Brown"),
  team = "ATL",
  gender = "F"
)
chi_asstsnames <- tibble(
  coach = c("Jhared Simpson", "Latricia Trammell", 
            "Rena Wakama", "Kelly Faris"),
  team = "CHI"
) |>
  mutate(
    gender = if_else(coach == "Jhared Simpson", "M", "F")
  )
con_asstsnames <- tibble(
  coach = c("Roneeka Hodges", "Ashlee McGee", "Pascal Angillis"),
  team = "CON"
) |>
  mutate(
    gender = if_else(coach == "Pascal Angillis", "M", "F")
  )
dal_asstsnames <- tibble(
  coach = c("Empress Davenport", "David Adkins", "Camille Smith",
            "Mike Neighbors"),
  team = "DAL"
)  |>
  mutate(
    gender = if_else(coach %in% c("David Adkins", "Mike Neighbors"), "M", "F")
  )