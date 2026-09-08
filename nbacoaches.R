# load-packages ----
library(tidyverse)
library(rvest)
library(janitor)


# read page ---

nbacoaches <- "https://www.basketball-reference.com/coaches/NBA_stats.html"
nbapage<- read_html("https://www.basketball-reference.com/coaches/NBA_stats.html")

# get data ---

nbacoachnames<- nbapage |>
  html_elements("#coaches") |>
  html_table()


nbacoachnames <- as.data.frame(nbacoachnames) |>
  select("Var.2", "Var.4")

nbacoachnames <- clean_names(nbacoachnames)

nbacoachnames <- nbacoachnames |>
  filter(var_2 != "Coach") |>
  filter(var_2 != "") |>
  rename("coach" = "var_2") |>
  rename("current" = "var_4") |>
  filter(current == 2027)

nbacoachnames <- nbacoachnames |>
  mutate(
    team = case_when(
      coach == "David Adelman" ~ "Denver Nuggets",
      coach == "Kenny Atkinson" ~ "Cleveland Cavaliers",
      coach == "J.B. Bickerstaff" ~ "Detroit Pistons",
      coach == "Mike Brown" ~ "New York Knicks",
      coach == "Rick Carlisle" ~ "Indiana Pacers",
      coach == "Doug Christie" ~ "Sacramento Kings",
      coach == "Mark Daigneault" ~ "Oklahoma City Thunder",
      coach == "Jordi Fernández Torres" ~ "Brooklyn Nets",
      coach == "Chris Finch" ~ "Minnesota Timberwolves",
      coach == "Will Hardy" ~ "Utah Jazz",
      coach == "Tuomas Iisalo" ~ "Memphis Grizzlies",
      coach == "Taylor Jenkins" ~ "Milwaukee Bucks",
      coach == "Mitch Johnson" ~ "San Antonio Spurs",
      coach == "Brian Keefe" ~ "Washington Wizards",
      coach == "Steve Kerr" ~ "Golden State Warriors",
      coach == "Charles Lee" ~ "Charlotte Hornets",
      coach == "Tyronn Lue" ~ "Los Angeles Clippers",
      coach == "Dusty May" ~ "Dallas Mavericks",
      coach == "Joe Mazzulla" ~ "Boston Celtics",
      coach == "Jamahl Mosley" ~ "New Orleans Pelicans",
      coach == "Micah Nori" ~ "Portland Trail Blazers",
      coach == "Nick Nurse" ~ "Philadelphia 76ers",
      coach == "Jordan Ott" ~ "Phoenix Suns",
      coach == "Darko Rajakovic" ~ "Toronto Raptors",
      coach == "JJ Redick" ~ "Los Angeles Lakers",
      coach == "Quin Snyder" ~ "Atlanta Hawks",
      coach == "Tiago Splitter" ~ "Chicago Bulls",
      coach == "Erik Spoelstra" ~ "Miami Heat",
      coach == "Sean Sweeney" ~ "Orlando Magic",
      coach == "Ime Udoka" ~ "Houston Rockets"
      
    )
  )
nbacoachnames <- nbacoachnames |>
  select(coach, team)