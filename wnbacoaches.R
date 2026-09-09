

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
        "Karl Smesko", "Tyler Marsh", "Rachid Meziane", "Jose Fernandez",
        "Chris DeMarco", "Nate Tibbetts", "Alex Sarama", "Sydney Johnson"
        ) ~ "M",
      coach %in% c("Natalie Nakase", "Stephanie White", "Lynne Roberts",
                   "Becky Hammon", "Cheryl Reeve", "Sofia Raman",
                   "Sandy Brondello", "Sonia Raman") ~ "F")
  )

# assistant coaches ----

atl_asstsnames <- tibble(
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
gsv_asstsnames <- tibble(
  coach = c("Sugar Rodgers", "Kasib Powell", "Landon Tatum"),
  team = "GSV"
)|>
  mutate(
    gender = if_else(coach == "Sugar Rodgers", "F", "M")
  )
ind_asstsnames <- tibble(
  coach = c("Briann January", "Karima Christmas-Kelly", "Austin Kelly"),
  team = "IND"
)|>
  mutate(
    gender = if_else(coach == "Austin Kelly", "M", "F")
  )
las_asstsnames <- tibble(
  coach = c("Ebony Hoffman", "Zach O’Brien", "Zak Buncik"),
  team = "LAS"
)|>
  mutate(
    gender = if_else(coach == "Ebony Hoffman", "F", "M")
  )
lva_asstsnames <- tibble(
  coach = c("Nola Henry", "Larry Lewis", "Ty Ellis", "Charlene Thomas-Swinson",
            "Micah Fraction"),
  team = "LVA"
) |>
  mutate(
    gender = if_else(coach %in% c("Ty Ellis", "Larry Lewis", "Micah Fraction"),
                     "M", "F")
  )
min_asstsnames <- tibble(
  coach = c("Janel McCarville", "Eric Thibault", "Lindsay Whalen",
            "Rebekkah Brunson"),
  team = "MIN"
)|>
  mutate(
    gender = if_else(coach == "Eric Thibault", "M", "F")
  )
nyl_asstsnames <- tibble(
  coach = c("Courtney Paris", "Addi Walters", "Will Sheehey",
            "Andrew Wade"),
  team = "NYL"
)|>
  mutate(
    gender = if_else(coach %in% c("Andrew Wade", "Will Sheehey"),
                     "M", "F")
  )
pho_asstsnames <- tibble(
  coach = c("Kristi Toliver", "Megan Vogel", "TC Swirsky",
            "John McCullough", "Chevelle Saunsoci", "Tangela Smith"),
  team = "PHO"
)|>
  mutate(
    gender = if_else(coach %in% c("TC Swirsky", "John McCullough"),
                     "M", "F")
  )
por_asstsnames <- tibble(
  coach = c("Danielle Boiago", "Sefu Bernard", "Sylvia Fowles",
            "Brittni Donaldson"),
  team = "POR"
)|>
  mutate(
    gender = if_else(coach == "Sefu Bernard",
                     "M", "F")
  )
sea_asstsnames <- tibble(
  coach = c("Natalie Achonwa", "Jarell Christian", "Michael Joiner"),
  team = "SEA"
)|>
  mutate(
    gender = if_else(coach == "Natalie Achonwa",
                     "F", "M")
  )
tor_asstsnames <- tibble(
  coach = c("Ciara Carl", "Brian Lankton", "Carly Clarke",
            "Sadie Edwards", "Olaf Lange"),
  team = "TOR"
)|>
  mutate(
    gender = if_else(coach %in% c("Brian Lankton", "Olaf Lange"),
                     "M", "F")
  )
was_asstsnames <- tibble(
  coach = c("Barbara Turner", "Jessie Miller", "Emre Vatansever"),
  team = "WAS"
)|>
  mutate(
    gender = if_else(coach == "Emre Vatansever",
                     "M", "F")
  )

# add to coaches ----
wnbacoachnames <- wnbacoachnames |>
  bind_rows(was_asstsnames,tor_asstsnames, sea_asstsnames, por_asstsnames,
            pho_asstsnames, nyl_asstsnames, min_asstsnames, lva_asstsnames,
            las_asstsnames, ind_asstsnames, gsv_asstsnames, dal_asstsnames,
            con_asstsnames, chi_asstsnames, atl_asstsnames)

# signify head coaches ----

wnbacoachnames <- wnbacoachnames |>
  mutate(
    hc = if_else(coach %in% c("Karl Smesko", "Tyler Marsh", "Rachid Meziane",
                              "Jose Fernandez", "Chris DeMarco",
                              "Nate Tibbetts", "Alex Sarama",
                              "Sydney Johnson", "Natalie Nakase",
                              "Stephanie White", "Lynne Roberts",
                              "Becky Hammon", "Cheryl Reeve", "Sofia Raman",
                              "Sandy Brondello", "Sonia Raman"),
                 "Yes", "No")
  )