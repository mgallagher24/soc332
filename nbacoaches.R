# load-packages ----

library(tidyverse)
library(rvest)
library(janitor)
library(polite)


# read page ---

nbacoaches <- "https://www.basketball-reference.com/coaches/NBA_stats.html"
nbapage<- read_html("https://www.basketball-reference.com/coaches/NBA_stats.html")

# get-data ---

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

# asst-coaches -----

bow("https://nbacoaches.com/nba-assistant-coaches/")
nba_assts <- read_html("https://nbacoaches.com/nba-assistant-coaches/")


nbaassts_names <- nba_assts |>
  html_elements("#content a") |>
  html_text()
nbaasstscoach_names <- as.data.frame(nbaassts_names)
nbaasstscoach_names <- nbaasstscoach_names |>
  mutate(nbaassts_names = str_squish(nbaassts_names))
nbaasstscoach_names <- nbaasstscoach_names |>
  mutate(
    team = case_when(
      nbaassts_names %in% c("Chad Forcier", "Rick Higgins",
      "Christopher Jones", "Scott Morrison",
      "George Rodman", "Sean Sheldon","Jason Terry",
      "Andrew Warren", "Mike Williams", "Steven Wojciechowski") ~ "Utah Jazz",
      nbaassts_names %in%
        c("J.J. Barea", "John Beckett", "Rodney Billups", "Ryan Bowen",
        "Chase Buford", "Jared Dudley", "Mike Moser", "Andrew Munson",
        "Ognjen Stojakovic", "Elvis Valcarcel") ~ "Denver Nuggets",
      nbaassts_names %in%
        c("Nathan Bubes", "Kevin Hanson", "Chris Hines",
          "Max Lefevre", "Jeff Newton", "Pablo Prigioni",
          "Elston Turner", "James White") ~ "Minnesota Timberwolves",
      nbaassts_names %in%
        c("David Akinyooye", "David Bliss", "Daniel Dixon", "Chip Engelland",
          "Grant Gibbs","Connor Johnson", "Eric Maynor", "Zoe Vernon",
          "Mike Wilks", "Kameron Woods") ~ "Oklahoma City Thunder",
      nbaassts_names %in%
        c("Nate Bjorkgren", "Ronnie Burrell", "Quinton Crawford",
          "Jonah Herscu", "James Posey","Patrick St. Andrews")
      ~ "Portland Trail Blazers",
      nbaassts_names %in%
        c("Sam Cassell", "Tony Dobbins", "Amile Jefferson", "Tyler Lashbrook",          
          "Craig Luschenat", "D.J. MacLeay", "Ross McMains", "Matt Reynolds")
      ~ "Boston Celtics",
      nbaassts_names %in%
        c("Travis Bader","Deividas Dulkys", "Ryan Forehan-Kelly",
          "Dutch Gaitley", "Connor Griffin", "Jay Hernandez", "Steve Hetzel",
          "Juwan Howard","Corey Vinson")
      ~ "Brooklyn Nets",
      nbaassts_names %in%
        c("Kwadzo Ahelegbe",  "Charles Allen","Jordan Brink", "Rick Brunson",
          "Mark Bryant", "Maurice Cheeks", "Darren Erman", "Riccardo Fois",
          "Chris Jent", "Brendan O’Connor", "Peter Patton",
          "T.J. Saint", "Mark Tyndale")
      ~ "New York Knicks",
      nbaassts_names %in%
        c("Matt Brase", "T.J. Dileo", "Fabulous Flournoy", "Bryan Gates",
          "Rico Hines", "Michael Longabardi", "Doug West")
      ~ "Philadelphia 76ers",
      nbaassts_names %in%
        c("Mery Andrade", "Mike Batiste", "Vin Bhavnani", "Pat Delany",
          "Drew Jones", "Eric Khoury", "Jama Mahlalela",
          "Jim Sann", "Ivo Simović", "James Wade")
      ~ "Toronto Raptors",
      nbaassts_names %in%
        c("John Bryant", "Damian Cotter", "Dan Craig", "Henry Domercant",
          "Billy Schmidt", "Wes Unseld Jr.")
      ~ "Chicago Bulls",
      nbaassts_names %in%
        c("Johnnie Bryant", "Omar Cook", "Mike Gerrity",
          "Trevor Hendry", "Andrew Olson", "Nate Reinking",
          "Alex Sarama", "Jawad Williams")
      ~ "Cleveland Cavaliers",
      nbaassts_names %in%
        c("Jerome Allen", "Kevin Burleson", "Josh Estes", "Will Gent",
          "Jarrett Jack", "Sidney Lowe", "Jamelle McMillan",
          "Vitaly Potapenko", "Steve Scalzi", "Fred Vinson", "Luke Walton")
      ~ "Detroit Pistons",
      nbaassts_names %in%
        c("Maurice Baker", "Jenny Boucek", "Jim Boylen",
         "Johnny Carpenter", "Jannero Pargo", "Lloyd Pierce", "Isaac Yacob")
      ~ "Indiana Pacers",
      nbaassts_names %in%
        c("Vin Baker","Greg Buckner", "Pete Dominguez",
          "Darvin Ham", "Jack Herum", "Dave Joerger","Rex Kalamian",
          "Jason Love", "Spencer Rivers")
      ~ "Milwaukee Bucks",
      nbaassts_names %in%
        c("Bryan Bailey", "Mike Brey", "Steve Klei", "Antonio Lang",
          "Sanjay Lumpkin", "Ronald Nored", "Reggis Onwukamuche",
          "Ryan Schmidt", "Ashton Smith", "Ekpe Udoh", "Jeff Watkinson")
      ~ "Atlanta Hawks",
      nbaassts_names %in%
        c("D.J. Bakker", "Jermaine Bucknor", "Ryan Frazier", "Matt Hill",
          "Josh Longstaff", "Blaine Mueller", "Kyle Neptune",
          "Zach Peterson","Lamar Skeeter")
      ~ "Charlotte Hornets",
      nbaassts_names %in%
        c("Malik Allen", "Caron Butler", "Octavio De La Grana",
          "Wayne Ellington", "Eric Glass", "Chris Quinn")
      ~ "Miami Heat",
      nbaassts_names %in%
        c("Ameer Bahhur", "Jeremiah Boswell", "Bret Brielmaier",
          "Lionel Chalmers", "Randy Gregory", "Dylan Murphy", "Dale Osbourne",
          "Joe Prunty", "God Shammgod")
      ~ "Orlando Magic",
      nbaassts_names %in%
        c("Alexis Ajinca", "Adam Caporn", "James Fraschilla",
          "J.J. Outlaw", "T.J. Sorrentine", "Cody Toppert", "David Vanterpool")
      ~ "Washington Wizards",
      nbaassts_names %in%
        c("Ron Adams", "Seth Cooper", "Bruce Fraser",
          "Nicholas Kerr","Khalid Robinson", "Jacob Rubin",
          "Jerry Stackhouse","Terry Stotts", "Anthony Vereen", "Kris Weems")
      ~ "Golden State Warriors",
      nbaassts_names %in%
        c("Jeremy Castleberry", "Tim Dather", "Larry Drew",
          "Conor Dunleavy", "Shaun Fein", "Chris Holguin", "Dahntay Jones",
          "Jay Larranaga", "Brian Shaw", "Jeff Van Gundy", "Todd Wright")
      ~ "Los Angeles Clippers",
      nbaassts_names %in%
        c("Ty Abbott", "Bob Beyer", "Scott Brooks", "Lindsey Harding",
          "Beau Levesque", "Nate McMillan", "Greg St. Jean")
      ~ "Los Angeles Lakers",
      nbaassts_names %in%
        c("Chaisson Allen",
          "DeMarre Carroll",
          "Sean Dwyer",
          "John Little",
          "Jesse Mermuys",
          "Mike Muscala",
          "Brian Randle")
      ~ "Phoenix Suns",
      nbaassts_names %in%
        c("Leandro Barbosa", "Chris Darnell", "Bobby Jackson",
          "Mike Miller", "Dipesh Mistry", "Mike Woodson")
      ~ "Sacramento Kings",
      nbaassts_names %in%
        c("Josh Broghamer",
          "Phil Handy",
          "Eric Hughes",
          "Popeye Jones",
          "Mike Penberthy",
          "Jordan Sears",
          "Jay Triano",
          "Keith Veney",
          "Frank Vogel")
      ~ "Dallas Mavericks",
      nbaassts_names %in%
        c("Joseph Blair", "Josh Bostic", "Will Dunn", "Cam Hodges",
          "Royal Ivey", "Garrett Jackson", "Robbie Lemons","Ben Sullivan")
      ~ "Houston Rockets",
      nbaassts_names %in%
        c("Darnell Lazare", "Jason March", "Ryan Saunders", "Erik Schmidt")
      ~ "Memphis Grizzlies",
      nbaassts_names %in%
        c("James Borrego",
          "Corey Brewer",
          "Jarron Collins",
          "Brandon Demas",
          "Casey Hill",
          "Mike Hopkins",
          "Jodie Meeks",
          "Aaron Miles",
          "Greg Monroe")
      ~ "New Orleans Pelicans",
      nbaassts_names %in%
        c("Jimmy Baron",
          "Joshua Brannon",
          "Brett Brown",
          "Scott King",
          "Tim Martin",
          "Matt Nielsen",
          "Mike Noyes",
          "Corliss Williamson")
      ~ "San Antonio Spurs"
    )
  )

# remove clutter and head coaches ----

nbaasstscoach_names <- nbaasstscoach_names |>
  filter(nbaassts_names != "NBA Coaches Association",
         nbaassts_names != "Micah Nori",
         nbaassts_names != "Tiago Splitter",
         nbaassts_names != "Sean Sweeney")

# add gender variable ----

nbaasstscoach_names <- nbaasstscoach_names |>
  mutate(
    gender = if_else(nbaassts_names == "Jenny Boucek" | nbaassts_names == "Lindsey Harding",
                     "F", "M"))
    
nbacoachnames <- nbacoachnames |>
  mutate(gender = "M")
