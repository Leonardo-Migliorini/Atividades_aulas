# Packages needed --------------------------------------------------------------

library(fpp3)

# Built in time series from the fpp3 package -----------------------------------

global_economy
tourism
PBS

# Making tsiblle objects -------------------------------------------------------

# A tsibble allows storage and maniputalion of multiple time series in R.

# It contains:
# - An index (time information about the observation)
# - Measured variables (numbers of interest)
# - Key variables (optional unique identifiers for each series)

# It works with tidyverse functions

# Creating a simple tsibble
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

# Converting a tibble to a tsibble 
tibble <- tibble(
  Year = 2015:2019,
  Observations = c(123, 39, 78, 52, 110),
  index = Year
)

tsibble <- tibble |> 
  as_tsibble(index = Year)

# Converting a str variable to a tsibble index

table1 <- tibble(
  Month = c("2019 Jan", "2019 Feb", "2019 Mar", "2019 Apr", "2019 May"),
  Observation = c(50, 23, 34, 30, 25)
)

table1 |>
  mutate(Month = yearmonth(Month)) |>
  as_tsibble(index = Month)

# We can use dplyr fucntion to filter our data of a tsibble object.
# Example:

PBS |> 
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |> 
  summarise(TotalCost = sum(Cost)) |> 
  mutate(Cost = TotalCost / 1e6) -> A10

# Converting a csv file to a tsibble object

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison |> mutate(
  Date = yearquarter(Date)
  ) |> 
  as_tsibble(
    index = Date,
    key = c(State, Gender, Legal, Indigenous)
  ) -> prison
