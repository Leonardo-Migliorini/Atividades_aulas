# Packages needed --------------------------------------------------------------

library(fpp3)
library(dplyr)

# Built in time series from the fpp3 package -----------------------------------

global_economy
tourism

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

# Convarting a tibble to a tsibble 
tibble <- tibble(
  Year = 2015:2019,
  Observations = c(123, 39, 78, 52, 110),
  index = Year
)

tsibble <- tibble |> 
  as_tsibble(index = Year)







