# Packages needed --------------------------------------------------------------

library(fpp3)
library(ggplot2)

# Exploring Graphs -------------------------------------------------------------

# Time plot
melsyd_economy <- ansett |>
  filter(Airports == "MEL-SYD", Class == "Economy") |>
  mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

#  The autoplot() function will automatically produce an appropriate plot of 
# whatever you pass to it in the fisrt argument.

# Another example using the A10 dataset saved earlier

# Filtering series
PBS |> 
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |> 
  summarise(TotalCost = sum(Cost)) |> 
  mutate(Cost = TotalCost / 1e6) -> A10

# Time plot
autoplot(A10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

# Time plot including data points
A10 |> autoplot(Cost) +
  geom_point() +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")


