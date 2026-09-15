
# ============================================================================
# Week 43: Introduction to R
#
# Today we will:
#   1. Use R as a calculator
#   2. Create objects and vectors
#   3. Create a small dataset
#   4. Select variables and observations
#   5. Create new variables
#   6. Calculate simple summaries
#   7. Save the dataset
# ============================================================================


# ---------------------------------------------------------------------------
# 1. Load packages
# ---------------------------------------------------------------------------

# Packages only need to be installed once.
# Run this line only if tidyverse has not already been installed:
#
# install.packages("tidyverse")

# Packages must be loaded when starting a new R session.

library(tidyverse)


# ---------------------------------------------------------------------------
# 2. Use R as a calculator
# ---------------------------------------------------------------------------

2 + 2

10 - 3

4 * 5

20 / 4

3^2

sqrt(16)


# ---------------------------------------------------------------------------
# 3. Create objects
# ---------------------------------------------------------------------------

age <- 35

weight_kg <- 80

height_m <- 1.75

age
weight_kg
height_m


# Calculate BMI

bmi <- weight_kg / height_m^2

bmi


# ---------------------------------------------------------------------------
# 4. Create vectors
# ---------------------------------------------------------------------------

age <- c(21, 35, 42, 28, 60)

sex <- c(
  "Female",
  "Male",
  "Female",
  "Female",
  "Male"
)

age
sex


# How many values are in the age vector?

length(age)

# what does length() do? 
?length

# Calculate the mean age

mean(age)


# Select the first age

age[1]


# Select ages greater than 30

age[age > 30]


# ---------------------------------------------------------------------------
# 5. Create a small public-health dataset
# ---------------------------------------------------------------------------

health_data <- tibble(
  id = 1:5,
  age = c(21, 35, 42, 28, 60),
  sex = c(
    "Female",
    "Male",
    "Female",
    "Female",
    "Male"
  ),
  weight_kg = c(60, 90, 67, 80, 85),
  height_m = c(1.65, 1.80, 1.70, 1.68, 1.75),
  active = c(
    "Yes",
    "No",
    "Yes",
    "No",
    "Yes"
  )
)


# Print the dataset

health_data


# Open the dataset in the data viewer

View(health_data)


# How many rows and columns?

dim(health_data)


# Look at the structure of the dataset

glimpse(health_data)


# ---------------------------------------------------------------------------
# 6. Create a new variable
# ---------------------------------------------------------------------------

health_data <- health_data |>
  mutate(
    bmi = weight_kg / height_m^2
  )


health_data


# ---------------------------------------------------------------------------
# 7. Select variables
# ---------------------------------------------------------------------------

health_data |>
  select(id, age, sex)


# Remove variables that are not needed

health_data |>
  select(id, age, sex, active, bmi)

#these do not store health_data with only the selected variables

# ---------------------------------------------------------------------------
# 8. Select observations
# ---------------------------------------------------------------------------

# Keep participants who are at least 30 years old

health_data |>
  filter(age >= 30)


# Keep physically active participants

health_data |>
  filter(active == "Yes")


# Use more than one condition

above30_active <- health_data |>
  filter(
    age >= 30,
    active == "Yes"
  )

above30_active

# ---------------------------------------------------------------------------
# 9. Calculate summaries
# ---------------------------------------------------------------------------

# Mean age

health_data |>
  summarise(
    mean_age = mean(age)
  )


# Mean BMI

health_data |>
  summarise(
    mean_bmi = mean(bmi)
  )


# Mean BMI by physical activity

health_data |>
  group_by(active) |>
  summarise(
    number = n(),
    mean_bmi = mean(bmi)
  )


# ---------------------------------------------------------------------------
# 10. Save the dataset
# ---------------------------------------------------------------------------

write_csv(
  health_data,
  "tiny_health_data.csv"
)


# ---------------------------------------------------------------------------
# 11. Where did we just store this?
# ---------------------------------------------------------------------------

getwd()


