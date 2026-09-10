# ============================================================================
# Week 43 — 00_setup_week43.R  (TEACHER SOLUTION)
# Purpose:
#   - Install & load packages
#   - Demonstrate basic R operations (calculator, vectors, matrix, data.frame)
#

# ============================================================================

install.packages(c('tidyverse', 'ggplot2', 'nhanesA', 'janitor', 'lubridate', 'here', 'skimr', 'broom', 'gt'))

# ---- 2. Load packages (every session) --------------------------------------
library(tidyverse)
library(ggplot2)
library(nhanesA)
library(janitor)
# library(lubridate)
# library(here)
# library(skimr)
# library(broom)
# library(gt)



# ---- 4. Use R as a calculator ----------------------------------------------
2 + 2
sqrt(16)
log(10)
(3^2 + 4^2)^(1/2)  

# ---- 5. Create vectors (numeric and text) ----------------------------------
nums <- c(2, 4, 6, 8, 10)
chars <- c('alpha', 'beta', 'gamma')

nums
chars

# ---- 6. Manipulate vectors --------------------------------------------------
nums * 2
nums[nums > 5]
mean(nums)

chars[1:2]
paste(chars, toupper(chars), sep=' : ')

# ---- 7. Create a data frame ----------------------------------

# Create a data frame from vectors
id <- 1:5
age <- c(21, 35, 42, 28, 60)
sex <- c('Female', 'Male', 'Female', 'Female', 'Male')

df_base <- data.frame(id = id, age = age, sex = sex)
df_base

# Tidyverse tibble version
df_tibble <- tibble(id = id, age = age, sex = sex)
df_tibble
 # ---- 8. Manipulate columns and rows ----------------------------------------
# Base R
# Add a column
df_base$bmi <- c(22.1, 27.4, 24.0, 29.2, 26.8)

# Filter rows
subset(df_base, age >= 30)

# Select columns
df_base[, c('id', 'sex')]

# Tidyverse (recommended)
df_tibble <- df_tibble |>
  mutate(bmi = c(22.1, 27.4, 24.0, 29.2, 26.8)) 
df_tibble |>
  filter(age >= 30) |>
  select(id, sex, age, bmi)

# ---- 9. Save a simple object for reuse -------------------------------------
# Store a small example dataset so students see how saving works
write_csv(df_tibble, 'tiny_example.csv')

# ---- 10. But where did we store it?  -------------------------------------
getwd()  # Show current working directory)

