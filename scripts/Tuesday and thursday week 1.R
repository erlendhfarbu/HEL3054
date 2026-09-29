

# ============================================================================
# Week 43: BMI and physical activity
#
# Research question:
# Is there an association between recreational physical activity and BMI?
#
# 
# ============================================================================

#Start an R-project
# 
# ---------------------------------------------------------------------------
# 1. Load packages. need to install nhanes, and add nhanes to the file "load_packages.R"
# ---------------------------------------------------------------------------
source("scripts/load_packages.R")


# ---------------------------------------------------------------------------
# 2. Read the data. Make sure you have downloaded the data
# ---------------------------------------------------------------------------

demo_j<- read_csv("data/demo_j.csv") # from 2017-2018 
bmx_j <- read_csv("data/body_measures_j.csv") # from 2017-2018  
paq_j <- read_csv("data/physical_activity_j.csv") # from 2017-2018 

# Look at the three datasets

demo_j
bmx_j
paq_j

View(demo_j)
View(bmx_j)
View(paq_j)

names(demo_j)
names(bmx_j)
names(paq_j)

dim(demo_j)
dim(bmx_j)
dim(paq_j)

# ---------------------------------------------------------------------------
# 3. Select the variables we need
# ---------------------------------------------------------------------------

# to find information about the variables, use browseNHANES(), or nhanesCodebook("BMX_J")
demo_small <- demo_j|>
  select(
    SEQN,       # Participant ID
    RIDAGEYR,   # Age
    RIAGENDR    # Sex
  )

bmx_small <- bmx_j|>
  select(
    SEQN,       # Participant ID
    BMXWT,      # Weight in kilograms
    BMXHT,      # Height in centimetres
    BMXBMI      # BMI calculated by NHANES
  )

paq_small <- paq_j|>
  select(
    SEQN,       # Participant ID
    PAQ650,     # Self-reported vigorous recreational activity
    PAQ665,     # Self-reported moderate recreational activity
    PAD680      # Self-reported minutes spent sitting per day
  )


# ---------------------------------------------------------------------------
# 4. Join the datasets
# ---------------------------------------------------------------------------

nhanes_data <- demo_small |>
  left_join(bmx_small, by = "SEQN") |>
  left_join(paq_small, by = "SEQN")


# Check the result

dim(nhanes_data)
View(nhanes_data)


# ---------------------------------------------------------------------------
# 5. Rename the variables
# ---------------------------------------------------------------------------

nhanes_data <- nhanes_data |>
  rename(
    id = SEQN,
    age = RIDAGEYR,
    sex = RIAGENDR,
    weight_kg = BMXWT,
    height_cm = BMXHT,
    bmi_nhanes = BMXBMI,
    vigorous_activity = PAQ650,
    moderate_activity = PAQ665,
    sitting_minutes = PAD680
  )


# ---------------------------------------------------------------------------
# 6. Keep adults
# ---------------------------------------------------------------------------

nhanes_data <- nhanes_data |>
  filter(age >= 18)



# ---------------------------------------------------------------------------
# 7. Create variables
# ---------------------------------------------------------------------------

nhanes_data <- nhanes_data |>
  mutate(
    # Convert height from centimetres to metres
    
    height_m = height_cm / 100,
    
    # Calculate BMI
    
    bmi = weight_kg / height_m^2,
    
    # Create a simple physical activity variable
    activity = case_when(
      vigorous_activity == "Yes" | moderate_activity == "Yes" ~ 1,
      vigorous_activity == "No" & moderate_activity == "No" ~ 2,
      TRUE ~ NA_real_)
  )

nhanes_data <- nhanes_data |>
  mutate(
    activity = factor(
      activity,
      levels = c(1, 2),
      labels = c("Active", "Inactive")
    )
  )

# Look at the new variables

View(nhanes_data)

# ============================================================================
# 8. Create BMI categories
# ============================================================================

nhanes_data <- nhanes_data |>
  mutate(
    bmi_category = case_when(
      bmi < 18.5             ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal weight",
      bmi >= 25   & bmi < 30 ~ "Overweight",
      bmi >= 30              ~ "Obesity",
      TRUE                   ~ NA_character_  # Maybe show what happens when this is not included? Most programs store missing as some high number.
    )
  )


# Check the new variable

table(nhanes_data$bmi_category, useNA = "ifany")

# Bar chart of BMI categories
ggplot(nhanes_data, aes(x = bmi_category)) +
  geom_bar()

ggplot(nhanes_data, aes(x = bmi_category)) +
  geom_bar() +
  labs(
    title = "Body mass index (BMI)",
    x = "BMI in categories",
    y = "Number of participants"
  ) +
  theme_minimal()

#Order the variable
nhanes_data <- nhanes_data |>
  mutate(
    bmi_category = factor(
      bmi_category,
      levels = c("Underweight", "Normal weight", "Overweight", "Obesity")
    )
  )


# ---------------------------------------------------------------------------
# 9. Explore the data
# ---------------------------------------------------------------------------

# Number of rows and columns

dim(nhanes_data)


# Number of participants

n_distinct(nhanes_data$id)


# Summary of selected variables

summary(
  nhanes_data |>
    select(age, weight_kg, height_cm, bmi, sitting_minutes)
)

#Show what happens when using summary on categorical/string variables


# Count missing values

nhanes_data |>
  summarise(
    missing_age = sum(is.na(age)),
    missing_sex = sum(is.na(sex)),
    missing_bmi = sum(is.na(bmi)),
    missing_activity = sum(is.na(activity))
  )

# There are other ways to inspect missing values. See https://www.gerkovink.com/miceVignettes/Missingness_inspection/Missingness_inspection.html 


# Drop missing 

nhanes_data <- nhanes_data |>
  drop_na(age, sex, bmi, activity)
n_distinct(nhanes_data$id)


# A more dificult way to check the range of the variables

nhanes_data |>
  summarise(
    minimum_age = min(age, na.rm = TRUE),
    maximum_age = max(age, na.rm = TRUE),
    minimum_bmi = min(bmi, na.rm = TRUE),
    maximum_bmi = max(bmi, na.rm = TRUE),
    minimum_sitting_minutes = min(sitting_minutes, na.rm = TRUE),
    maximum_sitting_minutes = max(sitting_minutes, na.rm = TRUE)
  )

# Histogram of BMI
ggplot(nhanes_data, aes(x = bmi)) +
  geom_histogram()

ggplot(nhanes_data, aes(x = bmi)) +
  geom_histogram() +
  labs(
    title = "Distribution of BMI",
    x = "BMI",
    y = "Number of participants"
  ) +
  theme_minimal()

ggplot(nhanes_data, aes(x = bmi)) +
  geom_histogram(
    binwidth = 0.5,
    color = "white"
  ) +
  labs(
    title = "Distribution of BMI",
    x = "BMI",
    y = "Number of participants"
  ) +
  theme_minimal()

#Histogram of minutes spent sitting per day
ggplot(nhanes_data, aes(x = sitting_minutes)) +
  geom_histogram() +   
  labs(
    title = "Distribution of Minutes Spent Sitting Per Day",
    x = "Minutes Spent Sitting Per Day",
    y = "Number of participants"
  ) +
  theme_minimal()

# What is the possible minutes spent sitting per day? The variable is coded as 77777 for "Refused" and 99999 for "Don't know". We can recode these values to NA.
nhanes_data <- nhanes_data |>
  mutate(
    sitting_minutes = case_when(
      sitting_minutes == 7777 ~ NA_real_,
      sitting_minutes == 9999 ~ NA_real_,
      TRUE ~ sitting_minutes
    )
  )
nhanes_data <- nhanes_data |>
  drop_na(sitting_minutes)
summary(
  nhanes_data |>
    select(age, weight_kg, height_cm, bmi, sitting_minutes)
)
n_distinct(nhanes_data$id)
summary(
  nhanes_data |>
    select(sitting_minutes)
)

# What is the possible total number of minutes a person could sit in a day? 24 hours * 60 minutes = 1440 minutes. 


# Compare our calculated BMI with the NHANES BMI variable
nhanes_data |>
  select(bmi, bmi_nhanes) |>
  head(10)

sum(nhanes_data$bmi - nhanes_data$bmi_nhanes) # should be 0
sum(nhanes_data$bmi== nhanes_data$bmi_nhanes) # returns the number of TRUE values, should be equal to the number of rows in the dataset.
table(nhanes_data$bmi == nhanes_data$bmi_nhanes, useNA = "always")
sum(round(nhanes_data$bmi, digits = 1) - nhanes_data$bmi_nhanes) # should have rounded to same number.
table(round(nhanes_data$bmi, digits = 1) == nhanes_data$bmi_nhanes, useNA = "always")


# ---------------------------------------------------------------------------
# 10. Descriptives
# ---------------------------------------------------------------------------

# Number of participants by sex

nhanes_data |>
  count(sex)


# Number of active and inactive participants

nhanes_data |>
  count(activity)


# BMI categories by sex
table(nhanes_data$sex, nhanes_data$bmi_category)

nhanes_data |>
  count(sex, bmi_category) # to use if you want to use dplyr instead of base R, and are including it in tidy-pipelines. 


# BMI categories by physical activity
table(nhanes_data$activity, nhanes_data$bmi_category)

nhanes_data |>
  count(activity, bmi_category) # to use if you want to use dplyr instead of base R, and are including it in tidy-pipelines 

#example of creating a crosstable with counts and percentages using dplyr and tidyr. This is commented out because it is not necessary for the analysis, but it is an example of how to create a crosstable.
# 
# activity_bmi_summary <- nhanes_data |>
#   count(activity, bmi_category) |>
#   group_by(activity) |>
#   mutate(
#     percentage = round(n / sum(n) * 100, 1)
#   ) |>
#   ungroup()
# 
# activity_bmi_crosstable <- activity_bmi_summary |>
#   mutate(
#     result = paste0(n, " (", percentage, "%)")
#   ) |>
#   select(activity, bmi_category, result) |>
#   pivot_wider(
#     names_from = bmi_category,
#     values_from = result
#   )
#
# activity_bmi_crosstable

# Mean BMI by physical activity

nhanes_data |>
  group_by(activity) |>
  summarise(
    number = sum(!is.na(bmi)),
    mean_bmi = mean(bmi, na.rm = TRUE),
    median_bmi = median(bmi, na.rm = TRUE)
  )

# Box plot of BMI by physical activity

ggplot(nhanes_data, aes(x = activity, y = bmi)) +
  geom_boxplot() +
  labs(
    title = "BMI by recreational physical activity",
    x = "Physical activity",
    y = "BMI"
  ) +
  theme_minimal()



# Mean BMI by physical activity and sex

nhanes_data |>
  group_by(sex, activity) |>
  summarise(
    number = sum(!is.na(bmi)),
    mean_bmi = mean(bmi, na.rm = TRUE),
    median_bmi = median(bmi, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# 11. Linear regression
# ---------------------------------------------------------------------------

# Fit a linear regression model

model1 <- lm(bmi ~ activity, data = nhanes_data)

# View the results

summary(model1)

#Adjust for sex and age
model2 <- lm(bmi ~ activity + sex + age, data = nhanes_data)

# View the results

summary(model2)
