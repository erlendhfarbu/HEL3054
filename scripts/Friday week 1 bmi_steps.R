# ==========================================================
# Week 43: BMI and Number of Steps
# Research question:
# Is BMI associated with the number of steps per day?
# ==========================================================

# Load packages
library(tidyverse)

# ----------------------------------------------------------
# 1. Read the data
# ----------------------------------------------------------

steps_df <- read.csv("data/bmi_steps.csv")

# Look at the first rows
head(steps_df)

# Check the variables
glimpse(steps_df)

# ----------------------------------------------------------
# 2. Simple descriptive statistics
# ----------------------------------------------------------

# Mean BMI and mean number of steps
steps_df %>%
  summarise(
    mean_bmi = mean(bmi, na.rm = TRUE),
    mean_steps = mean(steps, na.rm = TRUE)
  )

# Summary by sex
steps_df %>%
  group_by(sex) %>%
  summarise(
    mean_bmi = mean(bmi, na.rm = TRUE),
    mean_steps = mean(steps, na.rm = TRUE)
  )


# ----------------------------------------------------------
# 3. Run a simple linear regression
# ----------------------------------------------------------

model1 <- lm(bmi ~ steps, data = steps_df)

summary(model1)

model2 <- lm(bmi ~ steps + sex, data = steps_df)

summary(model2)

model3 <- lm(bmi ~ steps + sex + steps*sex, data = steps_df)

summary(model3)

# -------------------------------
# Residuals if interesting
# ------------------------
# steps_df$residuals <- residuals(model3)
# steps_df$fitted <- fitted(model3)
# 
# ggplot(steps_df, aes(x = fitted, y = residuals)) +
#   geom_point(alpha = 0.5) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   labs(
#     title = "Residuals vs fitted values",
#     x = "Fitted BMI",
#     y = "Residuals"
#   ) +
#   theme_minimal()
# 
# ggplot(steps_df, aes(x = residuals)) +
#   geom_histogram(bins = 30) +
#   labs(
#     title = "Distribution of residuals",
#     x = "Residuals",
#     y = "Count"
#   ) +
#   theme_minimal()
# ----------------------------------------------------------
# 4. Create a scatterplot
# ----------------------------------------------------------

ggplot(steps_df, aes(x = steps, y = bmi)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "BMI and daily steps",
    x = "Number of steps",
    y = "BMI"
  ) +
  theme_minimal()



# ----------------------------------------------------------
# 6. Repeat the plot by sex
# ----------------------------------------------------------
ggplot(steps_df, aes(x = steps, y = bmi)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ sex) +
  labs(
    title = "BMI and daily steps by sex",
    x = "Number of steps",
    y = "BMI"
  ) +
  theme_minimal()

# Male participants
ggplot(filter(steps_df, sex == "male"),
       aes(x = steps, y = bmi)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "BMI and daily steps (Male)",
    x = "Number of steps",
    y = "BMI"
  ) +
  theme_minimal()

# Female participants
ggplot(filter(steps_df, sex == "female"),
       aes(x = steps, y = bmi)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "BMI and daily steps (Female)",
    x = "Number of steps",
    y = "BMI"
  ) +
  theme_minimal()
