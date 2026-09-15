# ============================================================================
# Week 43: Blood pressure and mental health
#
# Research question:
# Is there an association between blood pressure and mental health?
# ============================================================================


# ---------------------------------------------------------------------------
# 1. Load packages
# ---------------------------------------------------------------------------

source("scripts/load_packages.R")


# ---------------------------------------------------------------------------
# 2. Read the data
# ---------------------------------------------------------------------------
dpq  <- nhanes("DPQ_J")

demo <- read_csv("data/demo.csv")
bpx  <- read_csv("data/blood_pressure.csv")
dpq  <- read_csv("data/depression.csv")


# Look at the datasets

View(demo)
View(bpx)
View(dpq)

names(demo)
names(bpx)
names(dpq)


# ---------------------------------------------------------------------------
# 3. Select variables
# ---------------------------------------------------------------------------

demo_small <- demo |>
  select(
    SEQN,
    RIDAGEYR,
    RIAGENDR
  )

bpx_small <- bpx |>
  select(
    SEQN,
    BPXSY1,
    BPXSY2,
    BPXSY3,
    BPXSY4
  )

dpq_small <- dpq |>
  select(
    SEQN,
    DPQ010,
    DPQ020,
    DPQ030,
    DPQ040,
    DPQ050,
    DPQ060,
    DPQ070,
    DPQ080,
    DPQ090
  )


# ---------------------------------------------------------------------------
# 4. Join the datasets
# ---------------------------------------------------------------------------

nhanes_data <- demo_small |>
  left_join(bpx_small, by = "SEQN") |>
  left_join(dpq_small, by = "SEQN")


# ---------------------------------------------------------------------------
# 5. Rename variables
# ---------------------------------------------------------------------------

nhanes_data <- nhanes_data |>
  rename(
    id = SEQN,
    age = RIDAGEYR,
    sex = RIAGENDR
  )


# ---------------------------------------------------------------------------
# 6. Keep adults
# ---------------------------------------------------------------------------

nhanes_data <- nhanes_data |>
  filter(age >= 18)


# ---------------------------------------------------------------------------
# 7. Create new variables
# ---------------------------------------------------------------------------

nhanes_data <- nhanes_data |>
  mutate(
    
    # Mean systolic blood pressure. Correct to use the first?
    
    sbp = rowMeans(
      cbind(BPXSY1, BPXSY2, BPXSY3, BPXSY4),
      na.rm = TRUE
    ),
    
    # Replace NHANES codes 7 and 9 with missing values
    
    DPQ010 = ifelse(DPQ010 %in% c(7, 9), NA, DPQ010),
    DPQ020 = ifelse(DPQ020 %in% c(7, 9), NA, DPQ020),
    DPQ030 = ifelse(DPQ030 %in% c(7, 9), NA, DPQ030),
    DPQ040 = ifelse(DPQ040 %in% c(7, 9), NA, DPQ040),
    DPQ050 = ifelse(DPQ050 %in% c(7, 9), NA, DPQ050),
    DPQ060 = ifelse(DPQ060 %in% c(7, 9), NA, DPQ060),
    DPQ070 = ifelse(DPQ070 %in% c(7, 9), NA, DPQ070),
    DPQ080 = ifelse(DPQ080 %in% c(7, 9), NA, DPQ080),
    DPQ090 = ifelse(DPQ090 %in% c(7, 9), NA, DPQ090),
    
    # Calculate PHQ-9 score
    
    phq9 =
      DPQ010 +
      DPQ020 +
      DPQ030 +
      DPQ040 +
      DPQ050 +
      DPQ060 +
      DPQ070 +
      DPQ080 +
      DPQ090
  )


# ---------------------------------------------------------------------------
# 8. Explore the data
# ---------------------------------------------------------------------------

dim(nhanes_data)

summary(
  nhanes_data |>
    select(age, sbp, phq9)
)

# Missing values

nhanes_data |>
  summarise(
    missing_age = sum(is.na(age)),
    missing_sbp = sum(is.na(sbp)),
    missing_phq9 = sum(is.na(phq9))
  )


# ---------------------------------------------------------------------------
# 9. Descriptive statistics
# ---------------------------------------------------------------------------

# Mean blood pressure

nhanes_data |>
  summarise(
    mean_sbp = mean(sbp, na.rm = TRUE)
  )

# Mean mental health score

nhanes_data |>
  summarise(
    mean_phq9 = mean(phq9, na.rm = TRUE)
  )

# Means by sex

nhanes_data |>
  group_by(sex) |>
  summarise(
    mean_sbp = mean(sbp, na.rm = TRUE),
    mean_phq9 = mean(phq9, na.rm = TRUE),
    .groups = "drop"
  )


# ---------------------------------------------------------------------------
# 10. Create figures
# ---------------------------------------------------------------------------

# Scatterplot

ggplot(nhanes_data,
       aes(x = sbp, y = phq9)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Mental health score and blood pressure",
    x = "Systolic blood pressure",
    y = "PHQ-9 score"
  ) +
  theme_minimal()


# Scatterplot with regression line

ggplot(nhanes_data,
       aes(x = sbp, y = phq9)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Mental health score and blood pressure",
    x = "Systolic blood pressure",
    y = "PHQ-9 score"
  ) +
  theme_minimal()


# Separate lines for males and females

ggplot(nhanes_data,
       aes(x = sbp,
           y = phq9,
           colour = factor(sex))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Mental health score and blood pressure by sex",
    x = "Systolic blood pressure",
    y = "PHQ-9 score",
    colour = "Sex"
  ) +
  theme_minimal()


# ---------------------------------------------------------------------------
# 11. Linear regression
# ---------------------------------------------------------------------------

# Crude model

model1 <- lm(phq9 ~ sbp,
             data = nhanes_data)

summary(model1)


# Adjusted for age

model2 <- lm(phq9 ~ sbp + age,
             data = nhanes_data)

summary(model2)


# Adjusted for age and sex

model3 <- lm(phq9 ~ sbp + age + sex,
             data = nhanes_data)

summary(model3)

model4 <- lm(phq9 ~ sbp + age + sex + sbp*sex,
             data = nhanes_data)

summary(model4)



