# web for data https://www.ssb.no/statbank/table/12451/ 
#choose language, variables, and years --> go to "Save" --> choose xlsx file instead of json in "API" --> "Copy link to clipboard" --> paste below

url_data <- "https://data.ssb.no/api/pxwebapi/v2/tables/12451/data?lang=en&outputFormat=xlsx&valuecodes[ContentsCode]=Sykefraversprosent&valuecodes[Tid]=2020K1,2020K2,2020K3,2020K4,2021K1,2021K2,2021K3,2021K4,2022K1,2022K2,2022K3,2022K4,2023K1,2023K2,2023K3,2023K4,2024K1,2024K2,2024K3,2024K4,2025K1,2025K2,2025K3,2025K4,2026K1,2026K2&valuecodes[Region]=*&codelist[Region]=agg_KommGjeldende&valuecodes[Kjonn]=2,1&heading=Tid,Kjonn,ContentsCode&stub=Region"
response <- request(url_data) |> 
  req_perform()
sick_leave_raw <- response |> 
  resp_body_raw()  
glimpse(sick_leave_raw)
tmp <- tempfile(fileext = '.xlsx')
writeBin(sick_leave_raw, tmp)


## number of inhabitants in kommuner https://www.ssb.no/statbank/table/11342, choosing only for 2026 
url_inhab <-"https://data.ssb.no/api/pxwebapi/v2/tables/11342/data?lang=no&outputFormat=xlsx&valuecodes[Tid]=2026&valuecodes[Region]=*&codelist[Region]=agg_KommGjeldende&valuecodes[ContentsCode]=Folkemengde&heading=Tid,ContentsCode&stub=Region"
response <- request(url_inhab) |> 
  req_perform()
n_inhab_raw <- response |> 
  resp_body_raw()  
tmp2 <- tempfile(fileext = '.xlsx')
writeBin(n_inhab_raw, tmp2)

# load data from temporary files
sick_leave_data <- readxl::read_excel(tmp, na=".")
n_inhab_data <- readxl::read_excel(tmp2) # to be used when calculating average % sick leave

View(sick_leave_data)
names(sick_leave_data)
dim(sick_leave_data)
tail(sick_leave_data)

sick_leave_data <- sick_leave_data |> 
  slice(-c(360:393)) # When importing technical information have been put in last. we therefore remove row 360 to 393 

tail(sick_leave_data)
sick_leave_data <- sick_leave_data |>
  drop_na("2020K1")       # there has been several reforms we therefore only chooses communities having the same code from 2020-2024. therefore dropping those having missing in 2020
View(sick_leave_data)
dim(sick_leave_data)




# -----------------------------------------------------------------------------
# Identify the female and male columns
# -----------------------------------------------------------------------------

# Column 1 contains region.
#
# After that, the columns occur in pairs:
#
# Column 2 = Females, 2020K1
# Column 3 = Males, 2020K1
# Column 4 = Females, 2020K2
# Column 5 = Males, 2020K2
# and so on.

female_columns <- seq(
  from = 2,
  to = ncol(sick_leave_data),
  by = 2
)

male_columns <- seq(
  from = 3,
  to = ncol(sick_leave_data),
  by = 2
)


# Inspect the column positions.

female_columns

male_columns


# -----------------------------------------------------------------------------
# Extract the quarter names
# -----------------------------------------------------------------------------

# The quarter names are attached to the female columns.
#
# Examples:
# 2020K1, 2020K2, 2020K3, 2020K4

quarters <- names(sick_leave_data)[female_columns]


# Inspect the quarter names.

quarters


# -----------------------------------------------------------------------------
#  Remove the two additional header rows
# -----------------------------------------------------------------------------

# Row 1 contains "Females" and "Males".
# Row 2 contains the name of the measure.
#
# Actual observations begin in row 3.

observations <- sick_leave_data |>
  slice(
    -(1:2)
  )


# Inspect the observations.

View(observations)


# -----------------------------------------------------------------------------
# Store the region information
# -----------------------------------------------------------------------------

# The first column contains both a region code and a region name.

regions <- observations[[1]] #chooses first column double [[]] stores it as a vector and not a tibble. Makes it easier to use later


# Inspect the first few regions.

head(regions)


# -----------------------------------------------------------------------------
# Create the female dataset
# -----------------------------------------------------------------------------

# Select the female measurement columns.

female <- observations |>
  select(
    all_of(female_columns)
  )

head(female)

# Add region and sex.

female <- female |>
  mutate(
    region = regions,
    sex = "Females",
    .before = 1 #places the column in the beginning of the dataset
  )


# Inspect the female dataset.

head(female)


# Select the male measurement columns.

male <- observations |>
  select(
    all_of(male_columns)
  )

head(male)

# Give the columns their quarter names.

names(male) <- quarters
head(male)

# Add region and sex.

male <- male |>
  mutate(
    region = regions,
    sex = "Males",
    .before = 1
  )


# Inspect the male dataset.

head(male)


# -----------------------------------------------------------------------------
# Combine the female and male datasets
# -----------------------------------------------------------------------------

# Both datasets now have the same variables:
#
# region, sex, 2020K1, 2020K2, 2020K3, ...
#
# bind_rows() places the rows from one dataset below the rows
# from the other dataset.

sick_leave_wide <- bind_rows(
  female,
  male
)

head(sick_leave_wide)
tail(sick_leave_wide)

# attaching number of inhabitants
head(n_inhab_data)
names(n_inhab_data) <- c("region", "n_inhab")
n_inhab_data <- n_inhab_data |>
  mutate(region = recode(region, "1841 Fauske - Fuossko" = "1841 Fauske - Fuosko")) # inconsistent naming from SSB!
head(n_inhab_data)

sick_leave_wide <- sick_leave_wide |>
  left_join(n_inhab_data, by = "region") 


# -----------------------------------------------------------------------------
# separating code and name for kommune. going to use code to create the variable county.
# -----------------------------------------------------------------------------


View(sick_leave_wide)
str_sub(sick_leave_wide[[1,1]], 1, 4)
test <- str_sub(sick_leave_wide$region, 1, 4)
test

sick_leave_wide <- sick_leave_wide |>
  mutate(
    region_code = as.numeric(str_sub(region, 1, 4)),
    region_name = str_sub(region, 6)
  ) |>
  select(
    region_code,
    region_name,
    n_inhab,
    everything(),
    -region
  )

head(sick_leave_wide)
source("scripts/kommune_to_county.R") 
head(sick_leave_wide)

sick_leave_long <- sick_leave_wide |>
  pivot_longer(
    cols = -c(        #I am choosing columns not to be turned into long
      region_code,
      region_name,
      n_inhab,
      county,
      sex
    ),
    names_to = "quarter",
    values_to = "sick_leave_percent"
  )
head(sick_leave_long)
sick_leave_long <- sick_leave_long |>    
  mutate(sick_leave_percent = as.numeric(sick_leave_percent), # need to change into numeric
         n_inhab = as.numeric(n_inhab)) # need to change into numeric
head(sick_leave_long) #check if numeric


# creating a population weighted mean sick leave pr county using number of inhabitants in 2026 as weight. The correct would be to use number of inhabitants in the workforce. 
# Here a kommune with an old population would be given too much weight
sick_leave_counties <- sick_leave_long |>
  group_by(county, sex, quarter) |>
  summarise(
    sick_leave_percent = weighted.mean(sick_leave_percent, w = n_inhab),  
    .groups = "drop"
  )
View(sick_leave_counties)

# pivot sick_leave_counties into wide
sick_leave_counties <- sick_leave_counties |>
  pivot_wider(    
    names_from = quarter,
    values_from = sick_leave_percent
  )

sick_leave_counties
#store it into data folder
write.csv(sick_leave_counties, "data/sick_leave_counties.csv")
remove(female, male, n_inhab_data, observations, response, sick_leave_data, sick_leave_long, sick_leave_wide)
