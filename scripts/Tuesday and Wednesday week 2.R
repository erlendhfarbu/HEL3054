# ============================================================================
# EV adoption and air quality in Tromsø (Hansjordnesbukta, station 203)

# ============================================================================

source("scripts/load_packages.R")


# =============================================================================
# PART 1 — Vehicles: compute EV proportion per year
# =============================================================================

# The Excel is a wide SSB export with multi-row headers.
# We read without column names, locate the Tromsø row, then reshape.
car_file <- 'Number of cars using petrol gas or electic.xlsx'
raw_cars <- read_excel(car_file) #need package readxl

View(raw_cars)
idx <- seq(from=2, to=104, by=6) #starting in second column, every 6th column is a year, and a total of 17 years (2008-2025) in the data. 
idx
raw_cars[5, idx] # Bensin
raw_cars[5, idx+1] # Diesel

cars <- data.frame(year = 2008:2025,
                   bensin = as.numeric(raw_cars[5, idx]), #number of cars are in row 5, columns idx (2, 8, 14, ...), 
                   diesel = as.numeric(raw_cars[5, idx + 1]), # diesel in idx+1 = (3, 9, 15....)
                   parafin = as.numeric(raw_cars[5, idx + 2]),
                   gass = as.numeric(raw_cars[5, idx + 3]),
                   el = as.numeric(raw_cars[5, idx + 4]),
                   other = as.numeric(raw_cars[5, idx + 5])
)
                   
glimpse(cars)
cars <- cars |>
  mutate(
    total_cars = bensin + diesel + parafin +
      gass + el + other,
    
    proportion_electric = el / total_cars
  )
glimpse(cars)



cars <- cars %>% 
  filter(year >= 2015) # filter to years 2015-2025 for comparison with air quality data)

plot_prop_el <- ggplot(cars, aes(x = year, y = proportion_electric)) +
  geom_line() +
  labs(title = "Proportion of Electric Vehicles in Troms (2008-2025)",
       y = "Proportion of EVs (%)") +
  scale_x_continuous(breaks = seq(2008, 2025, by = 2)) +
  theme_minimal() 

  
plot_prop_el

# =============================================================================
# PART 2 — Air quality: download measurements for station 203 and aggregate
# =============================================================================

# Station metadata (given in assignment)
station_id <- 203
station_name <- 'Hansjordnesbukta'


# --- API helper --------------------------------------------------------------
# IMPORTANT: The exact endpoint paths are defined in the Miljødirektoratet Swagger.


base_url <- 'https://api-luftmalinger.miljodirektoratet.no/public/agg/2/2015-01-01/2025-12-30/Hansjordnesbukta?components=NO2&showinvalid=false'
#base_url <- 'https://api-luftmalinger.miljodirektoratet.no/public/agg/2/2010-01-01/2010-12-30/Danmarks%20plass?components=NO2&showinvalid=false'
response <- request(base_url) |> 
  req_perform()


no2_json <- response |> 
  resp_body_json()  
  
glimpse(no2_json)
str(no2_json, max.level = 3)
glimpse(no2_json[[1]])

glimpse(no2_json[[1]]$values)

no2_list <- no2_json[[1]]$values |>
  map(
    \(measurement) {
      tibble(
        date_time = measurement$dateTime,
        no2 = measurement$value
      )
    }
  )

class(no2_list)

df_no2 <- no2_list |>
  list_rbind()

df_no2
# date is chr --> convert to date
df_no2$date <- as.Date(df_no2$date_time) #could also use mutate
df_no2

#PM2.5
base_url <- 'https://api-luftmalinger.miljodirektoratet.no/public/agg/2/2015-01-01/2025-12-30/Hansjordnesbukta?components=PM2.5&showinvalid=false'
#base_url <- 'https://api-luftmalinger.miljodirektoratet.no/public/agg/2/2010-01-01/2010-12-30/Danmarks%20plass?components=no2&showinvalid=false'
response <- request(base_url) |> 
  req_perform()


pm25_json <- response |> 
  resp_body_json()  




pm25_list <- pm25_json[[1]]$values |>
  map(
    \(measurement) {
      tibble(
        date_time = measurement$dateTime,
        pm25 = measurement$value
      )
    }
  )


df_pm25 <- pm25_list |>
  list_rbind()

df_pm25$date <- as.Date(df_pm25$date_time) #could also use mutate
df_pm25

#add pm2.5 to no2 and call it air_quality
air_quality <- df_no2 %>%
  left_join(df_pm25, by = "date") %>%
  select(date, no2, pm25)
air_quality

write_csv(
  air_quality,
  "data/air_quality_hansjordnesbukta.csv"
)


air_quality <- air_quality %>% 
  filter(date <= as.Date("2024-12-31"))
#tail(air_quality)


# =============================================================================
# PART 3 — Visualization
# =============================================================================


pm25 <- ggplot(air_quality, aes(x = date, y = pm25)) +
  geom_line() +
  labs(
    title = "PM2.5 over time",
    x = "Date",
    y = expression(PM[2.5] ~ "(" * mu * "g/m"^3 * ")")
  ) +
  theme_minimal()

no2 <- ggplot(air_quality, aes(x = date, y = no2)) +
  geom_line() +
  labs(
    title = "NO2 over time",
    x = "Date",
    y = expression(no2 ~ "(" * mu * "g/m"^3 * ")")
  ) +
  theme_minimal()

#pm25
#no2

plot_prop_el / pm25 / no2


# =============================================================================
# PART 4 — Yearly aggregation and visualization
# =============================================================================

#Yearly average 
# extract year from dateTime
air_quality <- air_quality %>% mutate(year = year(date))
#use mean or median?
hist(air_quality$no2, breaks = 130, main = "Distribution of no2", xlab = "no2 (mikrogram/m3)")
hist(air_quality$pm25, breaks = 130, main = "Distribution of PM2.5", xlab = "PM2.5 (mikrogram/m3)")


#Yearly median and mean appended to air_quality 

# Show for no2, students  can try to solve for pm2.5 and append to air_quality_year. Possible to show only mean/median/threshold depending on time

air_quality_year <- air_quality |>
  group_by(year) |>
  summarise(no2_median = median(no2, na.rm = TRUE))

glimpse(air_quality_year)
#for pm2.5 and append to air_quality_year


air_quality_pm25 <- air_quality %>%
  group_by(year) %>%
  summarise(pm25_median = median(pm25, na.rm = TRUE))

air_quality_year <- air_quality_year %>%
  left_join(air_quality_pm25, by = "year")

#repeat for yearly mean
air_quality_no2 <- air_quality %>%
  group_by(year) %>%
  summarise(no2_mean = mean(no2, na.rm = TRUE))
air_quality_year <- air_quality_year %>%
  left_join(air_quality_no2, by = "year")

air_quality_pm25 <- air_quality %>%
  group_by(year) %>%
  summarise(pm25_mean = mean(pm25, na.rm = TRUE))
air_quality_year <- air_quality_year %>%
  left_join(air_quality_pm25, by = "year")
air_quality_year

#number of days over a certain threshold (e.g., % of days with PM2.5 > 15 µg/m³, WHO guidelines) in a year
air_quality_pm25_threshold <- air_quality %>%
  group_by(year) %>%
  summarise(pm25_over_threshold = sum(pm25 > 15, na.rm = TRUE))
air_quality_year <- air_quality_year %>%
  left_join(air_quality_pm25_threshold, by = "year")
air_quality_year

#number over a certain threshold for no2 (e.g., % of days with no2 > 25 µg/m³) in a year
air_quality_no2_over_threshold <- air_quality %>% 
  group_by(year) %>%
  summarise(no2_over_threshold = sum(no2 > 25, na.rm = TRUE))
air_quality_year <- air_quality_year %>%
  left_join(air_quality_no2_over_threshold, by = "year")
air_quality_year

#add prop_el to air_quality_year
air_quality_year <- air_quality_year %>%
  left_join(cars %>% select(year, proportion_electric), by = "year")
air_quality_year

#visualizartion
#create one line plot for each variable (no2_mean, pm25_mean, prop_el) over time (year) 
# add them together 

no2_plot_mean <- ggplot(air_quality_year, aes(x = year, y = no2_mean)) +
  geom_line() +
  labs(title = "Yearly mean NO2", x = "Year", y = "NO2 (mikrog/m3)") +
  theme_minimal()
no2_plot_mean

pm25_plot_mean <- ggplot(air_quality_year, aes(x = year, y = pm25_mean)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly mean PM2.5", x = "Year", y = "PM2.5 (mikrog/m3)") +
  theme_minimal()

prop_el_plot <- ggplot(air_quality_year, aes(x = year, y = proportion_electric)) +
  geom_line() +
  geom_point() +
  labs(title = "Proportion of Electric Vehicles", x = "Year", y = "Proportion of EVs") +
  theme_minimal()

no2_plot_median <- ggplot(air_quality_year, aes(x = year, y = no2_median)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly median NO2", x = "Year", y = "NO2 (mikrog/m3)") +
  theme_minimal()

pm25_plot_median <- ggplot(air_quality_year, aes(x = year, y = pm25_median)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly median PM2.5", x = "Year", y = "PM2.5 (mikrog/m3)") +
  theme_minimal()
pm25_plot_median

no2_plot_threshold <- ggplot(air_quality_year, aes(x = year, y = no2_over_threshold)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of days with NO2 over threshold", x = "Year", y = "Number of days") +
  theme_minimal()
no2_plot_threshold
pm25_plot_threshold <- ggplot(air_quality_year, aes(x = year, y = pm25_over_threshold)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of days with PM2.5 over threshold", x = "Year", y = "Number of days") +
  theme_minimal()
pm25_plot_threshold

no2_plot_threshold / no2_plot_mean / no2_plot_median / prop_el_plot
pm25_plot_threshold / pm25_plot_mean / pm25_plot_median / prop_el_plot


#### Possible expantion #######
# what day of the week has the highest pollution?
# what month of the year has the highest pollution?

