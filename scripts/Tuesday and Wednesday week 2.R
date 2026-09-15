# ============================================================================
# EV adoption and air quality in Tromsø (Hansjordnesbukta station 203)

# ============================================================================

source("scripts/load_packages.R")


# =============================================================================
# PART 1 — Vehicles: compute EV proportion per year
# =============================================================================

# The Excel is a wide SSB export with multi-row headers.
# We read without column names, locate the Tromsø row, then reshape.
car_file <- 'Number of cars using petrol gas or electic.xlsx'
raw <- readxl::read_excel(car_file)

View(raw)
idx <- seq(from=2, to=104, by=6)



cars <- data.frame(year = c(2008:2025),
                   bensin = as.numeric(raw[5, idx]),
                   diesel = as.numeric(raw[5, idx + 1]),
                   parafin = as.numeric(raw[5, idx + 2]),
                   gass = as.numeric(raw[5, idx + 3]),
                   el = as.numeric(raw[5, idx + 4]),
                   other = as.numeric(raw[5, idx + 5])
)
                   
glimpse(cars)
cars <- cars %>%  mutate(prop_el = el/(bensin+diesel+parafin+gass+el+other))
glimpse(cars)


#plot(cars$prop_el, type="l")
# # make a nicer looking plot of prop_el over time using ggplot
# plot_prop_el <- ggplot(cars, aes(x = year, y = prop_el)) +
#   geom_line() +
#   labs(title = "Proportion of Electric Vehicles in Troms (2008-2025)",
#        x = "Year",
#        y = "Proportion of EVs (%)") +
#   theme_minimal() 
#plot_prop_el
cars <- cars %>% 
  filter(year >= 2015) # filter to years 2015-2025 for comparison with air quality data)
plot_prop_el <- ggplot(cars, aes(x = year, y = prop_el)) +
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

# Choose pollutants to examine.
# Traffic-related pollutants often include NO2, PM10, PM2.5.
# Adjust names to whatever the API expects.
pollutants <- c('no2', 'PM10', 'PM25')

# --- API helper --------------------------------------------------------------
# IMPORTANT: The exact endpoint paths are defined in the Miljødirektoratet Swagger.
# Because Swagger UIs can be configured differently, this function tries a small set of
# common endpoint patterns. If none work, it prints guidance.



base_url <- 'https://api-luftmalinger.miljodirektoratet.no/public/agg/2/2015-01-01/2025-12-30/Hansjordnesbukta?components=NO2&showinvalid=false'
#base_url <- 'https://api-luftmalinger.miljodirektoratet.no/public/agg/2/2010-01-01/2010-12-30/Danmarks%20plass?components=NO2&showinvalid=false'
response <- request(base_url) |> 
  req_perform()


c <- response |> 
  resp_body_json()  
  
glimpse(c)
str(c)


df_no2 <- map_dfr(c[[1]]$values, ~{
  tibble(
    dateTime = .x$dateTime,
    no2 = .x$value
  )
})

df_no2
df_no2$date <- as.Date(df_no2$dateTime)

base_url <- 'https://api-luftmalinger.miljodirektoratet.no/public/agg/2/2015-01-01/2025-12-30/Hansjordnesbukta?components=PM2.5&showinvalid=false'
#base_url <- 'https://api-luftmalinger.miljodirektoratet.no/public/agg/2/2010-01-01/2010-12-30/Danmarks%20plass?components=no2&showinvalid=false'
response <- request(base_url) |> 
  req_perform()


c <- response |> 
  resp_body_json()  

#glimpse(c)


df_pm25 <- map_dfr(c[[1]]$values, ~{
  tibble(
    dateTime = .x$dateTime,
    pm25 = .x$value
  )
})

df_pm25
df_pm25$date <- as.Date(df_pm25$dateTime)

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

# --- Visualization -------------------------------------
# pm25 <- plot(air_quality$date, air_quality$pm25,  type = "l", xlab = "Date", ylab = "PM2.5 (mikrog/m3)", main = "PM2.5 over time")
# no2 <- plot(air_quality$date, air_quality$no2,  type = "l", xlab = "Date", ylab = "no2 (mikrog/m3)", main = "no2 over time")

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
    title = "no2 over time",
    x = "Date",
    y = expression(no2 ~ "(" * mu * "g/m"^3 * ")")
  ) +
  theme_minimal()

#pm25
#no2

plot_prop_el / pm25 / no2
##### Transform of variables #####
#Yearly average 
# extract year from dateTime
air_quality <- air_quality %>% mutate(year = year(date))
#use mean or median?
hist(air_quality$no2, breaks = 130, main = "Distribution of no2", xlab = "no2 (mikrogram/m3)")
hist(air_quality$pm25, breaks = 130, main = "Distribution of PM2.5", xlab = "PM2.5 (mikrogram/m3)")


#Yearly median and mean appended to air_quality
# for no2
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
  left_join(cars %>% select(year, prop_el), by = "year")
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

prop_el_plot <- ggplot(air_quality_year, aes(x = year, y = prop_el)) +
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




