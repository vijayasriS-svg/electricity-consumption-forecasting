Electricity Consumption Forecasting for a Single Day (15-Minute Intervals)

Author: [Nicolas Len](https://www.linkedin.com/in/niclen/)

Data ScienceTech Institute, 2025





``` r
# Set CRAN mirror
options(repos = c(CRAN = "https://cran.rstudio.com"))

# Load necessary libraries
library(tidyverse)    # For data wrangling and visualization
library(knitr)        # For creating tables and controlling chunk output
library(kableExtra)   # For enhanced table formatting
library(ggplot2)      # For data visualization
library(dplyr)        # For data manipulation
library(tidyr)        # For data tidying
library(readr)        # For reading data
library(lubridate)    # For date manipulation
library(stringr)      # For string manipulation
library(forcats)      # For categorical data handling
library(rmarkdown)    # For document rendering
library(forecast)    

# Set global chunk options
knitr::opts_chunk$set(
  echo = TRUE,        # Display code in the output
  eval = TRUE,       # Do not evaluate code chunks
  warning = FALSE,    # Suppress warnings in the output
  message = FALSE,    # Suppress messages in the output
  fig.width = 7,      # Set default figure width
  fig.height = 5,     # Set default figure height
  fig.align = 'center', # Center align all figures by default
  cache = TRUE        # Enable caching to speed up knitting
)
```

Load models

``` r
# Load the saved models from their RDS files
hw_default <- readRDS("models/hw_default.rds")
hw_ft <- readRDS("models/hw_ft.rds")
arima_auto <- readRDS("models/arima_auto.rds")
tslm_model <- readRDS("models/tslm_model.rds")
arima_manual <- readRDS("models/arima_manual.rds")
arima_manual_xreg <- readRDS("models/arima_manual_xreg.rds")
nnar_temp <- readRDS("models/nnar_temp.rds")
nnar_temp_dt <- readRDS("models/nnar_temp_dt.rds")
nnar_temp_dt_ft <- readRDS("models/nnar_temp_dt_ft.rds")
rf_dt_no_lag <- readRDS("models/rf_dt_no_lag.rds")
rf_id_no_lag <- readRDS("models/rf_id_no_lag.rds")
xg_id_no_lag <- readRDS("models/xg_id_no_lag.rds")
rf_id_lagged <- readRDS("models/rf_id_lagged.rds")
final_model <- readRDS("models/final_model.rds")
```

# 1: Preprocessing


``` r
library(readxl)
library(lubridate)

# Set up the path as a separate line
file_path <- "consumption_15min_train.xlsx"

# Read the Excel file 
df <- read_excel(file_path)

# Define the column names
colnames(df) <- c("timestamp","temp","power") 
```


``` r
# Remove the first row
df <- df[-1, ]

# Drop the 4th
if (ncol(df) >= 4) {
  df <- df[, -4]
}

# View the modified dataset
head(df)
```

    ## # A tibble: 6 × 3
    ##   timestamp     temp  power
    ##   <chr>         <chr> <chr>
    ## 1 1/1/2010 1:15 51    165.1
    ## 2 1/1/2010 1:30 51    151.6
    ## 3 1/1/2010 1:45 51    146.9
    ## 4 1/1/2010 2:00 51    153.7
    ## 5 1/1/2010 2:15 51    153.8
    ## 6 1/1/2010 2:30 51    159


``` r
summary(df)
```

    ##   timestamp             temp              power          
    ##  Length:32059       Length:32059       Length:32059      
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character

We see that all the 3 columns are strings. We need to convert timestamps
and make ‘temp’ and ‘power’ numeric. After the transformation we will
double check NAs


``` r
# Parse all timestamps
df$timestamp <- parse_date_time(
  df$timestamp,
  orders = c("mdY HM", "Y-m-d H:M:S"),
  tz = "UTC"
)
```


``` r
df$temp <- as.numeric(df$temp)
df$power <- as.numeric(df$power)
```


``` r
summary(df)
```

    ##    timestamp                        temp            power      
    ##  Min.   :2010-01-01 01:15:00   Min.   : 33.00   Min.   :  0.0  
    ##  1st Qu.:2010-03-25 12:52:30   1st Qu.: 53.00   1st Qu.:195.4  
    ##  Median :2010-06-17 00:30:00   Median : 58.00   Median :276.5  
    ##  Mean   :2010-06-17 00:30:00   Mean   : 59.01   Mean   :262.3  
    ##  3rd Qu.:2010-09-08 12:07:30   3rd Qu.: 64.00   3rd Qu.:313.1  
    ##  Max.   :2010-11-30 23:45:00   Max.   :100.00   Max.   :457.9  
    ##                                                 NA's   :96

Now, as expected we have just 96 NAs in power column which refers to the
last day that we should forecast.


``` r
# Install and load tsibble
library(tsibble)

# Convert data frame to a tsibble
df_tsibble <- as_tsibble(df, index = timestamp)

# Check if the time series is regular
is_regular <- is_regular(df_tsibble)
print(paste("Is the time series regular?", is_regular))
```

    ## [1] "Is the time series regular? TRUE"

``` r
# Identify gaps in the time series
gaps <- scan_gaps(df_tsibble)
print("Gaps in the time series:")
```

    ## [1] "Gaps in the time series:"

``` r
print(gaps)
```

    ## # A tsibble: 0 x 1 [?] <UTC>
    ## # ℹ 1 variable: timestamp <dttm>

Based on the output, the dataframe is regular and there are no gaps in
the timestamps.


Despite the fact we haven’t found any gaps in the data frame, by manual
inspection of the data set, we can see that the observations in the
first date start from 01:15 instead of 00:00. It means that the first
day of the observation doesn’t have the full set of observations. Taking
into account that our forecast period starts at 00:00, the best way is
to delete the first day from our analysis.

``` r
library(dplyr)
library(lubridate)
```

Step 1: Calculate the frequency of the time series in minutes

``` r
frequency_minutes <- as.numeric(difftime(df$timestamp[2], df$timestamp[1], units = "mins"))
print(paste("Detected frequency (minutes):", frequency_minutes))
```

    ## [1] "Detected frequency (minutes): 15"

Step 2: Calculate the expected number of observations per day

``` r
observations_per_day <- 24 * 60 / frequency_minutes
print(paste("Expected observations per day:", observations_per_day))
```

    ## [1] "Expected observations per day: 96"

Step 3: Identify incomplete days

``` r
incomplete_days <- df %>%
  group_by(date = as.Date(timestamp)) %>%
  summarise(observations = n(), .groups = "drop") %>%
  filter(observations < observations_per_day)

# Print days with missing observations
if (nrow(incomplete_days) > 0) {
  print("Days missing full set of observations:")
  print(incomplete_days)
} else {
  print("No missing days found.")
}
```

    ## [1] "Days missing full set of observations:"
    ## # A tibble: 1 × 2
    ##   date       observations
    ##   <date>            <int>
    ## 1 2010-01-01           91

Step 4: Remove incomplete days from df

``` r
df <- df %>%
  filter(!as.Date(timestamp) %in% incomplete_days$date)

print("Incomplete days removed. df updated.")
```

    ## [1] "Incomplete days removed. df updated."

``` r
# Display the first few rows of the updated data frame to verify
print(head(df))
```

    ## # A tibble: 6 × 3
    ##   timestamp            temp power
    ##   <dttm>              <dbl> <dbl>
    ## 1 2010-01-02 00:00:00    56  163.
    ## 2 2010-01-02 00:15:00    51  154.
    ## 3 2010-01-02 00:30:00    51  152.
    ## 4 2010-01-02 00:45:00    51  159.
    ## 5 2010-01-02 01:00:00    51  164.
    ## 6 2010-01-02 01:15:00    50  159.


``` r
# Split df into historical and forecast parts based on NA in the power column
df_historical <- df %>%
  filter(!is.na(power))

df_forecast <- df %>%
  filter(is.na(power))

# Print summary information to verify the split
print("Historical Data (df_historical):")
```

    ## [1] "Historical Data (df_historical):"

``` r
print(head(df_historical))
```

    ## # A tibble: 6 × 3
    ##   timestamp            temp power
    ##   <dttm>              <dbl> <dbl>
    ## 1 2010-01-02 00:00:00    56  163.
    ## 2 2010-01-02 00:15:00    51  154.
    ## 3 2010-01-02 00:30:00    51  152.
    ## 4 2010-01-02 00:45:00    51  159.
    ## 5 2010-01-02 01:00:00    51  164.
    ## 6 2010-01-02 01:15:00    50  159.

``` r
print(paste("Number of historical rows:", nrow(df_historical)))
```

    ## [1] "Number of historical rows: 31872"

``` r
print("Forecast Data (df_forecast):")
```

    ## [1] "Forecast Data (df_forecast):"

``` r
print(head(df_forecast))
```

    ## # A tibble: 6 × 3
    ##   timestamp            temp power
    ##   <dttm>              <dbl> <dbl>
    ## 1 2010-11-30 00:00:00    41    NA
    ## 2 2010-11-30 00:15:00    39    NA
    ## 3 2010-11-30 00:30:00    39    NA
    ## 4 2010-11-30 00:45:00    39    NA
    ## 5 2010-11-30 01:00:00    39    NA
    ## 6 2010-11-30 01:15:00    37    NA

``` r
print(paste("Number of forecast rows:", nrow(df_forecast)))
```

    ## [1] "Number of forecast rows: 96"


Check for power

``` r
ggplot(df_historical, aes(x = timestamp, y = power)) +
geom_line() +
labs( x = "Timestamp", y = "Power Consumption")
```

<img src="index_files/figure-gfm/unnamed-chunk-15-1.png" alt="" style="display: block; margin: auto;" />

Based on the graph we can obviously see at least 2 zones of outliers.
Let’s check for the temp

``` r
ggplot(df_historical, aes(x = timestamp, y = temp)) +
geom_line() +
labs( x = "Timestamp", y = "Temp")
```

<img src="index_files/figure-gfm/unnamed-chunk-16-1.png" alt="" style="display: block; margin: auto;" />

The data point reaching 100 degrees seems suspicious.


We will use tsclean function from forecast library to impute the
outliers

``` r
# Load the forecast package
library(forecast)

# Step 1: Convert the power and temp columns in df_historical to time series objects
# Assuming a regular frequency
power_ts <- ts(df_historical$power, frequency = 96)
temp_ts <- ts(df_historical$temp, frequency = 96)

# Step 2: tsclean to impute outliers and missing values in both columns
cleaned_power <- tsclean(power_ts)
cleaned_temp <- tsclean(temp_ts)

# Step 3: Replace the original columns in df_historical with the cleaned values
df_historical$power <- as.numeric(cleaned_power)
df_historical$temp <- as.numeric(cleaned_temp)

# Print the summary of the cleaned dataframe
summary(df_historical)
```

    ##    timestamp                        temp           power      
    ##  Min.   :2010-01-02 00:00:00   Min.   :33.00   Min.   :120.6  
    ##  1st Qu.:2010-03-25 23:56:15   1st Qu.:53.00   1st Qu.:196.9  
    ##  Median :2010-06-16 23:52:30   Median :58.00   Median :277.4  
    ##  Mean   :2010-06-16 23:52:30   Mean   :59.04   Mean   :263.1  
    ##  3rd Qu.:2010-09-07 23:48:45   3rd Qu.:64.00   3rd Qu.:313.3  
    ##  Max.   :2010-11-29 23:45:00   Max.   :95.00   Max.   :430.5


Check for power

``` r
ggplot(df_historical, aes(x = timestamp, y = power)) +
geom_line() +
labs( x = "Timestamp", y = "Power Consumption")
```

<img src="index_files/figure-gfm/unnamed-chunk-18-1.png" alt="" style="display: block; margin: auto;" />

Now the power looks better. What about temp?

``` r
ggplot(df_historical, aes(x = timestamp, y = temp)) +
geom_line() +
labs( x = "Timestamp", y = "Temp")
```

<img src="index_files/figure-gfm/unnamed-chunk-19-1.png" alt="" style="display: block; margin: auto;" />

Not much difference from the original series, but it still looks a bit
smoother.

# 2: Data analysis

Taking into account that temperature influence the power consumption and
power consumption doesn’t influence the temperature, we are dealing with
univariate time series with external regressor presented by temperature


``` r
# Convert power to a time series object
power_ts <- ts(df_historical$power, frequency = 96) # 96 intervals per day for 15-min data

# Extract temp as a time series (external regressor)
temp_ts <- ts(df_historical$temp, frequency = 96)
```


``` r
# Decompose the time series to analyze trend and seasonality
decomposition <- stl(power_ts, s.window = "periodic")
autoplot(decomposition) +
  ggtitle("Decomposition of Power Time Series") +
  labs(x = "Time", y = "Power (kW)")
```

<img src="index_files/figure-gfm/unnamed-chunk-21-1.png" alt="" style="display: block; margin: auto;" />

From the plot we can infer that there is an upward trend in the first
half a year and a slight downward trend in the second half year. Taking
into account that we have a lot of data points and at the same time we
are required to forecast just one day in November, let’s cut the dataset
in such a way that we keep only downward trend. It can help our models
to capture linear downward trend instead of taking into account more
complicated parabolic trend. In addition, by doing so we will reduce the
computation workload.


In order to optimize computation workload we will take a dataset of the
last 64 days. Taking into account that we are going to train machine
learning models we split the data set into 3 subsets: training,
validation and test sets. This should help us to avoid overfitting. In
addition I have a hypothesis (it will be checked later) that we have a
strong consumption pattern through 24 hours: Night, Morning, Day,
Evening. Let’s add these new features to the dataset through one-hot
encoding.

``` r
library(lubridate)

# 0) Add one-hot encoded columns based on the timestamp column
df_historical$hour <- hour(df_historical$timestamp)
df_historical$Night   <- as.integer(df_historical$hour >= 0  & df_historical$hour < 6)
df_historical$Morning <- as.integer(df_historical$hour >= 6  & df_historical$hour < 12)
df_historical$Day     <- as.integer(df_historical$hour >= 12 & df_historical$hour < 18)
df_historical$Evening <- as.integer(df_historical$hour >= 18 & df_historical$hour < 24)
df_historical$hour <- NULL  # remove temporary column

# 1) Identify earliest & latest day in df_historical
first_day <- min(df_historical$timestamp)
last_day  <- max(df_historical$timestamp)
# If df_historical doesn't have day_number yet, create it
df_historical$day_number <- as.integer(difftime(df_historical$timestamp, first_day, units = "days")) + 1
total_days <- max(df_historical$day_number)

cat("First day:", format(first_day, "%Y-%m-%d"), "\n")
```

    ## First day: 2010-01-02

``` r
cat("Last  day:", format(last_day,  "%Y-%m-%d"), "\n")
```

    ## Last  day: 2010-11-29

``` r
cat("Total days in dataset:", total_days, "\n\n")
```

    ## Total days in dataset: 332

``` r
# 2) Desired day counts (train, val, test)
n_train <- 60
n_val   <-  2
n_test  <-  2
days_needed <- n_train + n_val + n_test

# We keep the last 'days_needed' days of the dataset
cut_start_day <- total_days - days_needed + 1
cat("We keep days", cut_start_day, "through", total_days, "...\n\n")
```

    ## We keep days 269 through 332 ...

``` r
# 3) Build df_historical_cut
df_historical_cut <- subset(df_historical, day_number >= cut_start_day)

# 4) Define day ranges for train, val, test
train_start <- cut_start_day
train_end   <- cut_start_day + n_train - 1
val_start   <- train_end + 1
val_end     <- train_end + n_val
test_start  <- val_end + 1
test_end    <- val_end + n_test

cat("Train day_number range:", train_start, "to", train_end, "\n")
```

    ## Train day_number range: 269 to 328

``` r
cat("Val   day_number range:", val_start, "to", val_end, "\n")
```

    ## Val   day_number range: 329 to 330

``` r
cat("Test  day_number range:", test_start, "to", test_end, "\n\n")
```

    ## Test  day_number range: 331 to 332

``` r
# 5) Keep separate dataframes:
df_historical_cut_train     <- subset(df_historical_cut, day_number >= train_start & day_number <= train_end)
df_historical_cut_val       <- subset(df_historical_cut, day_number >= val_start   & day_number <= val_end)
df_historical_cut_test      <- subset(df_historical_cut, day_number >= test_start  & day_number <= test_end)
df_historical_cut_train_val <- subset(df_historical_cut, day_number >= train_start & day_number <= val_end)

# 6) Create multivariate ts object
ts_historical_cut <- ts(df_historical_cut[ , !(names(df_historical_cut) %in% c("timestamp"))],
                        frequency = 96, start = c(cut_start_day, 1))

# Create the training, validation, test, and train+validation mts objects using window()
ts_historical_cut_train     <- window(ts_historical_cut, start = c(train_start, 1), end = c(train_end, 96))
ts_historical_cut_val       <- window(ts_historical_cut, start = c(val_start, 1),   end = c(val_end, 96))
ts_historical_cut_test      <- window(ts_historical_cut, start = c(test_start, 1),  end = c(test_end, 96))
ts_historical_cut_train_val <- window(ts_historical_cut, start = c(train_start, 1), end = c(val_end, 96))
```

Build a new STL plot

``` r
# Decompose the time series to analyze trend and seasonality
decomposition <- stl(ts_historical_cut[,"power"], s.window = "periodic")
autoplot(decomposition) +
  ggtitle("Decomposition of Power Time Series") +
  labs(x = "Time", y = "Power (kW)")
```

<img src="index_files/figure-gfm/unnamed-chunk-23-1.png" alt="" style="display: block; margin: auto;" />


Despite the fact that we have cut our dataset, it’s still too large for
visual analysis purposes. Let’s have look at the last month.

``` r
power_ts_cut_month=window(power_ts,start=c(300,1),end=c(332,96))
temp_ts_cut_month=window(temp_ts,start=c(300,1),end=c(332,96))
```

Build an STL plot

``` r
# Decompose the time series to analyze trend and seasonality
decomposition <- stl(power_ts_cut_month, s.window = "periodic")
autoplot(decomposition) +
  ggtitle("Decomposition of Power Time Series") +
  labs(x = "Time", y = "Power (kW)")
```

<img src="index_files/figure-gfm/unnamed-chunk-25-1.png" alt="" style="display: block; margin: auto;" />

Now we can clearly see a stable seasonality pattern. Moreover, we can
see that the remainder looks like it has some additional pattern (not
just the white noise)


``` r
seasonplot(power_ts_cut_month, main = "Daily Seasonal Plot", col = rainbow(7), year.labels = FALSE, pch = 19)
```

<img src="index_files/figure-gfm/unnamed-chunk-26-1.png" alt="" style="display: block; margin: auto;" />

From the daily seasonality plot we can clearly see a pattern that we can
observe in real life. Night hours are the lowest in terms of electricity
consumption, the peak hours are somewhat in the evening. Moreover, I
have a hypothesis that the consumption on weekends or some weekdays can
be less or instead higher than on other days. Let’s check this
hypothesis.


Convert timestamp to Date format and aggregate to daily level

``` r
df_daily <- df_historical_cut %>%
  mutate(date = as.Date(timestamp)) %>%  # Extract only the date part
  group_by(date) %>%
  summarise(daily_power = mean(power, na.rm = TRUE))  # Compute daily mean power
```

Convert to a Time Series Object

``` r
power_ts_weekly <- ts(df_daily$daily_power, frequency = 7, start = c(year(df_daily$date[1]), week(df_daily$date[1])))
```

Visualize seasonality based on weekdays

``` r
seasonplot(power_ts_weekly, main = "Weekday Seasonal Plot",
           col = rainbow(7), year.labels = FALSE, pch = 19)
```

<img src="index_files/figure-gfm/unnamed-chunk-29-1.png" alt="" style="display: block; margin: auto;" />

From the plot it’s quite ambiguous whether the daily seasonality exists.
Let’s make ANOVA test

``` r
# Ensure weekday column is present
df_daily$weekday <- weekdays(df_daily$date)

# Convert weekday to a factor (ensures correct ordering)
df_daily$weekday <- factor(df_daily$weekday, 
                           levels = c("Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday", "Saturday", "Sunday"))

anova_test <- aov(daily_power ~ weekday, data = df_daily)
summary(anova_test)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## weekday      6    423    70.6   0.207  0.973
    ## Residuals   57  19439   341.0

P-value (Pr(\>F)) is much greater than 0.05, meaning no significant
difference in power consumption across weekdays.

F-value A very low F-value suggests that variation in power consumption
is not explained by weekdays.

Sum of Squares (Sum Sq) for weekday is very small compared to the
residual variance, meaning most variability in power usage comes from
other factors, not the day of the week.

Thus, there is no strong evidence that power consumption varies
significantly across weekdays.


``` r
library(tseries)

# Perform the Augmented Dickey-Fuller Test
adf_result <- adf.test(ts_historical_cut[,"power"], alternative = "stationary")

# Print the result
print(adf_result)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  ts_historical_cut[, "power"]
    ## Dickey-Fuller = -16.275, Lag order = 18, p-value = 0.01
    ## alternative hypothesis: stationary

The results of the Augmented Dickey-Fuller (ADF) test indicate that the
time series is stationary.


Let’s have a look at the relationship between Power and Temperature

``` r
plot(temp_ts_cut_month, power_ts_cut_month, xlab="Temperature", ylab="Power", main="Scatter plot of Power vs Temperature")
```

<img src="index_files/figure-gfm/unnamed-chunk-32-1.png" alt="" style="display: block; margin: auto;" />

The scatter plot shows that temperature indeed has influence on power
consumption. However, the relationship is not linear or quadratic.
Instead the plot shows two distinct clusters. Let’s further investigate
the nature of the clusters. Despite the fact that I have already checked
the influence of weekdays on the power consumption, I would like to
double check it on the scatter plot

``` r
# Extract day of the week and classify as weekday (0) or weekend (1)
df_historical_cut$day_type <- ifelse(weekdays(df_historical_cut$timestamp) %in% c("Saturday", "Sunday"), 1, 0)
```

Plot the points marked weekend or weekday

``` r
# Define colors: Blue for Weekday (0), Red for Weekend (1)
colors <- ifelse(df_historical_cut$day_type == 0, "blue", "red")

# Create scatter plot with color differentiation
plot(df_historical_cut$temp, df_historical_cut$power, col = colors, pch = 19,
     xlab = "Temperature", ylab = "Power", main = "Power vs Temperature by Day Type")

# Add legend
legend("topleft", legend = c("Weekday", "Weekend"), col = c("blue", "red"), pch = 19)
```

<img src="index_files/figure-gfm/unnamed-chunk-34-1.png" alt="" style="display: block; margin: auto;" />

We have confirmed that the clusters we see are not workday vs weekend.
Let’s go further: we create an additional column that classifies each
data point in terms of the day time: Night, Morning, Day, Evening.

``` r
# Extract hour from timestamp
df_historical_cut$hour <- as.numeric(format(df_historical_cut$timestamp, "%H"))

# Classify into four time clusters
df_historical_cut$daytime <- cut(df_historical_cut$hour, 
                                 breaks = c(-1, 5, 11, 17, 23),
                                 labels = c("Night", "Morning", "Day", "Evening"))

# Define colors for each cluster
color_map <- c("Night" = "blue", "Morning" = "yellow", "Day" = "green", "Evening" = "red")
colors <- color_map[df_historical_cut$daytime]

# Create scatter plot with color differentiation
plot(df_historical_cut$temp, df_historical_cut$power, col = colors, pch = 19,
     xlab = "Temperature", ylab = "Power", main = "Power vs Temperature by Daytime Clusters")

# Add legend
legend("topleft", legend = names(color_map), col = color_map, pch = 19)
```

<img src="index_files/figure-gfm/unnamed-chunk-35-1.png" alt="" style="display: block; margin: auto;" />

Great! Power demand follows a predictable daily cycle: Night: low,
Morning: increasing, Day: higher, but variable, Evening: peak demand.
For now we won’t create any additional columns and let the models
capture the seasonal pattern.


The analysis reveals the following attributes of our time series:

1.  The dataset is stationary;
2.  There is a slight downward trend;
3.  There is a strong intraday seasonality that can be devided into 4
    clusters
4.  Days of week follow similar energy consumption patterns
5.  Temperature serves as an external regressor with a non-linear
    dependancy


Based on the task, we need to perform a forecast both with and without
an external regressor. Here is the set of the models to be tested:

1.  Holt-Winters (HW) - without xreg
2.  ARIMA – without and with xreg
3.  NNETAR – without and with xreg
4.  Random Forest (RF) – without and with xreg
5.  XGBoost – without and with xreg
6.  Prophet (by Meta) – without and with xreg

# 3: Modelling


Taking into account that our dataset demonstrates seasonality and a
subtle downward trend we will start our training with Holt-Winters (HW)
model which is an extension of exponential smoothing used for time
series forecasting, particularly when the data exhibits trend and
seasonality. Based on the graphs we can conclude that seasonal
variations are relatively constant. That is why we will proceed with
Additive HW. As HW is deterministic we don’t need validation set here
that’s why we will use combined train and validation set for training.

### 3.1.1 Default HW

Let’s start with default parameters

``` r
library(forecast)
hw_default <- HoltWinters(ts_historical_cut_train_val[,"power"], 
                        seasonal = "additive",
                        alpha = NULL,    
                        beta  = NULL,
                        gamma = NULL)
```

Forecast for the next 2 days (test set)

``` r
library(ggplot2)
library(forecast)
hw_default_forecast <- forecast(hw_default, h = 2 * 96)

autoplot(ts_historical_cut_test[,"power"], series = "Test Data") + autolayer(hw_default_forecast$mean, series="additive HW")
```

<img src="index_files/figure-gfm/unnamed-chunk-37-1.png" alt="" style="display: block; margin: auto;" />

RMSE calculation

``` r
# Calculate the Root Mean Squared Error (RMSE)
hw_default_rmse <- sqrt(mean((hw_default_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))

# Print the RMSE
cat("HW Default RMSE:", hw_default_rmse, "\n")
```

    ## HW Default RMSE: 18.54785

From the plot we can infer that the model is trying to repeat the
pattern of the test set, but it considerably underperforms on the high,
middle and low peaks. Let’s check residuals and numeric parameters.

``` r
summary(hw_default)
```

    ##              Length Class  Mode     
    ## fitted       23424  mts    numeric  
    ## x             5952  ts     numeric  
    ## alpha            1  -none- numeric  
    ## beta             1  -none- numeric  
    ## gamma            1  -none- numeric  
    ## coefficients    98  -none- numeric  
    ## seasonal         1  -none- character
    ## SSE              1  -none- numeric  
    ## call             6  -none- call

``` r
checkresiduals(hw_default)
```

<img src="index_files/figure-gfm/unnamed-chunk-39-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from HoltWinters
    ## Q* = 6652.2, df = 192, p-value < 2.2e-16
    ## 
    ## Model df: 0.   Total lags used: 192

``` r
tsdisplay(residuals(hw_default))
```

<img src="index_files/figure-gfm/unnamed-chunk-39-2.png" alt="" style="display: block; margin: auto;" />

From ACF and PCF we clearly see that unfortunately a strong
autocorrelation remains in the residuals. It is also confirmed by
extremely low p-value in the Ljung–Box test. Let’s check the smoothing
parameters the model has chosen.

``` r
# Extract all optimized smoothing parameters
optimized_parameters <- c(alpha = hw_default$alpha,
                          beta = hw_default$beta,
                          gamma = hw_default$gamma)

# Print the optimized parameters
cat("Optimized Default Parameters:\n")
```

    ## Optimized Default Parameters:

``` r
print(optimized_parameters)
```

    ## alpha.alpha   beta.beta gamma.gamma 

### 3.1.2 HW Finetuned

Let’s use the default smoothing parameters as a starting point for the
adjustment. Just to recap: - Alpha (α) determines how much weight is
given to the most recent observations (1 gives more weight to recent
observations, a value close to 0 gives more weight to older
observations) - Beta (β) controls the smoothing of the trend component -
Gamma (γ) determines how much weight is given to the most recent
observations when estimating the seasonal pattern

As our test data are the autumn days, it makes sense to give more weight
to recent seasonal patterns by increasing Gamma, as the whether is
getting colder and the seasonal pattern from the latest days can be the
most relevant. Also we will play around with Alpha, and at the same time
will be very cautious in terms of Beta as the trend is very subtle.

``` r
library(forecast)
hw_ft <- HoltWinters(ts_historical_cut_train_val[,"power"], 
                        seasonal = "additive",
                        alpha = 0.45,    # or just omit entirely
                        beta  = 0.00003,
                        gamma = 0.8)
```

Forecast for the next 2 days (test set)

``` r
library(ggplot2)
library(forecast)
hw_ft_forecast <- forecast(hw_ft, h = 2 * 96)

autoplot(ts_historical_cut_test[,"power"], series = "Test Data") + autolayer(hw_ft_forecast$mean, series="additive HW Finetuned")
```

<img src="index_files/figure-gfm/unnamed-chunk-42-1.png" alt="" style="display: block; margin: auto;" />

RMSE calculation

``` r
# Calculate the Root Mean Squared Error (RMSE)
hw_ft_rmse <- sqrt(mean((hw_ft_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))

# Print the RMSE
cat("HW Finetuned RMSE:", hw_ft_rmse, "\n")
```

    ## HW Finetuned RMSE: 12.20363

Through multiple iterations we have managed to find a set of parameters
leading us to a visible improvement of the plot and RMSE. Let’s check
residuals and numeric parameters.

``` r
summary(hw_ft)
```

    ##              Length Class  Mode     
    ## fitted       23424  mts    numeric  
    ## x             5952  ts     numeric  
    ## alpha            1  -none- numeric  
    ## beta             1  -none- numeric  
    ## gamma            1  -none- numeric  
    ## coefficients    98  -none- numeric  
    ## seasonal         1  -none- character
    ## SSE              1  -none- numeric  
    ## call             6  -none- call

``` r
checkresiduals(hw_ft)
```

<img src="index_files/figure-gfm/unnamed-chunk-44-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from HoltWinters
    ## Q* = 7035.7, df = 192, p-value < 2.2e-16
    ## 
    ## Model df: 0.   Total lags used: 192

``` r
tsdisplay(residuals(hw_ft))
```

<img src="index_files/figure-gfm/unnamed-chunk-44-2.png" alt="" style="display: block; margin: auto;" />

Despite the improvement of RMSE. ACF and PCF still indicates a strong
autocorrelation in the residuals. It is also confirmed by extremely low
p-value in the Ljung–Box test. It means that the model struggles to
catch the patterns in a proper way.


### 3.2.1 Auto-ARIMA w/o Xreg

``` r
library(forecast)
arima_auto = auto.arima(
  ts_historical_cut_train_val[,"power"],
  seasonal = TRUE,
  approximation = TRUE,
  stepwise = TRUE,
  trace = TRUE,
  max.p = 15, max.q = 15,  
  max.P = 2, max.Q = 2
)
```

Forecast for the next 2 days (test set)

``` r
library(ggplot2)
library(forecast)
arima_auto_forecast <- forecast(arima_auto, h = 2 * 96)

autoplot(ts_historical_cut_test[,"power"], series = "Test Data") + autolayer(arima_auto_forecast$mean, series="Auto Arima")
```

<img src="index_files/figure-gfm/unnamed-chunk-46-1.png" alt="" style="display: block; margin: auto;" />

RMSE calculation

``` r
# Calculate the Root Mean Squared Error (RMSE)
arima_auto_rmse <- sqrt(mean((arima_auto_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))

# Print the RMSE
cat("Auto Arima RMSE:", arima_auto_rmse, "\n")
```

    ## Auto Arima RMSE: 9.714442

The plot and RMSE immediately look better than those of HW. Let’s check
residuals and parameters.

``` r
summary(arima_auto)
```

    ## Series: ts_historical_cut_train_val[, "power"] 
    ## ARIMA(7,0,2)(0,1,0)[96] 
    ## 
    ## Coefficients:
    ##          ar1      ar2     ar3     ar4      ar5     ar6     ar7      ma1     ma2
    ## s.e.  0.1677   0.2195  0.0584  0.0311   0.0282  0.0299  0.0181   0.1675  0.1393
    ## 
    ## sigma^2 = 80.03:  log likelihood = -21137.02
    ## AIC=42294.03   AICc=42294.07   BIC=42360.79
    ## 
    ## Training set error measures:
    ##                       ME     RMSE      MAE         MPE     MAPE      MASE
    ## Training set -0.07274023 8.866605 6.695282 -0.08068506 2.617148 0.5719789
    ##                      ACF1
    ## Training set 0.0001754989

``` r
checkresiduals(arima_auto)
```

<img src="index_files/figure-gfm/unnamed-chunk-48-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(7,0,2)(0,1,0)[96]
    ## Q* = 1640.1, df = 183, p-value < 2.2e-16
    ## 
    ## Model df: 9.   Total lags used: 192

``` r
tsdisplay(residuals(arima_auto))
```

<img src="index_files/figure-gfm/unnamed-chunk-48-2.png" alt="" style="display: block; margin: auto;" />

The ACF and PACF plots indicate that the time series is relatively
uncorrelated except for a set of distinct negative spikes in the PACF
(and one in ACF). Notably, these spikes occur in PACF at seasonal
lags—specifically at lags 96, 192, 288, etc.—with the largest spike at
lag 96, and each subsequent spike decreasing in magnitude. This pattern
suggests a seasonal effect with a period of our 96 observations per day.
The reason could be a drop in energy consumption at night. At the next
tries we will focus on seasonal AR terms (P), but first let’s start with
TSLM by removing the effect of covariate.

### 3.2.2 TSLM

``` r
tslm_model=tslm(power~temp+trend+season,data=ts_historical_cut_train_val)
```

Forecast for the next 2 days (test set)

``` r
library(ggplot2)
library(forecast)
tslm_model_forecast <- forecast(tslm_model, newdata = ts_historical_cut_test[,"temp"])

autoplot(ts_historical_cut_test[,"power"], series = "Test Data") + autolayer(tslm_model_forecast$mean, series="TSLM")
```

<img src="index_files/figure-gfm/unnamed-chunk-50-1.png" alt="" style="display: block; margin: auto;" />

RMSE calculation

``` r
# Calculate the Root Mean Squared Error (RMSE)
tslm_rmse <- sqrt(mean((tslm_model_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))

# Print the RMSE
cat("TSLM RMSE:", tslm_rmse, "\n")
```

    ## TSLM RMSE: 15.80533

The plot and RMSE is worse then Auto Arima and fine tuned HW, but
interestingly it’s better than default HW. It’s due to the fact that we
have added temperature which improves the performace. Let’s check
residuals and parameters.

``` r
summary(tslm_model)
```

    ## 
    ## Call:
    ## tslm(formula = power ~ temp + trend + season, data = ts_historical_cut_train_val)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -53.385  -9.725  -1.389   8.063  65.156 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 121.640314   2.993176  40.639  < 2e-16 ***
    ## temp          1.424798   0.033666  42.322  < 2e-16 ***
    ## trend        -0.003236   0.000157 -20.606  < 2e-16 ***
    ## season2      -4.271103   2.761150  -1.547  0.12195    
    ## season3      -4.247889   2.761149  -1.538  0.12399    
    ## season4      -7.749697   2.761148  -2.807  0.00502 ** 
    ## season5      -4.729695   2.761147  -1.713  0.08678 .  
    ## season6       0.279252   2.761521   0.101  0.91946    
    ## season7      -8.395824   2.761519  -3.040  0.00237 ** 
    ## season8      -8.127577   2.761518  -2.943  0.00326 ** 
    ## season9      -5.244402   2.761516  -1.899  0.05760 .  
    ## season10     -1.812562   2.761994  -0.656  0.51169    
    ## season11     -1.595921   2.761992  -0.578  0.56341    
    ## season12     -1.694237   2.761989  -0.613  0.53963    
    ## season13     -4.495836   2.761986  -1.628  0.10363    
    ## season14      0.695040   2.762658   0.252  0.80137    
    ## season15     -3.344016   2.762655  -1.210  0.22616    
    ## season16     -4.518280   2.762651  -1.635  0.10200    
    ## season17      0.363991   2.762648   0.132  0.89518    
    ## season18      4.731876   2.763091   1.713  0.08685 .  
    ## season19      0.549562   2.763088   0.199  0.84235    
    ## season20     -2.856169   2.763084  -1.034  0.30132    
    ## season21     -0.691830   2.763080  -0.250  0.80230    
    ## season22     -1.731055   2.763577  -0.626  0.53109    
    ## season23      0.770234   2.763573   0.279  0.78048    
    ## season24      0.726070   2.763569   0.263  0.79277    
    ## season25      2.588720   2.763564   0.937  0.34893    
    ## season26     11.892866   2.763896   4.303 1.71e-05 ***
    ## season27     16.422673   2.763887   5.942 2.98e-09 ***
    ## season28     19.537360   2.763878   7.069 1.74e-12 ***
    ## season29     17.416127   2.763869   6.301 3.17e-10 ***
    ## season30     24.549402   2.763849   8.882  < 2e-16 ***
    ## season31     23.228210   2.763844   8.404  < 2e-16 ***
    ## season32     18.623492   2.763840   6.738 1.76e-11 ***
    ## season33     19.650669   2.763835   7.110 1.30e-12 ***
    ## season34    100.743698   2.762072  36.474  < 2e-16 ***
    ## season35     98.754029   2.762070  35.754  < 2e-16 ***
    ## season36     95.624410   2.762067  34.621  < 2e-16 ***
    ## season37     95.590924   2.762064  34.609  < 2e-16 ***
    ## season38     90.341255   2.761274  32.717  < 2e-16 ***
    ## season39     90.307660   2.761275  32.705  < 2e-16 ***
    ## season40     90.749176   2.761276  32.865  < 2e-16 ***
    ## season41     95.560822   2.761277  34.607  < 2e-16 ***
    ## season42     92.553881   2.764454  33.480  < 2e-16 ***
    ## season43     93.183770   2.764460  33.708  < 2e-16 ***
    ## season44     92.641336   2.764465  33.511  < 2e-16 ***
    ## season45     94.222696   2.764470  34.083  < 2e-16 ***
    ## season46    100.449029   2.769828  36.265  < 2e-16 ***
    ## season47    103.108137   2.769836  37.225  < 2e-16 ***
    ## season48    101.093678   2.769845  36.498  < 2e-16 ***
    ## season49    100.700170   2.769853  36.356  < 2e-16 ***
    ## season50    101.960705   2.776998  36.716  < 2e-16 ***
    ## season51    104.822469   2.777010  37.747  < 2e-16 ***
    ## season52    101.205561   2.777021  36.444  < 2e-16 ***
    ## season53    101.596445   2.777033  36.585  < 2e-16 ***
    ## season54    101.083472   2.783242  36.319  < 2e-16 ***
    ## season55    103.517625   2.783256  37.193  < 2e-16 ***
    ## season56    102.962353   2.783269  36.993  < 2e-16 ***
    ## season57    102.955646   2.783283  36.991  < 2e-16 ***
    ## season58    100.784374   2.787367  36.158  < 2e-16 ***
    ## season59    104.894555   2.787382  37.632  < 2e-16 ***
    ## season60     99.635188   2.787396  35.745  < 2e-16 ***
    ## season61     98.476276   2.787411  35.329  < 2e-16 ***
    ## season62    102.179997   2.787725  36.654  < 2e-16 ***
    ## season63    105.761610   2.787740  37.938  < 2e-16 ***
    ## season64    102.324593   2.787755  36.705  < 2e-16 ***
    ## season65    101.340965   2.787770  36.352  < 2e-16 ***
    ## season66    103.639908   2.784009  37.227  < 2e-16 ***
    ## season67    104.094178   2.783967  37.391  < 2e-16 ***
    ## season68     99.916685   2.783925  35.891  < 2e-16 ***
    ## season69    111.598686   2.783883  40.087  < 2e-16 ***
    ## season70    116.348187   2.776457  41.905  < 2e-16 ***
    ## season71    117.217279   2.776466  42.218  < 2e-16 ***
    ## season72    112.531057   2.776473  40.530  < 2e-16 ***
    ## season73    110.835804   2.776479  39.920  < 2e-16 ***
    ## season74    121.432686   2.769384  43.848  < 2e-16 ***
    ## season75    129.081802   2.769380  46.610  < 2e-16 ***
    ## season76    135.109748   2.769376  48.787  < 2e-16 ***
    ## season77    140.505998   2.769371  50.736  < 2e-16 ***
    ## season78    153.046922   2.765138  55.349  < 2e-16 ***
    ## season79    150.255554   2.765144  54.339  < 2e-16 ***
    ## season80    144.851066   2.765150  52.385  < 2e-16 ***
    ## season81    144.127199   2.765157  52.123  < 2e-16 ***
    ## season82    144.400720   2.762869  52.265  < 2e-16 ***
    ## season83    139.788238   2.762873  50.595  < 2e-16 ***
    ## season84    138.973752   2.762877  50.300  < 2e-16 ***
    ## season85    139.009413   2.762881  50.313  < 2e-16 ***
    ## season86    138.747930   2.761895  50.236  < 2e-16 ***
    ## season87    137.879337   2.761898  49.922  < 2e-16 ***
    ## season88    132.394465   2.761901  47.936  < 2e-16 ***
    ## season89    130.386569   2.761904  47.209  < 2e-16 ***
    ## season90    120.950190   2.761256  43.803  < 2e-16 ***
    ## season91    119.882858   2.761261  43.416  < 2e-16 ***
    ## season92    116.279837   2.761266  42.111  < 2e-16 ***
    ## season93    113.185371   2.761271  40.990  < 2e-16 ***
    ## season94     31.574391   2.761111  11.435  < 2e-16 ***
    ## season95     30.344839   2.761111  10.990  < 2e-16 ***
    ## season96      2.243527   2.761111   0.813  0.41651    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 15.37 on 5854 degrees of freedom
    ## Multiple R-squared:  0.9425, Adjusted R-squared:  0.9415 
    ## F-statistic: 988.4 on 97 and 5854 DF,  p-value: < 2.2e-16

``` r
checkresiduals(tslm_model)
```

<img src="index_files/figure-gfm/unnamed-chunk-52-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Breusch-Godfrey test for serial correlation of order up to 192
    ## 
    ## data:  Residuals from Linear regression model
    ## LM test = 4655, df = 192, p-value < 2.2e-16

``` r
tsdisplay(residuals(tslm_model))
```

<img src="index_files/figure-gfm/unnamed-chunk-52-2.png" alt="" style="display: block; margin: auto;" />

This model tells us that power consumption is very well explained (about
94% of the variation) by temperature, a time trend, and seasonal
effects. In particular:

• The temperature coefficient is highly significant, meaning that for
every one‐unit increase in temperature, power consumption increases by
about 1.425 units, holding other factors constant.

• The trend coefficient is negative and highly significant, which
suggests a very slight downward movement in power consumption over time.
We saw it on the graph.

• The seasonal dummy variables capture strong cyclical patterns
throughout the period. Some seasonal coefficients (especially those
later in the year) are very large and statistically significant,
indicating that power usage in certain seasons differs markedly from the
baseline season. In contrast, a few seasonal dummies (e.g., season2,
season3, season5, season6, season10, etc.) are not statistically
significant, suggesting that not every period deviates significantly
from the baseline.

ACF shows a strong repeating pattern that does not die out quickly, it
may imply that we need seasonal differencing at least D=1. Also Large,
slowly decaying spikes at seasonal lags can incur a seasonal MA
component (𝑄\>0).

Based on the plots from auto arima and tslm we need to consider non zero
P,D,Q

### 3.2.3 Manual ARIMA w/o Xreg

let’s take the parameters from the auto arima as starting point which is
ARIMA(7,0,2)(0,1,0)\[96\] and add P=1 and Q=1

``` r
library(forecast)
arima_manual <- Arima(
ts_historical_cut_train_val[,"power"],
order = c(0,1,9),
seasonal = list(order = c(1,1,1))
)
```

Forecast for the next 2 days (test set)

``` r
library(ggplot2)
library(forecast)
arima_manual_forecast <- forecast(arima_manual, h = 2 * 96)

autoplot(ts_historical_cut_test[,"power"], series = "Test Data") + autolayer(arima_manual_forecast$mean, series="Manual Arima")
```

<img src="index_files/figure-gfm/unnamed-chunk-54-1.png" alt="" style="display: block; margin: auto;" />

RMSE calculation

``` r
# Calculate the Root Mean Squared Error (RMSE)
arima_manual_rmse <- sqrt(mean((arima_manual_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))

# Print the RMSE
cat("Manual Arima RMSE:", arima_manual_rmse, "\n")
```

    ## Manual Arima RMSE: 8.299387

Let’s check residuals and parameters.

``` r
summary(arima_manual)
```

    ## Series: ts_historical_cut_train_val[, "power"] 
    ## ARIMA(0,1,9)(1,1,1)[96] 
    ## 
    ## Coefficients:
    ##           ma1      ma2      ma3     ma4      ma5      ma6     ma7     ma8
    ##       -0.4070  -0.0532  -0.0860  0.0237  -0.0512  -0.0333  0.0203  0.0172
    ## s.e.   0.0131   0.0142   0.0141  0.0142   0.0142   0.0143  0.0142  0.0145
    ##          ma9    sar1     sma1
    ## s.e.  0.0132  0.0191   0.0138
    ## 
    ## sigma^2 = 53.54:  log likelihood = -19997.87
    ## AIC=40019.75   AICc=40019.8   BIC=40099.85
    ## 
    ## Training set error measures:
    ##                       ME     RMSE     MAE         MPE     MAPE     MASE
    ## Training set -0.01026584 7.250247 5.37239 -0.04477942 2.087084 0.458964
    ##                       ACF1
    ## Training set -0.0003108443

``` r
checkresiduals(arima_manual)
```

<img src="index_files/figure-gfm/unnamed-chunk-56-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(0,1,9)(1,1,1)[96]
    ## Q* = 344.29, df = 181, p-value = 2.951e-12
    ## 
    ## Model df: 11.   Total lags used: 192

``` r
tsdisplay(residuals(arima_manual))
```

<img src="index_files/figure-gfm/unnamed-chunk-56-2.png" alt="" style="display: block; margin: auto;" />

After multiple iterations we have managed to achieve the best result
with the following model

ARIMA(0,1,9)(1,1,1)\[96\]

Theoretically increase of P and Q could improve the result, but the
calculation takes too long. Also the increase of q more than 9 leads to
infinite calculation. Adding any non-zero parameter to p while having
(1,1,1) for (P,D,Q) doesn’t allow even to start training (the code
breaks). Unfortunately despite the best RMSE so far we haven’t managed
to capture the full pattern as the residuals contain patterns. Let’s
test a model with this parameters with temperature as xreg.

### 3.2.4 Manual ARIMA w/ Xreg

let’s take thi model ARIMA(0,1,9)(1,1,1)\[96\] and add temperature as
xreg

``` r
library(forecast)
arima_manual_xreg <- Arima(ts_historical_cut_train_val[,"power"],
                      order = c(0,1,9),
                      seasonal = list(order = c(1,1,1)),
                      xreg = ts_historical_cut_train_val[,"temp"])
```

Forecast for the next 2 days (test set)

``` r
library(ggplot2)
library(forecast)
arima_manual_xreg_forecast <- forecast(arima_manual_xreg, h = 2 * 96, 
                                  xreg = ts_historical_cut_test[,"temp"])

autoplot(ts_historical_cut_test[,"power"], series = "Test Data") + 
  autolayer(arima_manual_xreg_forecast$mean, series = "Manual Arima w/ Xreg")
```

<img src="index_files/figure-gfm/unnamed-chunk-58-1.png" alt="" style="display: block; margin: auto;" />

RMSE calculation

``` r
# Calculate the Root Mean Squared Error (RMSE)
arima_manual_xreg_rmse <- sqrt(mean((arima_manual_xreg_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))

# Print the RMSE
cat("Manual Arima w/ Xreg RMSE:", arima_manual_xreg_rmse, "\n")
```

    ## Manual Arima w/ Xreg RMSE: 8.066566

Let’s check residuals and parameters.

``` r
summary(arima_manual_xreg)
```

    ## Series: ts_historical_cut_train_val[, "power"] 
    ## Regression with ARIMA(0,1,9)(1,1,1)[96] errors 
    ## 
    ## Coefficients:
    ##           ma1      ma2      ma3     ma4      ma5      ma6     ma7     ma8
    ##       -0.4084  -0.0543  -0.0865  0.0232  -0.0524  -0.0346  0.0199  0.0152
    ## s.e.   0.0131   0.0142   0.0141  0.0142   0.0143   0.0143  0.0142  0.0146
    ##          ma9    sar1     sma1    xreg
    ## s.e.  0.0132  0.0191   0.0137  0.1125
    ## 
    ## sigma^2 = 53.49:  log likelihood = -19994.67
    ## AIC=40015.35   AICc=40015.41   BIC=40102.12
    ## 
    ## Training set error measures:
    ##                        ME     RMSE      MAE         MPE     MAPE     MASE
    ## Training set -0.009865449 7.246073 5.364769 -0.04535045 2.083983 0.458313
    ##                       ACF1
    ## Training set -0.0003246181

``` r
checkresiduals(arima_manual_xreg)
```

<img src="index_files/figure-gfm/unnamed-chunk-60-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from Regression with ARIMA(0,1,9)(1,1,1)[96] errors
    ## Q* = 340.25, df = 181, p-value = 7.839e-12
    ## 
    ## Model df: 11.   Total lags used: 192

``` r
tsdisplay(residuals(arima_manual_xreg))
```

<img src="index_files/figure-gfm/unnamed-chunk-60-2.png" alt="" style="display: block; margin: auto;" />

With xreg, we’ve managed to improve the RMSE. However, the residuals
still exhibit autocorrelation. For our upcoming models, we plan to
incorporate xreg right from the start of the training process.


### 3.3.1 NNAR w/ Temperature

Let’s start with NNAR including temperature as xreg with default
parameters.

``` r
library(forecast)
nnar_temp=nnetar(ts_historical_cut_train_val[,2],xreg=ts_historical_cut_train_val[,1])
```

Forecast for the next 2 days (test set)

``` r
library(ggplot2)
library(forecast)
nnar_temp_forecast <- forecast(nnar_temp, h = 2 * 96, 
                                  xreg = ts_historical_cut_test[,1])
```


``` r
autoplot(ts_historical_cut_test[,"power"], series = "Test Data") + 
  autolayer(nnar_temp_forecast$mean, series = "NNAR w/ Temp")
```


From the plot we can see that for some reasons the model performs poorly
at the high peaks. Let’s have a look at RMSE.

``` r
# Calculate the Root Mean Squared Error (RMSE)
nnar_temp_rmse <- sqrt(mean((nnar_temp_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))
```


``` r
# Print the RMSE
cat("NNAR w/ Temp RMSE:", nnar_temp_rmse, "\n")
```


Interestingly RMSE is even better than the fine tuned HW, but
considerably worse than all ARIMA models.

``` r
summary(nnar_temp)
```

    ##           Length Class        Mode     
    ## x         5952   ts           numeric  
    ## m            1   -none-       numeric  
    ## p            1   -none-       numeric  
    ## P            1   -none-       numeric  
    ## scalex       2   -none-       list     
    ## scalexreg    2   -none-       list     
    ## size         1   -none-       numeric  
    ## xreg      5952   -none-       numeric  
    ## subset    5952   -none-       numeric  
    ## model       20   nnetarmodels list     
    ## nnetargs     0   -none-       list     
    ## fitted    5952   ts           numeric  
    ## residuals 5952   ts           numeric  
    ## lags        31   -none-       numeric  
    ## series       1   -none-       character
    ## method       1   -none-       character
    ## call         3   -none-       call

``` r
checkresiduals(nnar_temp)
```

<img src="index_files/figure-gfm/unnamed-chunk-64-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from NNAR(30,1,16)[96]
    ## Q* = 865.82, df = 192, p-value < 2.2e-16
    ## 
    ## Model df: 0.   Total lags used: 192

``` r
tsdisplay(residuals(nnar_temp))
```

<img src="index_files/figure-gfm/unnamed-chunk-64-2.png" alt="" style="display: block; margin: auto;" />

As before we still don’t see the white noise in ACF and PACF (some
pattern is still here).

### 3.3.2 NNAR w/ Temperature & Daytime

In paragraph 2.8 devoted to the relationship between power and
temperature we have identified 4 time clusters: Night, Morning, Day,
Evening. Let’s add these parameters through one-hot encoding into xreg
along with the temperature.

``` r
library(forecast)
nnar_temp_dt=nnetar(ts_historical_cut_train_val[,2],xreg=ts_historical_cut_train_val[, c(1, 3, 4, 5, 6)])
```

Forecast for the next 2 days (test set)

``` r
library(ggplot2)
library(forecast)
nnar_temp_dt_forecast <- forecast(nnar_temp_dt, h = 2 * 96, 
                                  xreg = ts_historical_cut_test[, c(1, 3, 4, 5, 6)])
```


``` r
autoplot(ts_historical_cut_test[,"power"], series = "Test Data") + 
  autolayer(nnar_temp_dt_forecast$mean, series = "NNAR w/ Xreg & Daytime")
```


RMSE calculation

``` r
# Calculate the Root Mean Squared Error (RMSE)
nnar_temp_dt_rmse <- sqrt(mean((nnar_temp_dt_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))
```


``` r
# Print the RMSE
cat("NNAR w/ Temp & Daytime:", nnar_temp_dt_rmse, "\n")
```


By adding daytime as an additional xreg we have substantially improved
the performance of the model. The result is comparable to basic Auto
Arima.

``` r
summary(nnar_temp_dt)
```

    ##           Length Class        Mode     
    ## x          5952  ts           numeric  
    ## m             1  -none-       numeric  
    ## p             1  -none-       numeric  
    ## P             1  -none-       numeric  
    ## scalex        2  -none-       list     
    ## scalexreg     2  -none-       list     
    ## size          1  -none-       numeric  
    ## xreg      29760  mts          numeric  
    ## subset     5952  -none-       numeric  
    ## model        20  nnetarmodels list     
    ## nnetargs      0  -none-       list     
    ## fitted     5952  ts           numeric  
    ## residuals  5952  ts           numeric  
    ## lags         31  -none-       numeric  
    ## series        1  -none-       character
    ## method        1  -none-       character
    ## call          3  -none-       call

``` r
print(nnar_temp_dt)
```
``` r
checkresiduals(nnar_temp_dt)
```

<img src="index_files/figure-gfm/unnamed-chunk-68-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from NNAR(30,1,18)[96]
    ## Q* = 551.41, df = 192, p-value < 2.2e-16
    ## 
    ## Model df: 0.   Total lags used: 192

``` r
tsdisplay(residuals(nnar_temp_dt))
```

<img src="index_files/figure-gfm/unnamed-chunk-68-2.png" alt="" style="display: block; margin: auto;" />

The residuals also look better. Let’s try to fine tune this model.

### 3.3.3 NNAR w/ Temp & Daytime finetuned

``` r
library(forecast)
# Define the tuning grid with parameters that vary
tuning_grid <- expand.grid(
  p = 30,        # Non-seasonal autoregressive lags
  P = 1,                  # Seasonal autoregressive lags
  size = 17         # Hidden layer sizes
)

best_rmse <- Inf
best_model <- NULL
best_params <- list()
total_models <- nrow(tuning_grid)
cat("Total models to train:", total_models, "\n")

for(i in 1:nrow(tuning_grid)) {
  cat("Training model", i, "of", total_models, "\n")
  
  # Extract parameters that vary
  p_val    <- tuning_grid$p[i]
  P_val    <- tuning_grid$P[i]
  size_val <- tuning_grid$size[i]
  
  # Fit NNAR model using the training set 
  model <- nnetar(ts_historical_cut_train[, 2],
                  xreg = ts_historical_cut_train[, c(1, 3, 4, 5, 6)],
                  p = p_val,
                  P = P_val,
                  size = size_val,
                  repeats = 20,                # Fixed parameter
                  lambda = "auto",             # Fixed parameter
                  decay = 0,                   # Fixed parameter
                  maxit = 1000,                # Fixed parameter
                  stepmax = 1e+05,             # Fixed parameter
                  MaxNWts = 1000,              # Added parameter to increase weight limit
                  trace = FALSE)
  
  # Forecast over the validation horizon
  fc <- forecast(model, xreg = ts_historical_cut_val[, c(1, 3, 4, 5, 6)], h = length(ts_historical_cut_val[, 2]))
  rmse_val <- sqrt(mean((fc$mean - ts_historical_cut_val[, 2])^2))
  
  cat("RMSE for current model:", rmse_val, "\n\n")
  
  if(rmse_val < best_rmse) {
    best_rmse <- rmse_val
    best_model <- model
    best_params <- list(p = p_val, P = P_val, size = size_val)
  }
}

cat("Best parameters found on validation set:\n")
print(best_params)
cat("Best RMSE on validation set:", best_rmse, "\n")
```

Retrain the final model on the already combined training and validation
set

``` r
nnar_temp_dt_ft <- nnetar(ts_historical_cut_train_val[, 2],
                      xreg = ts_historical_cut_train_val[, c(1, 3, 4, 5, 6)],
                      p = best_params$p,
                      P = best_params$P,
                      size = best_params$size,
                      repeats = 20,            # Fixed parameter
                      lambda = "auto",         # Fixed parameter
                      decay = 0,               # Fixed parameter
                      maxit = 1000,            # Fixed parameter
                      stepmax = 1e+05,         # Fixed parameter
                      trace = FALSE)
```

Finally, forecast using the final model on the test set

``` r
library(ggplot2)
library(forecast)
nnar_temp_dt_ft_forecast <- forecast(nnar_temp_dt_ft, xreg = ts_historical_cut_test[, c(1, 3, 4, 5, 6)], h = length(ts_historical_cut_test[, 2]))
```


``` r
autoplot(ts_historical_cut_test[,"power"], series = "Test Data") + 
  autolayer(nnar_temp_dt_ft_forecast$mean, series = "Finetuned NNAR w/ Temp & Daytime")
```


Let’s check RMSE

``` r
nnar_temp_dt_ft_rmse <- sqrt(mean((nnar_temp_dt_ft_forecast$mean - ts_historical_cut_test[, 2])^2))
```


``` r
cat("RMSE Finetuned NNAR w/ Temp & Daytime:", nnar_temp_dt_ft_rmse, "\n")
```


Residuals

``` r
summary(nnar_temp_dt_ft)
```

    ##           Length Class        Mode     
    ## x          5952  ts           numeric  
    ## m             1  -none-       numeric  
    ## p             1  -none-       numeric  
    ## P             1  -none-       numeric  
    ## scalex        2  -none-       list     
    ## scalexreg     2  -none-       list     
    ## size          1  -none-       numeric  
    ## xreg      29760  mts          numeric  
    ## lambda        1  -none-       numeric  
    ## subset     5952  -none-       numeric  
    ## model        20  nnetarmodels list     
    ## nnetargs      4  -none-       list     
    ## fitted     5952  ts           numeric  
    ## residuals  5952  ts           numeric  
    ## lags         31  -none-       numeric  
    ## series        1  -none-       character
    ## method        1  -none-       character
    ## call         12  -none-       call

``` r
print(nnar_temp_dt_ft)
```
``` r
checkresiduals(nnar_temp_dt_ft$residuals)
```

<img src="index_files/figure-gfm/unnamed-chunk-73-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals
    ## Q* = 515.99, df = 192, p-value < 2.2e-16
    ## 
    ## Model df: 0.   Total lags used: 192

``` r
tsdisplay(nnar_temp_dt_ft$residuals)
```

<img src="index_files/figure-gfm/unnamed-chunk-73-2.png" alt="" style="display: block; margin: auto;" />

Unfortunately, after multiple iterations we haven’t managed to achieve
better results than the automatic nnetar function. I was changing the
number of lagged values of the time series to use as inputs to the
neural network (p), number of seasonal autoregressive lags (P), and
number of hidden nodes in the neural network (size).


### 3.4.1 RF with temp & daytime w/o lags

Let’s test random forest with temperature and daytime (morning, night,
day, evening) covariates. For now we won’t add time lags into model

``` r
library(randomForest)
set.seed(777)
rf_dt_no_lag=randomForest(ts_historical_cut_train_val[,2],x=ts_historical_cut_train_val[, c(1, 3, 4, 5, 6)])
```

Let’s build a graph

``` r
library(ggplot2)
library(randomForest)

rf_dt_no_lag_forecast=predict(rf_dt_no_lag,newdata=ts_historical_cut_test[, c(1, 3, 4, 5, 6)])

ts_rf_dt_no_lag_forecast=ts(rf_dt_no_lag_forecast,start=c(331,1),end=c(332,96),frequency = 96)

autoplot(ts_historical_cut_test[,"power"], series="test data")+autolayer(ts_rf_dt_no_lag_forecast,series="RF temperature & daytime w/o lags")
```

<img src="index_files/figure-gfm/unnamed-chunk-75-1.png" alt="" style="display: block; margin: auto;" />

Let’s check RMSE

``` r
rf_dt_no_lag_rmse <- sqrt(mean((rf_dt_no_lag_forecast - ts_historical_cut_test[, 2])^2))
cat("RMSE RF temperature & daytime w/o lags:", rf_dt_no_lag_rmse, "\n")
```

    ## RMSE RF temperature & daytime w/o lags: 34.45074

Residuals

``` r
residuals_rf_dt_no_lag <- ts_historical_cut_test[, "power"] - rf_dt_no_lag_forecast


summary(rf_dt_no_lag)
```

    ##                 Length Class  Mode     
    ## call               3   -none- call     
    ## type               1   -none- character
    ## predicted       5952   -none- numeric  
    ## mse              500   -none- numeric  
    ## rsq              500   -none- numeric  
    ## oob.times       5952   -none- numeric  
    ## importance         5   -none- numeric  
    ## importanceSD       0   -none- NULL     
    ## localImportance    0   -none- NULL     
    ## proximity          0   -none- NULL     
    ## ntree              1   -none- numeric  
    ## mtry               1   -none- numeric  
    ## forest            11   -none- list     
    ## coefs              0   -none- NULL     
    ## y               5952   ts     numeric  
    ## test               0   -none- NULL     
    ## inbag              0   -none- NULL

``` r
print(rf_dt_no_lag)
```

    ## 
    ## Call:
    ##  randomForest(x = ts_historical_cut_train_val[, c(1, 3, 4, 5,      6)], y = ts_historical_cut_train_val[, 2]) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 1
    ## 
    ##           Mean of squared residuals: 1276.392
    ##                     % Var explained: 68.4

``` r
checkresiduals(residuals_rf_dt_no_lag)
```

<img src="index_files/figure-gfm/unnamed-chunk-77-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals
    ## Q* = 554.31, df = 38, p-value < 2.2e-16
    ## 
    ## Model df: 0.   Total lags used: 38

``` r
tsdisplay(residuals_rf_dt_no_lag)
```

<img src="index_files/figure-gfm/unnamed-chunk-77-2.png" alt="" style="display: block; margin: auto;" />

This model demonstrates a quite poor performance. But an interesting
observation is that the model is at least following the pattern of the
daytime covariates. Let’s test a model for which we create for each day
96 numeric labels

### 3.4.2 RF with temp & 96 labels w/o lags

First let’s create and additional feature for train_val and test sets

``` r
# Pattern from 1..96
intraday_train_val <- rep(1:96, times = 62)
intraday_test <- rep(1:96, times = 2)
```

Let’s double check the number of rows in ts_historical_cut_train_val

``` r
# Get the number of rows directly
num_rows <- nrow(ts_historical_cut_train_val)

# Print the number of rows
print(num_rows)
```

    ## [1] 5952

Let’s add intraday column to train_val set

``` r
ts_historical_cut_train_val <- cbind(
  temp    = ts_historical_cut_train_val[, 1],
  power   = ts_historical_cut_train_val[, 2],
  Night  = ts_historical_cut_train_val[, 3],
  Morning  = ts_historical_cut_train_val[, 4],
  Day  = ts_historical_cut_train_val[, 5],
  Evening  = ts_historical_cut_train_val[, 6],
  day_number  = ts_historical_cut_train_val[, 7],
  intraday  = intraday_train_val[1:nrow(ts_historical_cut_train_val)]
)
```

Let’s add intraday column to test set

``` r
ts_historical_cut_test <- cbind(
  temp    = ts_historical_cut_test[, 1],
  power   = ts_historical_cut_test[, 2],
  Night  = ts_historical_cut_test[, 3],
  Morning  = ts_historical_cut_test[, 4],
  Day  = ts_historical_cut_test[, 5],
  Evening  = ts_historical_cut_test[, 6],
  day_number  = ts_historical_cut_test[, 7],
  intraday  = intraday_test[1:nrow(ts_historical_cut_test)]
)
```

Let’s fit the model

``` r
set.seed(777)
rf_id_no_lag=randomForest(y=ts_historical_cut_train_val[,2],x=ts_historical_cut_train_val[, c(1, 8)])
```

Check the graph

``` r
library(ggplot2)
library(randomForest)

rf_id_no_lag_forecast=predict(rf_id_no_lag,newdata=ts_historical_cut_test[, c(1, 8)])

ts_rf_id_no_lag_forecast=ts(rf_id_no_lag_forecast,start=c(331,1),end=c(332,96),frequency = 96)

autoplot(ts_historical_cut_test[,"power"], series="test data")+autolayer(ts_rf_id_no_lag_forecast,series="RF temperature & intraday w/o lags")
```

<img src="index_files/figure-gfm/unnamed-chunk-83-1.png" alt="" style="display: block; margin: auto;" />

Let’s check RMSE

``` r
rf_id_no_lag_rmse <- sqrt(mean((rf_id_no_lag_forecast - ts_historical_cut_test[, 2])^2))
cat("RMSE RF temperature & intraday w/o lags:", rf_id_no_lag_rmse, "\n")
```

    ## RMSE RF temperature & intraday w/o lags: 12.76255

Residuals

``` r
residuals_rf_id_no_lag <- ts_historical_cut_test[, "power"] - rf_id_no_lag_forecast


summary(rf_id_no_lag)
```

    ##                 Length Class  Mode     
    ## call               3   -none- call     
    ## type               1   -none- character
    ## predicted       5952   -none- numeric  
    ## mse              500   -none- numeric  
    ## rsq              500   -none- numeric  
    ## oob.times       5952   -none- numeric  
    ## importance         2   -none- numeric  
    ## importanceSD       0   -none- NULL     
    ## localImportance    0   -none- NULL     
    ## proximity          0   -none- NULL     
    ## ntree              1   -none- numeric  
    ## mtry               1   -none- numeric  
    ## forest            11   -none- list     
    ## coefs              0   -none- NULL     
    ## y               5952   ts     numeric  
    ## test               0   -none- NULL     
    ## inbag              0   -none- NULL

``` r
print(rf_id_no_lag)
```

    ## 
    ## Call:
    ##  randomForest(x = ts_historical_cut_train_val[, c(1, 8)], y = ts_historical_cut_train_val[,      2]) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 1
    ## 
    ##           Mean of squared residuals: 186.6302
    ##                     % Var explained: 95.38

``` r
checkresiduals(residuals_rf_id_no_lag)
```

<img src="index_files/figure-gfm/unnamed-chunk-85-1.png" alt="" style="display: block; margin: auto;" />

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals
    ## Q* = 443.02, df = 38, p-value < 2.2e-16
    ## 
    ## Model df: 0.   Total lags used: 38

``` r
tsdisplay(residuals_rf_id_no_lag)
```

<img src="index_files/figure-gfm/unnamed-chunk-85-2.png" alt="" style="display: block; margin: auto;" />

Great! By adding a numeric index for each 15-minutes for each day we
have managed to achieved a decent result comparable to other models. Now
let’s check XGBoost with the similar parameters.

### 3.4.3 XGBoost with temp & 96 labels w/o lags

``` r
set.seed(777)
library(xgboost)
xg_id_no_lag<- xgboost(data = ts_historical_cut_train_val[, c(1, 8)], label = ts_historical_cut_train_val[,2],
max_depth = 10, eta = .5, nrounds = 100,
nthread = 2, objective = "reg:squarederror", verbose = 0)
```

Check the graph

``` r
library(ggplot2)
library(xgboost)

xg_id_no_lag_forecast=predict(xg_id_no_lag,newdata=ts_historical_cut_test[, c(1, 8)])
```


``` r
ts_xg_id_no_lag_forecast=ts(xg_id_no_lag_forecast,start=c(331,1),end=c(332,96),frequency = 96)
```


``` r
autoplot(ts_historical_cut_test[,"power"], series="test data")+autolayer(ts_xg_id_no_lag_forecast,series="XGBoost temperature & intraday w/o lags")
```


Let’s check RMSE

``` r
xg_id_no_lag_rmse <- sqrt(mean((xg_id_no_lag_forecast - ts_historical_cut_test[, 2])^2))
```


``` r
cat("RMSE XGBoost temperature & intraday w/o lags:", xg_id_no_lag_rmse, "\n")
```


XGBoost has demonstrated a worse result than RF. Let’s add lags.

### 3.4.4 RF with temp & 96 labels w/ lags

First let’s create new columns with lagged values for the train_val
dataset

``` r
ts_lagged_train_val <- cbind(
  temp    = ts_historical_cut_train_val[, 1],
  power   = ts_historical_cut_train_val[, 2],
  Night  = ts_historical_cut_train_val[, 3],
  Morning  = ts_historical_cut_train_val[, 4],
  Day  = ts_historical_cut_train_val[, 5],
  Evening  = ts_historical_cut_train_val[, 6],
  day_number  = ts_historical_cut_train_val[, 7],
  intraday  = ts_historical_cut_train_val[, 8],
  x1 = c(rep(NA, 1), head(ts_historical_cut_train_val[, "power"], -1)),
  x2 = c(rep(NA, 2), head(ts_historical_cut_train_val[, "power"], -2)),
  x3 = c(rep(NA, 3), head(ts_historical_cut_train_val[, "power"], -3)),
  x4 = c(rep(NA, 4), head(ts_historical_cut_train_val[, "power"], -4)),
  x5 = c(rep(NA, 5), head(ts_historical_cut_train_val[, "power"], -5)),
  x6 = c(rep(NA, 6), head(ts_historical_cut_train_val[, "power"], -6)),
  x7 = c(rep(NA, 7), head(ts_historical_cut_train_val[, "power"], -7))
)
```

Remove the first 96 rows that contain NA in lag columns:

``` r
ts_lagged_train_val <- ts_lagged_train_val[-c(1:96), ]
```

Let’s fit the model

``` r
library(randomForest)

rf_id_lagged <- randomForest(
  y = ts_lagged_train_val[, 2],
  x = ts_lagged_train_val[, c(1,8:15)]
)
```

Initialize the Lag Vector

``` r
n_train <- nrow(ts_lagged_train_val)
# The last 7 real demands from training (in descending order):
x <- c(
  ts_lagged_train_val[n_train,     "power"],  # 7th-latest
  ts_lagged_train_val[n_train - 1, "power"],  # 6th-latest
  ts_lagged_train_val[n_train - 2, "power"],
  ts_lagged_train_val[n_train - 3, "power"],
  ts_lagged_train_val[n_train - 4, "power"],
  ts_lagged_train_val[n_train - 5, "power"],
  ts_lagged_train_val[n_train - 6, "power"]   # 1st-latest
)
```

Loop Over Each Test Row

``` r
n_test <- nrow(ts_historical_cut_test)
prev   <- numeric(n_test)  # store predictions here

for (i in seq_len(n_test)) {
  test_row <- cbind(
    temp     = ts_historical_cut_test[i, "temp"],
    power    = ts_historical_cut_test[i, "power"],    
    Night    = ts_historical_cut_test[i, "Night"],
    Morning  = ts_historical_cut_test[i, "Morning"],
    Day      = ts_historical_cut_test[i, "Day"],
    Evening  = ts_historical_cut_test[i, "Evening"],
    day_number = ts_historical_cut_test[i, "day_number"],
    intraday = ts_historical_cut_test[i, "intraday"],
    x1 = x[1],
    x2 = x[2],
    x3 = x[3],
    x4 = x[4],
    x5 = x[5],
    x6 = x[6],
    x7 = x[7]
  )
  
  # 2) Predict using the same columns that were in 'x=...' for training
  y_hat <- predict(
    rf_id_lagged, 
    newdata = test_row[, c(1,8:15)]
  )
  
  # 3) Save the forecast
  prev[i] <- y_hat
  
  # 4) SHIFT the x vector to incorporate the new forecast as the "most recent"
  # The sample does: x=c(y, x[1:6])
  x <- c(y_hat, x[1:6])
}

# 'prev' now has 1-step-ahead forecasts for each test row
```

Wrap to ts and build a plot

``` r
ts_preds <- ts(
  prev, 
  start = c(331, 1),  
  frequency = 96      
)

library(ggplot2)
autoplot(ts_historical_cut_test[,"power"], series="Test Data") +
  autolayer(ts_preds, series="RF Lagged Forecast")
```

<img src="index_files/figure-gfm/unnamed-chunk-94-1.png" alt="" style="display: block; margin: auto;" />

Compute RMSE vs. actual power in test set

``` r
rmse_iterative <- sqrt(mean((prev - ts_historical_cut_test[, "power"])^2))

cat("RMSE (iterative lagged model):", rmse_iterative, "\n")
```

    ## RMSE (iterative lagged model): 10.45727

Great! By adding lags we have managed to improve the model performance

# 4: Forecast


Let’s plot the best models

``` r
library(ggplot2)
library(forecast)

autoplot(ts_historical_cut_test[,"power"], series = "Test Data") +
  autolayer(hw_ft_forecast$mean, series = "Additive HW Finetuned") +
  autolayer(arima_manual_xreg_forecast$mean, series = "Manual ARIMA w/ Temp") +
  autolayer(nnar_temp_dt_forecast$mean, series = "NNAR w/ Temp & Daytime") +
  autolayer(ts_preds, series = "RF w/ Temp & Intraday Lagged") +
  xlab("Time") +
  ylab("Power") +
  ggtitle("Forecast Comparison of Multiple Models") +
  guides(colour = guide_legend(title = "Series"))
```


Let’s compare RMSEs of these models

``` r
# Calculate each model's RMSE
hw_ft_rmse <- sqrt(mean((hw_ft_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))
arima_manual_xreg_rmse <- sqrt(mean((arima_manual_xreg_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))
nnar_temp_dt_rmse <- sqrt(mean((nnar_temp_dt_forecast$mean - ts_historical_cut_test[,"power"]) ^ 2))
```


``` r
rmse_iterative <- sqrt(mean((prev - ts_historical_cut_test[, "power"])^2))

# Combine into a data frame
rmse_values <- data.frame(
  Model = c("HW Finetuned", 
            "Manual ARIMA w/ Temp", 
            "NNAR w/ Temp & Daytime", 
            "RF w/ Temp & Intraday Lagged"),
  RMSE = c(hw_ft_rmse, 
           arima_manual_xreg_rmse, 
           nnar_temp_dt_rmse, 
           rmse_iterative)
)
```


``` r
# Order by RMSE (ascending)
rmse_values <- rmse_values[order(rmse_values$RMSE), ]
```


``` r
# Print the table
print(rmse_values)
```


ARIMA with temperature as covariate is the best performing model.
Despite all our efforts we haven’t managed to achieve similar results
with all other approaches. But interestingly neural network with
covariates represented by temperature and one-hot encoded daytime frames
(night, morning, day, evening) has performed quite well. Let’s make a
forecast with our best model.


Let’s retrain our best model on the full dataset to capture the last 2
days we were using for test.

``` r
library(forecast)
final_model <- Arima(ts_historical_cut[,"power"],
                      order = c(0,1,9),
                      seasonal = list(order = c(1,1,1)),
                      xreg = ts_historical_cut[,"temp"])
```

Convert df_forecast to mts

``` r
ts_forecast <- ts(df_forecast[,"temp"],
                        frequency = 96)
```

Make forecast

``` r
library(forecast)
final_forecast <- forecast(final_model, h = 1 * 96, 
                                  xreg = ts_forecast[,"temp"])

autoplot(final_forecast$mean, series = "Manual Arima w/ Temp")
```

<img src="index_files/figure-gfm/unnamed-chunk-100-1.png" alt="" style="display: block; margin: auto;" />

Extract the 96 forecasted values as a data frame

``` r
library(openxlsx)
forecast_values <- as.data.frame(final_forecast$mean)

# Save these values to Excel 
write.xlsx(
  x        = forecast_values,
  file     = "output.xlsx",
  colNames = FALSE,
  rowNames = FALSE
)
```

## Contact

[Nicolas Len](https://www.linkedin.com/in/niclen/)

## Code Repository

Repository: [electricity-time-series](https://github.com/nicolas-len/electricity-time-series)
