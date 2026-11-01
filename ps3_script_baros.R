### PART 2
library(tidyverse)

# Load in Data
retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

## RSAFS seasonally adjusted 
## RSAFSNA not seasonally adjusted 

# Reshape Data
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

# Build tsibble
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)
head(retail_ts)

has_gaps(retail_ts)

# Canadian gas production

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

# Filtering can_gas date 
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

# Extracting parts of a data 
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume)) # Seasonal Pattern 
## June has the lowest average production and December has the highest.

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

# Monthly to Quarterly Data 
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# Plotting with autoplot() 
retail_ts |> autoplot(sales)

# ggplot() graph U.S. Retail and Food Service Sales
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Service Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

## In the series that is not seasonally adjusted, RSAFSNA, shows major spikes and drops that follow a general pattern year to year. The seasonally adjusted series removes the retail changes that occur monthly to show the true underlying trend. 
## Over the long run, the two series follow the same path. The seasonally adjusted data has dips around 2008 and 2020 which reflect the great recession and the pandemic. The unadjusted data also follows this trend, but also shows the monthly swings.  

# ggplot() Canadian gas production 
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    subtitle = "Volume of Canadian Gas Production (Jan 1960-Feb 2005)",
    x = NULL, y = "Volume", color = NULL
  ) +
  theme_minimal()

# Loess curve added to Canadian Gas Production time series 
can_gas |>
  autoplot(Volume) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE,
              color = "firebrick", formula = y ~ x) +
  labs(
    title = "Canadian Gas Production",
    subtitle = "Volume of Canadian Gas Production (Jan 1960-Feb 2005), with LOESS trend",
    x = NULL, y = "Volume", color = NULL
  ) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 20, 5)) +
  theme_minimal()
## A loess curve was added to the time series to show underlying trend and 0 added to the y axis.

### PART 3

## The seasonal swings seem to change with time periods. There is a time period around 1980 to about 1990 where there are large seasonal swings. After this period, there is still growth, and the swings are still present but not nearly as large. 

# Add natural log of Volume to can_gas
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))
  
  # Plot Logged Series
can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Scaled Canadian Gas Production",
    subtitle = "Logged scale of Volume of Canadian Gas Production (Jan 1960-Feb 2005)",
    x = NULL, y = "Volume (Log Scale)", color = NULL
  ) +
  theme_minimal()

## The log transformation taken of the Canadian gas production time series plot to show a true scale of gas production and reveal how growth has slowed over time.
## Taking the natural log of the can_gas time series is a good idea because without this, it appears that growth is trending upwards at the same rate. The log shows absolute change, so the logged version shows a clearer picture of how gas production has actually changed over time. This transformation shows the significant growth from the 1960s to around 1975. After this point production still grows but significantly flattens out. 

retail_ts <- retail_ts |>
  mutate(log_sales = log(sales))

retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA")

retail_nsa |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Service Sales",
    subtitle = "Not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "U.S. Retail and Food Service Sales",
    subtitle = "Not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

## This series has a weaker case for logging compared to can_gas. The underlying trend does not have a large significant difference between the logged and the un-logged version. While using the logged version of non-seasonally adjusted sales shows the clear linear upward trend in terms of absolute change, the un-logged sales also show this same pattern. While it does have a weaker case, the logged version of sales can still give you more information about absolute change that is not as clear in the un-logged version. 

# Growth Rates 
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

  # Plot month over month percentage growth rate (growth_direct with autoplot())
can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Month-over-month growth rate of volume of gas",
    subtitle = "Direct computation of volume growth rate",
    x = NULL, y = "Volume Growth Rate", color = NULL
  ) +
  theme_minimal() 

  # Growth gap between direct computation of growth rate and the first difference of the logged volume
can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct -growth_logdiff)
  )

  # Plot month over month absolute growth rate 
can_gas |> 
  autoplot(growth_gap) +
  labs(
    title = "Month-over-month Growth Gap",
    subtitle = "Absolute difference between direct computation and logged series of gas volume",
    x = NULL, y = "Volume Growth Rate", color = NULL
  ) +
  theme_minimal()

## In this case, it seems as though the logged difference has some error given the large absolute differences shown in the graph.

# SA vs NSA Growth Rates
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

## The non-seasonally adjusted series shows significantly more noise. The RSAFSNA series has much larger seasonal swings indicating more noise. The seasonally adjusted data removes these large swings to show true changes.

### PART 4

# Seasonal Plot 
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal() # NSA 

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal() # SA

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Logged Volume",
    y = "Volume"
  ) +
  theme_minimal()

## The months that tend to be higher in colder months, October to March, and lower in warmer months, April to September. This makes sense because people may be using extra gas to heat their homes in colder weather.


can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# Seasonally Adjusting a Series with X-13ARIMA-SEATS
library(seasonal)

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

## The seasonal factor seems to be highest in December, which tracts with the holidays being around this time. Sales at this time are roughly 10% to 20% above trend during this month. The seasonal swings appear to get somewhat smaller over time but are still present.

# Compare official seasonally adjusted series by computing the percent difference between them
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

  # Plot of comparison
sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

## The adjustment matches closely with only 4 months after 2020 that go over a 1% difference. The largest differences seem to be skewed left skewed with larger differences around 2020 and after. The differences do not follow any sort of pattern.















