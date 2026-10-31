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
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Service Sales",
    subtitle = "Not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  autoplot(log_sales) +
  labs(
    title = "U.S. Retail and Food Service Sales",
    subtitle = "Not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

## This series has a weaker case for logging compared to can_gas. The underlying trend does not have a large significant difference between the logged and the un-logged version. While using the logged version of non-seasonally adjusted sales shows the clear linear upward trend in terms of absolute change, the un-logged sales also show this same pattern. While it does have a weaker case, the logged version of sales can still give you more information about absolute change that is not as clear in the un-logged version. 

## The non-seasonally adjusted data is much noisier with larger swings with the seasons in the calendar year. 










