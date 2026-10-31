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

# Scale Canadian Gas Production time series 
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Log Transformation of Canadian Gas Production",
    subtitle = "Volume of Canadian Gas Production (Jan 1960-Feb 2005)",
    x = NULL, y = "Volume (log scale)", color = NULL
  ) +
  scale_y_continuous(transform = "log10") +
  theme_minimal()

## There was a log transformation taken of the Canadian gas production time series plot to show a true scale of gas production and reveal how growth has slowed over time.


### PART 3



## The non-seasonally adjusted data is much noisier with larger swings with the seasons in the calendar year. 










