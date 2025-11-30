# jalaliR

**jalaliR** is an R package for **fast and accurate conversion** between Jalali (Persian) and Gregorian calendars.  
It efficiently handles **large datasets** using a **pre-built mapping table** covering the years **1000–1600 (Jalali)**.

The package provides five main functions:

1. `normalize_date()`: Normalize date strings to `"YYYY-MM-DD"` format, with optional time. Automatically fixes incomplete dates and hours/minutes/seconds.  
2. `convert_date()`: Convert dates from Jalali to Gregorian or vice versa using the pre-built mapping.  
3. `day2day()`: Generate a sequence of dates between two given dates, including optional start and end times.  
4. `jalali_diff()`: Compute the difference in days between two Jalali dates using fast index lookup.  
5. `jalali_time_diff()`: Compute the difference between two Jalali date-time values. Returns both raw seconds and a formatted `"X days HH:MM:SS"` string.

The package uses **data.table** for high-speed processing when installed, and falls back to a fast hash-based lookup if not.  

---

## Installation

```r
# install.packages("devtools") # if not installed
devtools::install_github("bistoonh/jalaliR", force = TRUE)
```

---

## Examples

```r
library(jalaliR)

# Normalize dates
normalize_date(c("14020101", "1402-01", "1402-01-01 14:30:00"))
# [1] "1402-01-01" "1402-01-01" "1402-01-01 14:30:00"

# Convert Jalali to Gregorian
convert_date("1402-01-01", from = "jalali")
# [1] "2023-03-21"

# Convert Gregorian to Jalali
convert_date("2023-03-21", from = "gregorian")
# [1] "1402-01-01"

# Generate a sequence of Jalali dates
day2day("1402-01-01", "1402-01-03", calendar = "jalali")
# [1] "1402-01-01" "1402-01-02" "1402-01-03"

# Include start and end times
day2day("1402-01-01 08:00:00", "1402-01-03 18:00:00", calendar = "jalali")
# [1] "1402-01-01 08:00:00" "1402-01-02" "1402-01-03 18:00:00"

# Multiple conversions in a vector
dates <- c("1402-01-01", "1402-05-15", "1402-12-29")
convert_date(dates, from = "jalali")
# [1] "2023-03-21" "2023-08-06" "2024-03-18"

# Compute day differences between Jalali dates
jalali_diff("1402-01-01", "1402-01-03")
# [1] 2

# Vectorized day differences
jalali_diff(c("1402-01-01","1402-01-05"), c("1402-01-03","1402-01-10"))
# [1] 2 5

# Compute date-time differences (seconds + formatted)
res <- jalali_time_diff("1402-01-01 12:00:00", "1402-01-02 14:30:00")
res$seconds   # 95400
res$formatted # "1 days 02:30:00"
```

---

## Data

The package includes a **built-in dataset**:

```r
data("jalali_greg_map", package = "jalaliR")
head(jalali_greg_map)
```

This table maps Jalali dates to Gregorian dates for **years 1300–1500**, allowing **instant lookup without calculations**.  
It is the core of the package’s **high performance** conversion functions.

---

## License

MIT License

