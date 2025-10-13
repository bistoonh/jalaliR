convert_date <- function(dates, from = "jalali") {
  # --- تشخیص مقصد ---
  to <- if (from == "jalali") "gregorian" else if (from == "gregorian") "jalali" else stop("from must be 'jalali' or 'gregorian'")
  
  # --- نرمال‌سازی ورودی ---
  dates_norm <- normalize_date(dates, calendar = from)
  
  # --- جدا کردن تاریخ و زمان ---
  dt_split <- strsplit(dates_norm, " ")
  date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
  time_only <- vapply(dt_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
  
  # --- بارگذاری جدول ---
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  
  # --- تبدیل با استفاده از data.table ---
  if (requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
    
    dt_map <- data.table(jalali_date = jalali_greg_map$jalali_date,
                         gregorian_date = jalali_greg_map$gregorian_date)
    
    dt_input <- data.table(date_only = date_only)
    dt_input[, idx := .I]  # حفظ ترتیب
    
    if (from == "jalali") {
      setkey(dt_map, jalali_date)
      dt_input[, converted := dt_map[.SD, gregorian_date, on = c(jalali_date = "date_only")]]
    } else {
      setkey(dt_map, gregorian_date)
      dt_input[, converted := dt_map[.SD, jalali_date, on = c(gregorian_date = "date_only")]]
    }
    
    result_date <- dt_input$converted
  } else {
    # --- fallback با list2env ---
    if (from == "jalali") {
      lookup_env <- list2env(setNames(as.list(jalali_greg_map$gregorian_date),
                                      jalali_greg_map$jalali_date), hash = TRUE, parent = emptyenv())
    } else {
      lookup_env <- list2env(setNames(as.list(jalali_greg_map$jalali_date),
                                      jalali_greg_map$gregorian_date), hash = TRUE, parent = emptyenv())
    }
    
    result_date <- vapply(date_only, function(x) {
      val <- lookup_env[[x]]
      if (is.null(val)) NA_character_ else val
    }, character(1))
  }
  
  # --- چسباندن زمان ---
  result <- ifelse(is.na(time_only) | time_only == "", result_date, paste(result_date, time_only))
  
  return(result)
}
