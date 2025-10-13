convert_date <- function(dates, from = "jalali") {
  k <- 1
  
  print(paste(k, "- start function")); k <- k + 1
  
  # --- تشخیص مقصد ---
  to <- if(from == "jalali") "gregorian" else if(from == "gregorian") "jalali" else stop("from must be 'jalali' or 'gregorian'")
  print(paste(k, "- destination:", to)); k <- k + 1
  
  # --- نرمال‌سازی ورودی ---
  dates_norm <- normalize_date(dates, calendar = from)
  print(paste(k, "- dates_norm:", paste(dates_norm, collapse=", "))); k <- k + 1
  
  # --- جدا کردن تاریخ و زمان ---
  dt_split <- strsplit(dates_norm, " ")
  date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
  time_only <- vapply(dt_split, function(x) if(length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
  print(paste(k, "- date_only:", paste(date_only, collapse=", "))); k <- k + 1
  print(paste(k, "- time_only:", paste(time_only, collapse=", "))); k <- k + 1
  
  # --- بارگذاری جدول ---
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  print(paste(k, "- jalali_greg_map loaded")); k <- k + 1
  
  # --- آماده‌سازی lookup با data.table ---
  if(requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
    
    dt_map <- data.table(
      jalali_date    = as.character(jalali_greg_map$jalali_date),
      gregorian_date = as.character(jalali_greg_map$gregorian_date)
    )
    dt_input <- data.table(date_only = as.character(date_only))
    
    print(paste(k, "- dt_map and dt_input created")); k <- k + 1
    print(head(dt_map)); print(head(dt_input))
    
    if(from == "jalali") {
      setkey(dt_map, jalali_date)
      dt_join <- dt_map[dt_input, on = c("jalali_date" = "date_only"), nomatch = 0]
      print(paste(k, "- dt_join created for jalali -> gregorian")); k <- k + 1
      print(head(dt_join))
      result_date <- rep(NA_character_, length(date_only))
      idx_match <- !is.na(match(date_only, dt_join$jalali_date))
      result_date[idx_match] <- dt_join$gregorian_date
    } else {
      setkey(dt_map, gregorian_date)
      dt_join <- dt_map[dt_input, on = c("gregorian_date" = "date_only"), nomatch = 0]
      print(paste(k, "- dt_join created for gregorian -> jalali")); k <- k + 1
      print(head(dt_join))
      result_date <- rep(NA_character_, length(date_only))
      idx_match <- !is.na(match(date_only, dt_join$gregorian_date))
      result_date[idx_match] <- dt_join$jalali_date
    }
    
  } else {
    # fallback بدون data.table
    if(from == "jalali") {
      lookup_env <- list2env(setNames(as.list(as.character(jalali_greg_map$gregorian_date)),
                                      as.character(jalali_greg_map$jalali_date)),
                             hash = TRUE, parent = emptyenv())
    } else {
      lookup_env <- list2env(setNames(as.list(as.character(jalali_greg_map$jalali_date)),
                                      as.character(jalali_greg_map$gregorian_date)),
                             hash = TRUE, parent = emptyenv())
    }
    result_date <- vapply(date_only, function(x) {
      val <- lookup_env[[x]]
      if(is.null(val)) NA_character_ else val
    }, character(1))
  }
  
  print(paste(k, "- result_date prepared:", paste(result_date, collapse=", "))); k <- k + 1
  
  # --- چسباندن زمان ---
  result <- ifelse(is.na(time_only) | time_only == "", result_date, paste(result_date, time_only))
  print(paste(k, "- final result:", paste(result, collapse=", "))); k <- k + 1
  
  return(result)
}
