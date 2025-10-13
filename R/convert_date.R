convert_date <- function(dates, from = "jalali") {
  cat("1 - start function\n")
  
  # --- تشخیص مقصد ---
  to <- if(from == "jalali") "gregorian" else if(from == "gregorian") "jalali" else stop("from must be 'jalali' or 'gregorian'")
  cat("2 - destination:", to, "\n")
  
  # --- نرمال‌سازی ورودی ---
  dates_norm <- normalize_date(dates, calendar = from)
  cat("3 - dates_norm:", paste(dates_norm, collapse = ", "), "\n")
  
  # --- جدا کردن تاریخ و زمان ---
  dt_split <- strsplit(dates_norm, " ")
  date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
  time_only <- vapply(dt_split, function(x) if(length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
  cat("4 - date_only:", paste(date_only, collapse = ", "), "\n")
  cat("5 - time_only:", paste(time_only, collapse = ", "), "\n")
  
  # --- بارگذاری جدول ---
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  cat("6 - jalali_greg_map loaded\n")
  
  # --- اطمینان از اینکه ستون‌ها character هستند ---
  jalali_greg_map2 <- data.frame(
    jalali_date    = as.character(jalali_greg_map$jalali_date),
    gregorian_date = as.character(jalali_greg_map$gregorian_date),
    stringsAsFactors = FALSE
  )
  cat("7 - jalali_greg_map2 created\n")
  
  # --- آماده‌سازی lookup ---
  if(requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
    
    if(from == "jalali") {
      dt_map <- data.table(
        jalali_date = jalali_greg_map2$jalali_date,
        gregorian_date = jalali_greg_map2$gregorian_date
      )
      setkey(dt_map, jalali_date)
      dt_input <- data.table(date_only = date_only)
      dt_join <- dt_map[dt_input, on = c("jalali_date" = "date_only"), nomatch = 0]
      cat("8 - dt_map and dt_input created\n")
      
      result_date <- rep(NA_character_, length(date_only))
      idx_match <- !is.na(match(date_only, dt_join$jalali_date))
      result_date[idx_match] <- dt_join$gregorian_date
    } else {
      dt_map <- data.table(
        jalali_date = jalali_greg_map2$jalali_date,
        gregorian_date = jalali_greg_map2$gregorian_date
      )
      setkey(dt_map, gregorian_date)
      dt_input <- data.table(date_only = date_only)
      dt_join <- dt_map[dt_input, on = c("gregorian_date" = "date_only"), nomatch = 0]
      cat("8 - dt_map and dt_input created\n")
      
      result_date <- rep(NA_character_, length(date_only))
      idx_match <- !is.na(match(date_only, dt_join$gregorian_date))
      result_date[idx_match] <- dt_join$jalali_date
    }
    
  } else {
    # fallback environment/hash
    if(from == "jalali") {
      lookup_env <- list2env(
        setNames(as.list(jalali_greg_map2$gregorian_date),
                 jalali_greg_map2$jalali_date),
        hash = TRUE, parent = emptyenv()
      )
    } else {
      lookup_env <- list2env(
        setNames(as.list(jalali_greg_map2$jalali_date),
                 jalali_greg_map2$gregorian_date),
        hash = TRUE, parent = emptyenv()
      )
    }
    
    result_date <- vapply(date_only, function(x) {
      val <- lookup_env[[x]]
      if(is.null(val)) NA_character_ else val
    }, character(1))
  }
  
  # --- چسباندن زمان ---
  result <- ifelse(is.na(time_only) | time_only == "", result_date, paste(result_date, time_only))
  
  return(result)
}
