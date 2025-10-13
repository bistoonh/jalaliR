convert_date <- function(dates, from = "jalali") {
  k <- 1
  print(paste("Step", k)); k <- k + 1
  
  # --- تشخیص مقصد ---
  to <- if(from == "jalali") "gregorian" else if(from == "gregorian") "jalali" else stop("from must be 'jalali' or 'gregorian'")
  print(paste("Step", k, "- to:", to)); k <- k + 1
  
  # --- نرمال‌سازی ورودی ---
  dates_norm <- normalize_date(dates, calendar = from)
  print(paste("Step", k, "- dates_norm:")); print(dates_norm); k <- k + 1
  
  # --- جدا کردن تاریخ و زمان ---
  dt_split <- strsplit(dates_norm, " ")
  date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
  time_only <- vapply(dt_split, function(x) if(length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
  print(paste("Step", k, "- date_only:")); print(date_only); k <- k + 1
  print(paste("Step", k, "- time_only:")); print(time_only); k <- k + 1
  
  # --- بارگذاری جدول ---
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  print(paste("Step", k, "- jalali_greg_map loaded")); k <- k + 1
  str(jalali_greg_map)
  
  # --- آماده‌سازی lookup ---
  if(requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
    print(paste("Step", k, "- Using data.table")); k <- k + 1
    if(from=="jalali") {
      dt_map <- data.table(jalali_date = jalali_greg_map$jalali_date,
                           gregorian_date = jalali_greg_map$gregorian_date)
      setkey(dt_map, jalali_date)
      dt_input <- data.table(date_only = date_only)
      print(paste("Step", k, "- dt_map and dt_input created")); k <- k + 1
      dt_join <- dt_map[dt_input, on = c("jalali_date"="date_only"), nomatch=0]
      print(paste("Step", k, "- dt_join:")); str(dt_join); k <- k + 1
      
      # نتیجه
      result_date <- rep(NA_character_, length(date_only))
      idx_match <- !is.na(match(date_only, dt_join$jalali_date))
      result_date[idx_match] <- dt_join$gregorian_date
    } else {
      dt_map <- data.table(jalali_date = jalali_greg_map$jalali_date,
                           gregorian_date = jalali_greg_map$gregorian_date)
      setkey(dt_map, gregorian_date)
      dt_input <- data.table(date_only = date_only)
      dt_join <- dt_map[dt_input, on = c("gregorian_date"="date_only"), nomatch=0]
      result_date <- rep(NA_character_, length(date_only))
      idx_match <- !is.na(match(date_only, dt_join$gregorian_date))
      result_date[idx_match] <- dt_join$jalali_date
    }
  } else {
    print(paste("Step", k, "- Using fallback list2env")); k <- k + 1
    if(from=="jalali") {
      lookup_env <- list2env(setNames(as.list(jalali_greg_map$gregorian_date),
                                      jalali_greg_map$jalali_date), hash=TRUE, parent=emptyenv())
    } else {
      lookup_env <- list2env(setNames(as.list(jalali_greg_map$jalali_date),
                                      jalali_greg_map$gregorian_date), hash=TRUE, parent=emptyenv())
    }
    result_date <- vapply(date_only, function(x) {
      val <- lookup_env[[x]]
      if(is.null(val)) NA_character_ else val
    }, character(1))
  }
  
  print(paste("Step", k, "- result_date:")); print(result_date); k <- k + 1
  
  # --- چسباندن زمان ---
  result <- ifelse(is.na(time_only) | time_only=="" , result_date, paste(result_date, time_only))
  print(paste("Step", k, "- final result:")); print(result); k <- k + 1
  
  return(result)
}
