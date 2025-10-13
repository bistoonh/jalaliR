convert_date <- function(dates, from = "jalali") {
  print("1 - start function")
  
  # --- مقصد ---
  to <- if(from=="jalali") "gregorian" else if(from=="gregorian") "jalali" else stop("from must be 'jalali' or 'gregorian'")
  print(paste("2 - destination:", to))
  
  # --- نرمال‌سازی ---
  dates_norm <- normalize_date(dates, calendar=from)
  print(paste("3 - dates_norm:", paste(dates_norm, collapse=", ")))
  
  # --- تاریخ و زمان ---
  dt_split <- strsplit(dates_norm, " ")
  date_only <- vapply(dt_split, `[`, 1, FUN.VALUE=character(1))
  time_only <- vapply(dt_split, function(x) if(length(x)>1) x[2] else NA_character_, FUN.VALUE=character(1))
  print(paste("4 - date_only:", paste(date_only, collapse=", ")))
  print(paste("5 - time_only:", paste(time_only, collapse=", ")))
  
  # --- بارگذاری جدول ---
  data("jalali_greg_map", package="jalaliR", envir=environment())
  print("6 - jalali_greg_map loaded")
  
  # --- ساخت نسخه امن از جدول ---
  jalali_greg_map2 <- data.frame(
    jalali_date = as.character(jalali_greg_map$jalali_date),
    gregorian_date = as.character(jalali_greg_map$gregorian_date),
    stringsAsFactors = FALSE
  )
  print("7 - jalali_greg_map2 created")
  
  # --- data.table lookup ---
  library(data.table)
  dt_map <- data.table(
    jalali_date = jalali_greg_map2$jalali_date,
    gregorian_date = jalali_greg_map2$gregorian_date
  )
  print("8 - dt_map created")
  
  if(from=="jalali") {
    setkey(dt_map, jalali_date)
    dt_input <- data.table(date_only = as.character(date_only)) # اینجا مطمئن می‌شویم vector است
    dt_join <- dt_map[dt_input, on = c("jalali_date"="date_only"), nomatch=0]
    result_date <- rep(NA_character_, length(date_only))
    idx_match <- match(date_only, dt_join$jalali_date)
    result_date[!is.na(idx_match)] <- dt_join$gregorian_date[!is.na(idx_match)]
  } else {
    setkey(dt_map, gregorian_date)
    dt_input <- data.table(date_only = as.character(date_only))
    dt_join <- dt_map[dt_input, on = c("gregorian_date"="date_only"), nomatch=0]
    result_date <- rep(NA_character_, length(date_only))
    idx_match <- match(date_only, dt_join$gregorian_date)
    result_date[!is.na(idx_match)] <- dt_join$jalali_date[!is.na(idx_match)]
  }
  
  # --- چسباندن زمان ---
  result <- ifelse(is.na(time_only) | time_only=="", result_date, paste(result_date, time_only))
  
  print("9 - finished")
  return(result)
}
