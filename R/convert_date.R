convert_date_debug <- function(dates, from = "jalali") {
  k <- 1
  print(k); k <- k + 1
  
  # تشخیص مقصد
  to <- if(from == "jalali") "gregorian" else if(from == "gregorian") "jalali" else stop("from must be 'jalali' or 'gregorian'")
  print(k); k <- k + 1
  
  # نرمال‌سازی ورودی
  dates_norm <- normalize_date(dates, calendar = from)
  print(k); k <- k + 1
  print(dates_norm)
  
  # جدا کردن تاریخ و زمان
  dt_split <- strsplit(dates_norm, " ")
  date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
  time_only <- vapply(dt_split, function(x) if(length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
  print(k); k <- k + 1
  print(date_only)
  
  # بارگذاری جدول
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  print(k); k <- k + 1
  str(jalali_greg_map)
  
  # ساخت دیتافریم امن
  jalali_greg_map2 <- data.frame(
    jalali_date    = as.character(unlist(jalali_greg_map$jalali_date)),
    gregorian_date = as.character(unlist(jalali_greg_map$gregorian_date)),
    stringsAsFactors = FALSE
  )
  print(k); k <- k + 1
  str(jalali_greg_map2)
  
  # آماده‌سازی lookup
  lookup_env <- list2env(setNames(as.list(jalali_greg_map2$gregorian_date),
                                  jalali_greg_map2$jalali_date), hash=TRUE, parent=emptyenv())
  print(k); k <- k + 1
  
  result_date <- vapply(date_only, function(x) {
    val <- lookup_env[[x]]
    if(is.null(val)) NA_character_ else val
  }, character(1))
  print(k); k <- k + 1
  print(result_date)
  
  # چسباندن زمان
  result <- ifelse(is.na(time_only) | time_only == "", result_date, paste(result_date, time_only))
  print(k); k <- k + 1
  print(result)
  
  return(result)
}
