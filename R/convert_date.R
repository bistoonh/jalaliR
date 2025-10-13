convert_date <- function(dates, from = "jalali") {
  cat("شروع تابع\n")
  
  to <- if (from == "jalali") "gregorian" else if (from == "gregorian") "jalali" else stop("from must be 'jalali' or 'gregorian'")
  cat("مقصد تعیین شد:", to, "\n")
  
  dates_norm <- normalize_date(dates, calendar = from)
  cat("تاریخ نرمال شد:", dates_norm, "\n")
  
  dt_split <- strsplit(dates_norm, " ")
  date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
  time_only <- vapply(dt_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
  cat("تاریخ و زمان جدا شدند\n")
  
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  cat("جدول بارگذاری شد\n")
  
  if (requireNamespace("data.table", quietly = TRUE)) {
    cat("data.table موجود است\n")
    
    dt_map <- data.table::data.table(jalali_date = jalali_greg_map$jalali_date,
                                     gregorian_date = jalali_greg_map$gregorian_date)
    cat("جدول تبدیل ساخته شد\n")
    
    dt_input <- data.table::data.table(date_only = date_only)
    cat("ورودی تبدیل به data.table شد\n")
    
    # این خط مشکل‌دار است، پس با cat بررسی می‌کنیم
    cat("در حال افزودن ایندکس...\n")
    data.table::set(dt_input, j = "idx", value = seq_len(nrow(dt_input)))
    cat("ایندکس اضافه شد\n")
    
    if (from == "jalali") {
      data.table::setkey(dt_map, jalali_date)
      dt_input[, converted := dt_map[.SD, gregorian_date, on = c(jalali_date = "date_only")]]
    } else {
      data.table::setkey(dt_map, gregorian_date)
      dt_input[, converted := dt_map[.SD, jalali_date, on = c(gregorian_date = "date_only")]]
    }
    
    result_date <- dt_input$converted
    cat("تبدیل انجام شد\n")
    
  } else {
    cat("data.table موجود نیست، استفاده از list2env\n")
    
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
    
    cat("تبدیل با list2env انجام شد\n")
  }
  
  result <- ifelse(is.na(time_only) | time_only == "", result_date, paste(result_date, time_only))
  cat("خروجی نهایی ساخته شد\n")
  
  return(result)
}
