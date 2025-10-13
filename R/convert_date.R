convert_date <- function(dates, from = "jalali") {
  
  to <- if (from == "jalali") "gregorian" else if (from == "gregorian") "jalali" else stop("from must be 'jalali' or 'gregorian'")
  
  dates_norm <- normalize_date(dates, calendar = from)
  
  dt_split <- strsplit(dates_norm, " ")
  date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
  time_only <- vapply(dt_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
  
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  

  if(requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
    if(from=="jalali") {
      dt_map <- data.table(jalali_date = jalali_greg_map$jalali_date,
                           gregorian_date = jalali_greg_map$gregorian_date)
      setkey(dt_map, jalali_date)
      dt_input <- data.table(date_only = date_only)
      dt_join <- dt_map[dt_input, on = c("jalali_date"="date_only"), nomatch=NA]
      
      # اجباری: تبدیل خروجی جوین به character
      dt_join <- dt_join[, lapply(.SD, as.character)]
      
      result_date <- rep(NA_character_, length(date_only))
      idx_match <- match(date_only, dt_join$jalali_date)
      result_date[!is.na(idx_match)] <- dt_join$gregorian_date[!is.na(idx_match)]
      
    } else {
      dt_map <- data.table(jalali_date = jalali_greg_map$jalali_date,
                           gregorian_date = jalali_greg_map$gregorian_date)
      setkey(dt_map, gregorian_date)
      dt_input <- data.table(date_only = date_only)
      dt_join <- dt_map[dt_input, on = c("gregorian_date"="date_only"), nomatch=NA]
      
      # اجباری: تبدیل خروجی جوین به character
      dt_join <- dt_join[, lapply(.SD, as.character)]
      
      result_date <- rep(NA_character_, length(date_only))
      idx_match <- match(date_only, dt_join$gregorian_date)
      result_date[!is.na(idx_match)] <- dt_join$jalali_date[!is.na(idx_match)]
    }
  }
  
  
  result <- ifelse(is.na(time_only) | time_only == "", result_date, paste(result_date, time_only))
  
  return(result)
}
