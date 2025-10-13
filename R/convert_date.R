convert_date <- function(dates, from = "jalali") {
  to <- if (from == "jalali") "gregorian" else if (from == "gregorian") "jalali" else stop("from must be 'jalali' or 'gregorian'")
  
  dates_norm <- normalize_date(dates, calendar = from)
  dt_split <- strsplit(dates_norm, " ")
  date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
  time_only <- vapply(dt_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
  
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt_map <- data.table::data.table(jalali_date = jalali_greg_map$jalali_date,
                                     gregorian_date = jalali_greg_map$gregorian_date)
    dt_input <- data.table::data.table(date_only = date_only)
    
    if (from == "jalali") {
      data.table::setkey(dt_map, jalali_date)
      converted_vals <- dt_map[dt_input, on = c(jalali_date = "date_only")][["gregorian_date"]]
    } else {
      data.table::setkey(dt_map, gregorian_date)
      converted_vals <- dt_map[dt_input, on = c(gregorian_date = "date_only")][["jalali_date"]]
    }
    
    result_date <- converted_vals
  } else {
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
  
  result <- ifelse(is.na(time_only) | time_only == "", result_date, paste(result_date, time_only))
  return(result)
}
