convert_date <- function(dates, from = "jalali") {
  
  to <- if (from == "jalali") "gregorian" else if (from == "gregorian") "jalali" else stop("from must be 'jalali' or 'gregorian'")
  
  dates_norm <- normalize_date(dates, calendar = from)
  
  dt_split <- strsplit(dates_norm, " ")
  date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
  time_only <- vapply(dt_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
  
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  
  # 👇 اصلاح ساختار در صورت لود تودرتو
  if ("jalali_greg_map" %in% names(jalali_greg_map))
    jalali_greg_map <- jalali_greg_map$jalali_greg_map
  
  if (requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
    if (from == "jalali") {
      dt_map <- data.table(
        jalali_date = jalali_greg_map$jalali_date,
        gregorian_date = jalali_greg_map$gregorian_date
      )
      setkey(dt_map, jalali_date)
      dt_input <- data.table(date_only = date_only)
      dt_join <- dt_map[dt_input, on = c("jalali_date" = "date_only"), nomatch = 0]
      result_date <- rep(NA_character_, length(date_only))
      idx_match <- match(date_only, dt_join$jalali_date)
      result_date[!is.na(idx_match)] <- dt_join$gregorian_date[na.omit(idx_match)]
    } else {
      dt_map <- data.table(
        jalali_date = jalali_greg_map$jalali_date,
        gregorian_date = jalali_greg_map$gregorian_date
      )
      setkey(dt_map, gregorian_date)
      dt_input <- data.table(date_only = date_only)
      dt_join <- dt_map[dt_input, on = c("gregorian_date" = "date_only"), nomatch = 0]
      result_date <- rep(NA_character_, length(date_only))
      idx_match <- match(date_only, dt_join$gregorian_date)
      result_date[!is.na(idx_match)] <- dt_join$jalali_date[na.omit(idx_match)]
    }
  } else {
    if (from == "jalali") {
      lookup_env <- list2env(setNames(as.list(jalali_greg_map$gregorian_date), jalali_greg_map$jalali_date),
                             hash = TRUE, parent = emptyenv())
    } else {
      lookup_env <- list2env(setNames(as.list(jalali_greg_map$jalali_date), jalali_greg_map$gregorian_date),
                             hash = TRUE, parent = emptyenv())
    }
    result_date <- vapply(date_only, function(x) {
      val <- lookup_env[[x]]
      if (is.null(val)) NA_character_ else val
    }, character(1))
  }
  
  result <- ifelse(is.na(time_only) | time_only == "", result_date, paste(result_date, time_only))
  
  return(result)
}
