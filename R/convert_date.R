convert_date <- function(dates, from = "jalali") {
  suppressWarnings({  # Suppress warnings from parsing or matching
    
    # Determine target calendar
    to <- if (from == "jalali") "gregorian"
    else if (from == "gregorian") "jalali"
    else stop("from must be 'jalali' or 'gregorian'")
    
    # Normalize input dates (standardize format and validate)
    dates_norm <- normalize_date(dates, calendar = from)
    
    # Split into date and time components
    dt_split <- strsplit(dates_norm, " ")
    date_only <- vapply(dt_split, `[`, 1, FUN.VALUE = character(1))
    time_only <- vapply(dt_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
    
    # Load lookup table from package data
    data("jalali_greg_map", package = "jalaliR", envir = environment())
    
    # Use data.table for fast matching if available
    if (requireNamespace("data.table", quietly = TRUE)) {
      dt_map <- data.table::data.table(
        jalali_date = jalali_greg_map$jalali_date,
        gregorian_date = jalali_greg_map$gregorian_date
      )
      
      # Match and convert using precomputed mapping
      if (from == "jalali") {
        idx <- match(date_only, dt_map$jalali_date)
        result_date <- dt_map$gregorian_date[idx]
      } else {
        idx <- match(date_only, dt_map$gregorian_date)
        result_date <- dt_map$jalali_date[idx]
      }
      
    } else {
      # Fallback: use environment-based hash lookup (faster than join for small data)
      if (from == "jalali") {
        lookup_env <- list2env(
          setNames(as.list(jalali_greg_map$gregorian_date), jalali_greg_map$jalali_date),
          hash = TRUE, parent = emptyenv()
        )
      } else {
        lookup_env <- list2env(
          setNames(as.list(jalali_greg_map$jalali_date), jalali_greg_map$gregorian_date),
          hash = TRUE, parent = emptyenv()
        )
      }
      
      # Lookup each date individually
      result_date <- vapply(date_only, function(x) {
        val <- lookup_env[[x]]
        if (is.null(val)) NA_character_ else val
      }, character(1))
    }
    
    # Reattach time component if present
    result <- ifelse(is.na(time_only) | time_only == "", result_date, paste(result_date, time_only))
    
    return(result)
  })
}
