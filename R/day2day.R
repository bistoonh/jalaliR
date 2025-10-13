day2day <- function(start_date, end_date, calendar = "jalali") {
  suppressWarnings({
    # Normalize input dates
    start_norm <- normalize_date(start_date, calendar = calendar)
    end_norm   <- normalize_date(end_date, calendar = calendar)
    
    if (any(is.na(start_norm) | is.na(end_norm))) {
      stop("Invalid start or end date after normalization.")
    }
    
    # Split into date and time components
    start_split <- strsplit(start_norm, " ")
    end_split   <- strsplit(end_norm, " ")
    
    start_date_only <- vapply(start_split, `[`, 1, FUN.VALUE = character(1))
    start_time_only <- vapply(start_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
    
    end_date_only <- vapply(end_split, `[`, 1, FUN.VALUE = character(1))
    end_time_only <- vapply(end_split, function(x) if (length(x) > 1) x[2] else NA_character_, FUN.VALUE = character(1))
    
    # Load lookup table
    data("jalali_greg_map", package = "jalaliR")
    
    # Select appropriate date column
    date_vec <- if (calendar == "jalali") jalali_greg_map$jalali_date else jalali_greg_map$gregorian_date
    
    # Find index positions
    idx_start <- match(start_date_only, date_vec)
    idx_end   <- match(end_date_only, date_vec)
    
    if (is.na(idx_start) | is.na(idx_end)) {
      stop("Start or end date not found in map.")
    }
    
    # Generate date sequence
    if (idx_start <= idx_end) {
      result_dates <- date_vec[idx_start:idx_end]
    } else {
      result_dates <- rev(date_vec[idx_end:idx_start])
    }
    
    # Attach time components if present
    if (!all(is.na(start_time_only)) | !all(is.na(end_time_only))) {
      result_times <- rep(NA_character_, length(result_dates))
      result_times[1] <- start_time_only
      result_times[length(result_times)] <- end_time_only
      result <- ifelse(!is.na(result_times), paste(result_dates, result_times), result_dates)
    } else {
      result <- result_dates
    }
    
    return(result)
  })
}
