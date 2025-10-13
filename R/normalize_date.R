normalize_date <- function(dates, calendar = "jalali") {
  # --- Load mapping table from the package ---
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  
  # --- Prepare input data ---
  dates <- trimws(as.character(dates))
  dates <- gsub("[/\\.]", "-", dates)
  time_part <- sub("^[^ ]*(?: (.*))?$", "\\1", dates)
  time_part[time_part == ""] <- NA_character_
  date_part <- sub(" .*", "", dates)
  
  # --- Normalize date part ---
  date_norm <- ifelse(
    grepl("^\\d{8}$", date_part),
    sprintf("%04d-%02d-%02d",
            as.integer(substr(date_part, 1, 4)),
            as.integer(substr(date_part, 5, 6)),
            as.integer(substr(date_part, 7, 8))),
    ifelse(
      grepl("^\\d{6}$", date_part),
      sprintf("%04d-%02d-01",
              as.integer(substr(date_part, 1, 4)),
              as.integer(substr(date_part, 5, 6))),
      date_part
    )
  )
  
  # --- Fix incomplete dates ---
  idx <- !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_norm)
  if(any(idx)) {
    date_norm[idx] <- sub("^([0-9]{4}-[0-9]{1,2})$", "\\1-01", date_norm[idx])
    parts <- do.call(rbind, strsplit(date_norm[idx], "-"))
    y <- parts[,1]
    m <- sprintf("%02d", as.integer(parts[,2]))
    d <- sprintf("%02d", as.integer(parts[,3]))
    date_norm[idx] <- paste0(y,"-",m,"-",d)
  }
  
  # --- Validate dates using data.table if installed ---
  if(requireNamespace("data.table", quietly = TRUE)) {
    dt_map <- data.table::data.table(
      date_val = if(calendar == "jalali") jalali_greg_map$jalali_date else jalali_greg_map$gregorian_date
    )
    data.table::setkey(dt_map, date_val)
    dt_input <- data.table::data.table(date_norm = date_norm)
    dt_input <- dt_map[dt_input, on = c("date_val" = "date_norm"), nomatch = 0]
    valid_dates <- dt_input$date_val
    valid_idx <- date_norm %in% valid_dates
    date_norm[!valid_idx] <- NA_character_
  } else {
    # --- fallback: environment/hash ---
    lookup_env <- list2env(
      setNames(
        as.list(if(calendar=="jalali") jalali_greg_map$jalali_date else jalali_greg_map$gregorian_date),
        if(calendar=="jalali") jalali_greg_map$jalali_date else jalali_greg_map$gregorian_date
      ),
      hash = TRUE, parent = emptyenv()
    )
    valid <- vapply(date_norm, function(x) !is.null(lookup_env[[x]]), logical(1))
    date_norm[!valid] <- NA_character_
  }
  
  # --- Normalize time part ---
  time_norm <- rep("", length(time_part))
  idx_time <- !is.na(time_part) & !is.na(date_norm)
  if(any(idx_time)) {
    t <- time_part[idx_time]
    h <- as.integer(sub("^([0-9]{1,2}).*", "\\1", t))
    m <- as.integer(sub("^[0-9]{1,2}:([0-9]{1,2}).*", "\\1", t))
    s <- as.integer(sub("^[0-9]{1,2}:[0-9]{1,2}:?([0-9]{1,2})?.*", "\\1", t))
    m[is.na(m)] <- 0
    s[is.na(s)] <- 0
    invalid <- h>23 | m>59 | s>59 | is.na(h)
    h[invalid] <- m[invalid] <- s[invalid] <- NA
    time_norm[idx_time] <- ifelse(invalid, NA_character_, sprintf("%02d:%02d:%02d", h, m, s))
  }
  
  # --- Combine date and time ---
  result <- ifelse(time_norm == "", date_norm, paste(date_norm, time_norm))
  
  return(result)
}
