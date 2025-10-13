normalize_date <- function(dates, calendar = "jalali") {
  # --- Load mapping CSV from package ---
  map_file <- system.file("extdata", "jalali_greg_map.csv", package = "jalaliR")
  if(!file.exists(map_file)) stop("Mapping CSV file not found in package!")
  jalali_greg_map <- read.csv(map_file, stringsAsFactors = FALSE)
  
  # --- Prepare input ---
  dates <- trimws(as.character(dates))
  dates <- gsub("[/\\.]", "-", dates)
  # Split date and time safely
  dt_split <- strsplit(dates, " ", fixed = TRUE)
  date_part <- sapply(dt_split, `[`, 1)
  time_part <- sapply(dt_split, function(x) if(length(x)>1) x[2] else NA_character_)
  
  # --- Normalize date ---
  date_norm <- ifelse(grepl("^\\d{8}$", date_part),
                      sprintf("%04d-%02d-%02d",
                              as.integer(substr(date_part,1,4)),
                              as.integer(substr(date_part,5,6)),
                              as.integer(substr(date_part,7,8))),
                      ifelse(grepl("^\\d{6}$", date_part),
                             sprintf("%04d-%02d-01",
                                     as.integer(substr(date_part,1,4)),
                                     as.integer(substr(date_part,5,6))),
                             date_part))
  
  # --- Fix incomplete dates ---
  idx <- !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_norm)
  if(any(idx)) {
    date_norm[idx] <- sub("^([0-9]{4}-[0-9]{1,2})$", "\\1-01", date_norm[idx])
    parts <- do.call(rbind, strsplit(date_norm[idx], "-", fixed = TRUE))
    y <- parts[,1]
    m <- sprintf("%02d", as.integer(parts[,2]))
    d <- sprintf("%02d", as.integer(parts[,3]))
    date_norm[idx] <- paste0(y,"-",m,"-",d)
  }
  
  # --- Validate using CSV map ---
  valid_dates <- if(calendar=="jalali") jalali_greg_map$jalali_date else jalali_greg_map$gregorian_date
  date_norm[!date_norm %in% valid_dates] <- NA_character_
  
  # --- Normalize time ---
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
    time_norm[idx_time] <- ifelse(invalid, NA_character_, sprintf("%02d:%02d:%02d", h,m,s))
  }
  
  # --- Combine date and time ---
  result <- ifelse(time_norm=="", date_norm, paste(date_norm, time_norm))
  
  return(result)
}
