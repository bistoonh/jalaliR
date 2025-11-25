#' Compute day differences between Jalali dates (NA-safe)
#'
#' @param start_date Character vector of Jalali start dates
#' @param end_date   Character vector of Jalali end dates
#' @return Integer vector of day differences (end_date - start_date), NA if invalid
#' @export
jalali_diff <- function(start_date, end_date) {
  # Check length consistency
  if (length(start_date) != length(end_date)) {
    stop("Lengths of start_date and end_date must be equal.")
  }
  
  # Normalize safely
  safe_normalize <- function(x) {
    if (is.na(x) || x == "") return(NA_character_)
    tryCatch(normalize_date(x, calendar = "jalali"),
             error = function(e) NA_character_)
  }
  
  start_norm <- vapply(start_date, safe_normalize, character(1))
  end_norm   <- vapply(end_date,   safe_normalize, character(1))
  
  # Load mapping
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  idx_map <- setNames(seq_len(nrow(jalali_greg_map)), jalali_greg_map$jalali_date)
  
  # Safe lookup
  fetch_idx <- function(x) {
    if (is.na(x)) return(NA_integer_)
    val <- idx_map[[x]]
    if (is.null(val)) NA_integer_ else as.integer(val)
  }
  
  start_idx <- vapply(start_norm, fetch_idx, integer(1))
  end_idx   <- vapply(end_norm,   fetch_idx, integer(1))
  
  # Differences
  diffs <- ifelse(is.na(start_idx) | is.na(end_idx),
                  NA_integer_,
                  end_idx - start_idx)
  
  return(diffs)
}
