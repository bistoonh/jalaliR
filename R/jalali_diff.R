#' Compute day differences between Jalali dates (NA-safe, fast)
#'
#' @param start_date Character vector of Jalali start dates
#' @param end_date   Character vector of Jalali end dates
#' @return Integer vector of day differences (end_date - start_date). NA if either input is NA or invalid.
#' @export
jalali_diff <- function(start_date, end_date) {
  # Optional: enforce equal lengths to avoid recycling
  if (length(start_date) != length(end_date)) {
    stop("Lengths of start_date and end_date must be equal.")
  }
  
  # Vectorized safe normalization: skip NA/"" and only normalize valid entries
  safe_norm_vec <- function(x) {
    out <- x
    ok <- !is.na(x) & nzchar(x)
    if (any(ok)) {
      out[ok] <- suppressWarnings(normalize_date(x[ok], calendar = "jalali"))
    }
    out[!ok] <- NA_character_
    out
  }
  start_norm <- safe_norm_vec(start_date)
  end_norm   <- safe_norm_vec(end_date)
  
  # Load lookup table
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  
  # Fast matching
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt_map <- data.table::data.table(
      jalali_date = jalali_greg_map$jalali_date,
      idx = seq_len(nrow(jalali_greg_map))
    )
    start_idx <- dt_map$idx[match(start_norm, dt_map$jalali_date)]
    end_idx   <- dt_map$idx[match(end_norm,   dt_map$jalali_date)]
  } else {
    start_idx <- match(start_norm, jalali_greg_map$jalali_date)
    end_idx   <- match(end_norm,   jalali_greg_map$jalali_date)
  }
  
  # Differences; NA propagates
  as.integer(end_idx - start_idx)
}
