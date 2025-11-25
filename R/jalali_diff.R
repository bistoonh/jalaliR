#' Compute day differences between Jalali dates
#'
#' This function calculates the difference in days between two Jalali dates.
#' It uses the precomputed lookup table `jalali_greg_map` for speed.
#' Input dates are normalized with `normalize_date()` to ensure consistency.
#'
#' @param start_date Character vector of start dates in Jalali calendar
#' @param end_date Character vector of end dates in Jalali calendar
#' @return Integer vector of day differences (end_date - start_date)
#' @examples
#' jalali_diff("1402-01-01", "1402-01-03")
#' jalali_diff(c("1402-01-01","1402-01-05"), c("1402-01-03","1402-01-10"))
#'
#' @export
jalali_diff <- function(start_date, end_date) {
  # Normalize input dates
  start_norm <- normalize_date(start_date, calendar = "jalali")
  end_norm   <- normalize_date(end_date,   calendar = "jalali")
  
  # Load lookup table from package data
  data("jalali_greg_map", package = "jalaliR", envir = environment())
  
  # Use data.table for fast matching if available
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt_map <- data.table::data.table(
      jalali_date = jalali_greg_map$jalali_date,
      idx = seq_len(nrow(jalali_greg_map))
    )
    
    start_idx <- dt_map$idx[match(start_norm, dt_map$jalali_date)]
    end_idx   <- dt_map$idx[match(end_norm,   dt_map$jalali_date)]
    
    diff_days <- end_idx - start_idx
    
  } else {
    # Fallback: environment-based hash lookup
    lookup_env <- list2env(
      setNames(as.list(seq_len(nrow(jalali_greg_map))), jalali_greg_map$jalali_date),
      hash = TRUE, parent = emptyenv()
    )
    
    start_idx <- vapply(start_norm, function(x) {
      val <- lookup_env[[x]]
      if (is.null(val)) NA_integer_ else val
    }, integer(1))
    
    end_idx <- vapply(end_norm, function(x) {
      val <- lookup_env[[x]]
      if (is.null(val)) NA_integer_ else val
    }, integer(1))
    
    diff_days <- end_idx - start_idx
  }
  
  return(as.integer(diff_days))
}
