#' Compute day differences between Jalali dates (NA-safe)
#'
#' Returns NA when either input is NA or not found in the lookup table.
#'
#' @param start_date Character vector of Jalali start dates
#' @param end_date   Character vector of Jalali end dates
#' @return Integer vector of day differences (end - start), NA if invalid
#' @export
jalali_diff <- function(start_date, end_date) {
  # Normalize inputs; keep NA if normalization fails
  start_norm <- tryCatch(normalize_date(start_date, calendar = "jalali"),
                         error = function(e) rep(NA_character_, length(start_date)))
  end_norm   <- tryCatch(normalize_date(end_date,   calendar = "jalali"),
                         error = function(e) rep(NA_character_, length(end_date)))

  # Load mapping
  data("jalali_greg_map", package = "jalaliR", envir = environment())

  # Precompute indices
  idx_vec <- seq_len(nrow(jalali_greg_map))
  dates   <- jalali_greg_map$jalali_date

  # Fast path with data.table if available
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt_map <- data.table::data.table(jalali_date = dates, idx = idx_vec)
    start_idx <- dt_map$idx[match(start_norm, dt_map$jalali_date)]
    end_idx   <- dt_map$idx[match(end_norm,   dt_map$jalali_date)]
  } else {
    # Hash lookup; returns NULL for missing keys, convert to NA_integer_
    lookup_env <- list2env(setNames(as.list(idx_vec), dates), hash = TRUE, parent = emptyenv())
    fetch_idx <- function(x) {
      if (is.na(x)) return(NA_integer_)
      val <- lookup_env[[x]]
      if (is.null(val)) NA_integer_ else as.integer(val)
    }
    start_idx <- vapply(start_norm, fetch_idx, integer(1))
    end_idx   <- vapply(end_norm,   fetch_idx, integer(1))
  }

  # Compute differences; NA propagates naturally
  diffs <- end_idx - start_idx

  # Ensure integer type
  as.integer(diffs)
}
