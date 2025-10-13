normalize_date <- function(dates, calendar = "jalali") {
  # --- بارگذاری جدول (از data/ پکیج) ---
  data("jalali_greg_map", package = "jalaliR")
  
  # --- آماده‌سازی داده ---
  dates <- trimws(as.character(dates))
  dates <- gsub("[/\\.]", "-", dates)
  # جدا کردن بخش زمان (اگر باشد)
  time_part <- sub("^[^ ]*(?: (.*))?$", "\\1", dates)
  time_part[time_part == ""] <- NA_character_
  date_part <- sub(" .*", "", dates)
  
  # --- نرمال‌سازی تاریخ (فرمت‌های 8 رقمی، 6 رقمی، yyyy-mm و yyyy-mm-dd) ---
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
  
  # استانداردسازی موارد yyyy-mm -> yyyy-mm-01 و پر کردن صفرها
  idx <- !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_norm)
  if (any(idx)) {
    date_norm[idx] <- sub("^([0-9]{4}-[0-9]{1,2})$", "\\1-01", date_norm[idx])
    parts <- do.call(rbind, strsplit(date_norm[idx], "-"))
    y <- parts[,1]
    m <- sprintf("%02d", as.integer(parts[,2]))
    d <- sprintf("%02d", as.integer(parts[,3]))
    date_norm[idx] <- paste0(y,"-",m,"-",d)
  }
  
  # اطمینان از اینکه date_norm یک وکتور کاراکتری است (نه لیست)
  date_norm <- as.character(date_norm)
  
  # --- اعتبارسنجی تاریخ‌ها با جدول موجود در پکیج ---
  # استفاده از membership ساده برای جلوگیری از مشکلات list-column در join
  date_vec <- if (calendar == "jalali") as.character(jalali_greg_map$jalali_date) else as.character(jalali_greg_map$gregorian_date)
  valid_idx <- date_norm %in% date_vec
  date_norm[!valid_idx] <- NA_character_
  
  # --- پردازش زمان ---
  time_norm <- rep("", length(time_part))
  idx_time <- !is.na(time_part) & !is.na(date_norm)
  if (any(idx_time)) {
    t <- time_part[idx_time]
    # استخراج ساعت:دقیقه:ثانیه با الگوی ساده‌تر و محافظت در مقابل NA
    h <- as.integer(sub("^([0-9]{1,2}).*", "\\1", t))
    m <- as.integer(sub("^[0-9]{1,2}:([0-9]{1,2}).*", "\\1", t))
    s <- as.integer(sub("^[0-9]{1,2}:[0-9]{1,2}:?([0-9]{1,2})?.*", "\\1", t))
    m[is.na(m)] <- 0
    s[is.na(s)] <- 0
    invalid <- is.na(h) | h > 23 | m > 59 | s > 59
    h[invalid] <- m[invalid] <- s[invalid] <- NA
    time_norm[idx_time] <- ifelse(invalid, NA_character_, sprintf("%02d:%02d:%02d", h, m, s))
  }
  
  # --- ترکیب تاریخ و زمان ---
  result <- ifelse(time_norm == "", date_norm, paste(date_norm, time_norm))
  return(result)
}
