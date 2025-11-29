\name{jalali_time_diff}
\alias{jalali_time_diff}
\title{Compute Date-Time Differences Between Jalali Dates}
\description{
Compute the difference in days and times between two Jalali date-time values using the pre-built mapping table.
Input dates are normalized with \code{normalize_date()} to ensure consistency.
If one of the inputs has no time component, it is assumed to be \code{00:00:00}.
}
\usage{
jalali_time_diff(start_date, end_date)
}
\arguments{
  \item{start_date}{Character vector of Jalali start dates (with optional time).}
  \item{end_date}{Character vector of Jalali end dates (with optional time).}
}
\value{
A list with two components:
\itemize{
  \item \code{seconds}: Numeric vector of total differences in seconds (end_date - start_date).
  \item \code{formatted}: Character vector of differences formatted as \code{"X days HH:MM:SS"}.
}
Returns NA if either input is invalid or NA.
}
\examples{
# Pure date difference
jalali_time_diff("1402-01-01", "1402-01-03")$formatted

# Date-time difference with hours and minutes
jalali_time_diff("1402-01-01 12:00:00", "1402-01-02 14:30:00")$seconds
jalali_time_diff("1402-01-01 12:00:00", "1402-01-02 14:30:00")$formatted

# Vectorized usage
jalali_time_diff(c("1402-01-01 08:00:00","1402-01-05"),
                 c("1402-01-03 09:15:00","1402-01-10"))$formatted

# Handling NA values
jalali_time_diff(c("1402-01-01","1402-01-05"),
                 c("1402-01-03", NA))$seconds
}
