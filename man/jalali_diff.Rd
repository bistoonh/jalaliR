\name{jalali_diff}
\alias{jalali_diff}
\title{Compute Day Differences Between Jalali Dates}
\description{
Compute the difference in days between two Jalali dates using the pre-built mapping table.
Input dates are normalized with \code{normalize_date()} to ensure consistency.
}
\usage{
jalali_diff(start_date, end_date)
}
\arguments{
  \item{start_date}{Character vector of start dates in Jalali calendar.}
  \item{end_date}{Character vector of end dates in Jalali calendar.}
}
\value{
Integer vector of day differences (end_date - start_date).
}
\examples{
# Single difference
jalali_diff("1402-01-01", "1402-01-03")

# Vectorized differences
jalali_diff(c("1402-01-01","1402-01-05"), 
            c("1402-01-03","1402/1/10"))
}
