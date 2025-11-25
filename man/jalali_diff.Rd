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
  \item{start_date}{Character vector of Jalali start dates.}
  \item{end_date}{Character vector of Jalali end dates.}
}
\value{
Integer vector of day differences (end_date - start_date). Returns NA if either input is invalid or NA.
}
\examples{
jalali_diff("1402-01-01", "1402-01-03")
jalali_diff(c("1402-01-01","1402-01-05"),
            c("1402-01-03","1402-01-10"))
jalali_diff(c("1402-01-01","1402-01-05"),
            c("1402-01-03", NA))
}
