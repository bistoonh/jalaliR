\name{convert_date}
\alias{convert_date}
\title{Convert Dates Between Jalali and Gregorian}
\description{
Convert dates from Jalali to Gregorian or vice versa using the pre-built mapping table.
}
\usage{
convert_date(dates, from = "jalali")
}
\arguments{
  \item{dates}{Character vector of dates.}
  \item{from}{Character. Either "jalali" or "gregorian". Default is "jalali".}
}
\value{
Character vector of converted dates.
}
\examples{
convert_date("1402-01-01", from = "jalali")
convert_date(c("1402-01-01", "1402-05-15"), from = "jalali")
convert_date("2023-03-21", from = "gregorian")
}
