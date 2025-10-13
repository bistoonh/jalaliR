\name{day2day}
\alias{day2day}
\title{Generate a Sequence of Dates}
\description{
Generate all dates between a start and end date in Jalali or Gregorian calendar.
Start and end times are preserved if provided.
}
\usage{
day2day(start_date, end_date, calendar = "jalali")
}
\arguments{
  \item{start_date}{Character. Start date.}
  \item{end_date}{Character. End date.}
  \item{calendar}{Character. Either "jalali" or "gregorian". Default is "jalali".}
}
\value{
Character vector of dates (with times for start and end if present).
}
\examples{
day2day("1402-01-01", "1402-01-03", calendar = "jalali")
day2day("1402-01-01 08:00:00", "1402-01-03 18:00:00", calendar = "jalali")
day2day("2023-03-21", "2023-03-23", calendar = "gregorian")
}
