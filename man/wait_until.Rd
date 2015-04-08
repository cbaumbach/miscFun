\name{wait_until}
\alias{wait_until}
\docType{package}
\title{
  Suspend execution until deadline
}
\description{
  Suspend execution until deadline
}
\usage{
wait_until(date_string)
}
\arguments{
  \item{date_string}{
    String specifying when to continue execution.
  }
}
\details{
  The date string must have the format "%d-%m-%y %H:%M", basically
  `day-month-year hour:minute'.  For more details see
  \code{\link[base]{strptime}}.

  It is an error to specify a date string that designates a time point
  that, at the moment \code{wait_until} is called, lies in the past.

  The accuracy of \code{wait_until} is +/- 1 minute.
}
\value{
  Suspends the execution until the deadline designated by `date_string'
  is reached.
}
\examples{
library(miscFun)

\dontrun{
wait_until("26-4-34 17:03")
}
}
