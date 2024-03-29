% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/os_checks.R
\name{os}
\alias{os}
\alias{get_os}
\alias{get_R_version}
\alias{get_R_version_age}
\alias{get_latest_CRAN_version}
\alias{get_system_python}
\alias{is.os_mac}
\alias{is.os_win}
\alias{is.os_lnx}
\alias{is.os_unx}
\alias{is.os_x64}
\alias{is.os_arm}
\alias{is.R_x64}
\alias{is.R_revo}
\alias{is.RStudio}
\alias{is.http_available}
\title{Operating system checks}
\usage{
get_os()

get_R_version()

get_R_version_age(units = c("years", "months", "weeks", "days"), rounding = 2)

get_latest_CRAN_version()

get_system_python()

is.os_mac()

is.os_win()

is.os_lnx()

is.os_unx()

is.os_x64()

is.os_arm()

is.R_x64()

is.R_revo()

is.RStudio()

is.http_available()
}
\arguments{
\item{units}{character - how do you want to display the age? e.g. years or months?}

\item{rounding}{integer - how many decimal points do you want to see. e.g. 0.25 years}
}
\description{
Determine the current operating system as well as provide flags to indicate
whether the operating system is a Mac/Windows/Linux.
}
\examples{
# determine operating system
get_os()

# do we have a particular operating system
is.os_mac()
is.os_win()
is.os_lnx()
is.os_unx()

}
\author{
Ben Wiseman, \email{benjamin.wiseman@kornferry.com}

Steven Nydick, \email{steven.nydick@kornferry.com}
}
