% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_ggswim.R
\name{drop_missing_override_data}
\alias{drop_missing_override_data}
\title{Check for missing data}
\usage{
drop_missing_override_data(override)
}
\arguments{
\item{override}{the \code{override} list object, responsible for powering \code{guides()}}
}
\value{
the \code{override} list object, responsible for powering \code{guides()}.
Corrected for missing data if any exists.
}
\description{
Internal checking function responsible for alerting the user to missing data
present in the \code{colour} aesthetic layer. ggswim does not allow for this
and will automatically correct it by dropping missing data.
}
\keyword{internal}
