% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{check_arrow_fill_type}
\alias{check_arrow_fill_type}
\title{check add_arrows for arrow_fill and arrow_type arguments}
\usage{
check_arrow_fill_type(arrow_type, arrow_fill)
}
\arguments{
\item{arrow_type}{One of "open" or "closed" indicating whether the arrow head should
be a closed triangle. Default "closed."}

\item{arrow_fill}{Fill colour to use for the arrow head (if closed). Default \code{NULL}.}
}
\description{
Supply users with a warning when \code{arrow_type} is not "closed" and an \code{arrow_fill}
argument is specified. No error will occur, but nothing will indicate an issue
in the output.
}
\keyword{internal}
