% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_swim_label.R
\name{get_mapping_obj}
\alias{get_mapping_obj}
\title{Create mapping object}
\usage{
get_mapping_obj(object_mapping, plot_mapping)
}
\arguments{
\item{object_mapping}{\code{mapping} from the layer object}

\item{plot_mapping}{\code{mapping} from the plot object}
}
\value{
a \code{\link[=aes]{aes()}} frame
}
\description{
To assist with label legend fixing, the mapping object helps
pass mapped data from \code{\link[=aes]{aes()}} down to \code{\link[=get_label_override]{get_label_override()}}, including
in instances when data is inherited from \code{\link[=ggplot]{ggplot()}}.
}
\keyword{internal}
