% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hiddenFunctions.R
\name{.top_image_cut}
\alias{.top_image_cut}
\title{Top Image Cut}
\usage{
.top_image_cut(imageMatrix, percentFromEdge, percentEdgeForLeft = NULL)
}
\arguments{
\item{imageMatrix}{Imported image into matrix form, can use tiff_import()}

\item{percentFromEdge}{used in find_peaks if you know there wont be a peak
in a region}

\item{percentEdgeForLeft}{passed into find peaks, if not specified, uses
percentFromEdge for both left and right sides, if specified, percentFromEdge
is defaulted to just the right side of the plot}
}
\value{
the recommended cutoff of the top of your image
}
\description{
returns part of the image that doesn't have any horizontal lines (traces) in,
will discard lettering(main use)
}
