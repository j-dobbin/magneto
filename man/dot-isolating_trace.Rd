% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hiddenFunctions.R
\name{.isolating_trace}
\alias{.isolating_trace}
\title{Isolating Traces}
\usage{
.isolating_trace(imageMatrix, topEnv, bottomEnv)
}
\arguments{
\item{imageMatrix}{The processed Image matrix with import_process_image()}

\item{topEnv}{Upper envelope for one trace (scaled to your matrix correctly)}

\item{bottomEnv}{Lower envelope for one trace (scaled to your matrix correctly)}
}
\value{
matrix of the one trace on black(NOTE: still in origional spot on plot)
}
\description{
Takes two envelopes and sets all other pixels to 0 (black)
}
\details{
NOTE: topEnv is less then bottomEnv because indexing 0 is at top of image for matrix scaled
}
