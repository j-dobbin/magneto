% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/userFunctions.R
\name{intersection_check}
\alias{intersection_check}
\title{Intersection Check}
\usage{
intersection_check(topEnv, bottomEnv, imageName, rmAmount = 300)
}
\arguments{
\item{topEnv}{The top envelope of the comparison}

\item{bottomEnv}{The bottom envelope of the comparison}

\item{imageName}{Name of the image for the warning if cross}

\item{rmAmount}{The amount ignored from both the right and left sides
of the image where possible trace lines could intersect because of noise
This is to ensure that false intersections aren't found between the two traces}
}
\value{
warning if there is an intersection
}
\description{
Takes to envelope lines and checks to see if the lower is higher then the
upper
}
