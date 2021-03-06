% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palettes.R
\name{showPals}
\alias{showPals}
\title{Demonstrate selected palettes}
\usage{
showPals(pal = NULL, numbers = T)
}
\arguments{
\item{pal}{Either a character vector of colors or a list of character
vectors}

\item{numbers}{On each of the colors, show a number}
}
\description{
Show a plot demonstrating all colors in the provided palettes
}
\examples{
## Show all palettes in plotwidget
showPals(plotPals())

## Show just a few colors
showPals(c("red", "green", "blue"))
}

