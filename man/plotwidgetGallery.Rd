% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gallery.R
\name{plotwidgetGallery}
\alias{plotwidgetGallery}
\title{A showcase of all plot types available in plotwidgets}
\usage{
plotwidgetGallery(theme = "default", pal = NULL, ...)
}
\arguments{
\item{theme}{sets both the palette and a suitable bakckground (e.g. dark for "neon")}

\item{pal}{color palette to use (a character vector with colors)}

\item{...}{all subsequent parameters will be passed to the plotWidget() function.}
}
\value{
Invisibly returns the example data used to generate the plots
}
\description{
A showcase of all plot types available in plotwidgets
}
\details{
plotwidgetGallery() simply draws all available plot widgets on a single
plot.

\if{html}{\figure{gallery.svg}{options: width=400 alt="Gallery example"}}
\if{latex}{\figure{gallery.pdf}{options: width=5in}}
}
\examples{
plotwidgetGallery()
## automatically set black bg
plotwidgetGallery(theme="neon") 
## yuck, ugly:
plotwidgetGallery(pal=c("red", "#FF9900", "blue", "green", "cyan", "yellow"))
## much better:
plotwidgetGallery(pal=plotPals("pastel", alpha=0.8))
}

