% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggcolors.R
\name{ggcolors}
\alias{ggcolors}
\title{ggcolors}
\usage{
ggcolors(n)
}
\arguments{
\item{n}{The number of colors you want (equally spaced along the color wheel)}
}
\value{
A vector of colors is returned.
}
\description{
Get the colors that ggplot2 uses by default. This code is not totally written by
 the author; he adapted it from someplace on the web.
}
\details{
I made this into a function because I was tired of copying it into all my code.
}
\examples{
ggcolors(3)
ggcolors(6) # overlaps the ggcolors(3) call
}
