% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoms.R
\docType{data}
\name{eqpoints}
\alias{eqpoints}
\title{eqpoints}
\format{
An object of class \code{eqpoints} (inherits from \code{Geom}, \code{ggproto}, \code{gg}) of length 4.
}
\usage{
eqpoints
}
\arguments{
\item{Geom}{geom for ggplot}

\item{required_aes}{required_aes}

\item{default_aes}{default_aes}

\item{drawn_key}{drawn_key (default)}

\item{draw_panel}{draw_panel (default)}
}
\value{
default function for ggplot representation (time series of earthquakes)
}
\description{
This functions is the geom config form a geom_timeline function, where a timeline
of a selected earthquakes (by period and country) are shown
}
\details{
eqpoints
}
\examples{
\dontrun{
df <- eq_clean_data(NOAArawData='./data_raw/earthquakes-2021-10-06_09-24-38_+0200.tsv')
}
}
\keyword{datasets}
