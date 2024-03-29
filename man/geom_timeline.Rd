% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoms.R
\name{geom_timeline}
\alias{geom_timeline}
\title{geom_timeline}
\usage{
geom_timeline(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{mapping}{mapping}

\item{data}{data}

\item{stat}{stat}

\item{position}{position}

\item{na.rm}{na.rm}

\item{show.legend}{show.legend}

\item{inherit.aes}{inherit.aes}
}
\value{
ggplot representation (time series of earthquakes)
}
\description{
This function allows the user a faster representation of data
based on a predefined layout of a ggplot2
}
\details{
geom_timeline
}
\examples{
\dontrun{
dfs <- df \%>\%
dplyr::filter(Year>2015 & Year<2021 & (COUNTRY =='CHINA' | COUNTRY =='GREECE'))

p1 <- dfs \%>\%
ggplot() +
geom_timeline(aes(x = Date,
                  y = COUNTRY,
                  size = Mag,
                  colour = Deaths)) +
                  theme_minimal()+
                  theme(legend.position="bottom")
                  p1
}
}
