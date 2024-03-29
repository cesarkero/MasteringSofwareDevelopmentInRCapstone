% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoms.R
\name{geom_eqloc}
\alias{geom_eqloc}
\title{geom_eqloc}
\usage{
geom_eqloc(
  mapping = NULL,
  data = NULL,
  na.rm = TRUE,
  show.legend = NA,
  stat = "identity",
  position = "identity",
  inherit.aes = TRUE,
  ...
)
}
\arguments{
\item{mapping}{mapping}

\item{data}{data}

\item{na.rm}{na.rm}

\item{show.legend}{show.legend}

\item{stat}{stat}

\item{position}{position}

\item{inherit.aes}{inherit.aes}
}
\value{
ggplot with lines and locations of the main earth quackes represented with geom_timeline()
}
\description{
This functions adds lines with the locations to the graph created with geom_timeline() function
}
\details{
geom_eqloc
}
\examples{
\dontrun{
p2 <- p1 + geom_eqloc(aes(x = Date, y = COUNTRY, locations = LOCATION_NAME, number = 3))
p2
}
}
