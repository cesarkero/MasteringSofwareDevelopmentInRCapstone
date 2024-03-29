% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{eq_location_clean}
\usage{
eq_location_clean(df)
}
\arguments{
\item{df}{data.frame from eq_clean_data()}
}
\value{
df object with the cleaned data
}
\description{
In addition, write a function eq_location_clean() that cleans the LOCATION_NAME
column by stripping out the country name (including the colon) and converts names to title case (as opposed to all caps)
}
\examples{
\dontrun{
dfclean <- eq_location_clean(df)
}

}
