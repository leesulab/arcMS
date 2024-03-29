% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_drift_time.R
\name{add_drift_time}
\alias{add_drift_time}
\title{Add Drift Time to Unnested Spectral Data}
\usage{
add_drift_time(connection_params, unnestdt, sample_id)
}
\arguments{
\item{connection_params}{Connection parameters to the UNIFI API - url and token}

\item{unnestdt}{An unnested data table that contains at least the columns 'bin'.}

\item{sample_id}{The sample ID to be used in the API call.}
}
\value{
The unnested data table with an additional column 'dt' for drift time.
}
\description{
This function takes an unnested data table with spectral data and adds drift time
}
\keyword{internal}
