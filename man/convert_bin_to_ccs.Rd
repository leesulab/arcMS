% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_bin_to_ccs.R
\name{convert_bin_to_ccs}
\alias{convert_bin_to_ccs}
\title{Convert bin value to CCS (Collision Cross Section) value}
\usage{
convert_bin_to_ccs(sample_dataset, connection_params = NULL)
}
\arguments{
\item{sample_dataset}{A sample_dataset object containing the sample data.
This data should include the necessary columns: `bin`, `mz`, and `rt`.
The `id` of the sample should be present in the `sample_metadata` slot of the sample_dataset object.}

\item{connection_params}{OPTIONAL: Connection parameters object created by the
\code{\link{create_connection_params}} function. If not provided, the
\code{\link{get_connection_params}} will look for such an object in the global environment.}
}
\value{
A sample_dataset object including the original data and an additional column `CCS` containing the CCS values.
}
\description{
This function converts bin values to CCS values for the given sample.
This conversion should be performed for a limited number of detected peaks
and not on raw data to avoid too many requests and useless CCS calculations
for all m/z peaks and fragments.
Peak lists can be obtained after peak detection, for example using the DEIMoS Python library.
For more information on DEIMoS, see \url{https://deimos.readthedocs.io/en/latest/}.
}
