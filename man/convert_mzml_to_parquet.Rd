% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mzml_to_parquet.R
\name{convert_mzml_to_parquet}
\alias{convert_mzml_to_parquet}
\title{Convert mzML (or mzXML) file to parquet file}
\usage{
convert_mzml_to_parquet(filename, chunksize = 10000, path = ".", overwrite = T)
}
\arguments{
\item{filename}{Path name of the netCDF, mzXML or mzML file to read/write.}

\item{chunksize}{size (number of scans) for each chunk to be retrieved from the mzML file.}

\item{path}{OPTIONAL The destination path for the exported file. Default is current working directory.}

\item{overwrite}{OPTIONAL overwrite the sample if already present on disk}
}
\description{
The function collects spectral data and metadata from an mzML file with mzR package.
Data are transformed to data.table, passed in a sample_dataset object and saved as parquet file.
}
\seealso{
\code{\link{save_one_sample_data}} to save collected/converted data from the R environment to Parquet or HDF5 files.
}
