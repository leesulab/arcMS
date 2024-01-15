
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arcMS

<!-- badges: start -->
<!-- badges: end -->

arcMS can convert HDMSe data acquired with Unifi to tabular format
for use in R or Python, with a small filesize when saved on disk.

Two output data file formats can be obtained:

- the [Apache Parquet](https://parquet.apache.org/) format for minimal
  filesize and fast access. Two files are produced: one for MS data, one
  for metadata.

- the [HDF5](https://www.hdfgroup.org/solutions/hdf5/) format with all
  data and metadata in one file, fast access but larger filesize.

## Installation

You can install arcMS in R with the following command:

``` r
install.packages("devtools")
devtools::install_github("leesulab/arcMS")
```

To use the HDF5 format, the `rhdf5` package needs to be installed:

``` r
install.packages("BiocManager")
BiocManager::install("rhdf5")
```

## Usage

First load the package:

``` r
library("arcMS")
```

Then create connection parameters to the Unifi API (retrieve token).

``` r
con = create_connection_params(apihosturl = "http://localhost:50034/unifi/v1", identityurl = "http://localhost:50333/identity/connect/token")
```

If the `arcMS` and `R` session are run from another computer than
where the Unifi API is installed, replace `localhost` by the IP address
of the Unifi API.

``` r
con = create_connection_params(apihosturl = "http://192.0.2.0:50034/unifi/v1", identityurl = "http://192.0.2.0:50333/identity/connect/token")
```

Now these connection parameters will be used to access the Unifi
folders. The following function will show the list of folders and their
IDs (e.g. `abe9c297-821e-4152-854a-17c73c9ff68c` in the example below).

``` r
folders = folders_search()
folders
```

    #>                                     id                name
    #> 3 abe9c297-821e-4152-854a-17c73c9ff68c          Christelle
    #> 4 dde4ecfc-fe08-4cb2-ad8a-c10f3e45f4dd Imports temporaires
    #>                          path folderType                             parentId
    #> 3          Company/Christelle    Project 7c3a0fc7-3805-4c14-ab68-8da3e115702e
    #> 4 Company/Imports temporaires    Project 7c3a0fc7-3805-4c14-ab68-8da3e115702e

With a folder ID, we can access the list of Analysis items in the
folder:

``` r
ana = analysis_search("abe9c297-821e-4152-854a-17c73c9ff68c")
ana
```

Finally, with an Analysis ID, we can get the list of samples
(injections) acquired in this Analysis:

``` r
samples = get_samples_list("e236bf99-31cd-44ae-a4e7-74915697df65")
samples
```

Once we get a sample ID, we can use it to download the sample data:

``` r
convert_one_sample_data(sample_id = "0134efbf-c75a-411b-842a-4f35e2b76347")
```

This command will get the sample name (`sample_name`) and its parent
analysis (`analysis_name`), create a folder named `analysis_name` in the
working directory and save the sample data with the name
`sample_name.parquet` and its metadata with the name
`sample_name-metadata.parquet`.

With an Analysis ID, we can convert and save all samples from the chosen
Analysis:

``` r
convert_all_samples_data(analysis_id = "e236bf99-31cd-44ae-a4e7-74915697df65")
```

To use the HDF5 format instead of Parquet, the format argument can be
used as below:

``` r
convert_one_sample_data(sample_id = "0134efbf-c75a-411b-842a-4f35e2b76347", format = "hdf5")

convert_all_samples_data(analysis_id = "e236bf99-31cd-44ae-a4e7-74915697df65", format = "hdf5")
```

This will save the samples data and metadata in the same `file.h5` file.

Parquet or HDF5 files can be opened easily in `R` with the `arrow` or
`rhdf5` packages. Parquet files contain both low and high energy spectra
(HDMSe), and HDF5 files contain low energy in the “ms1” dataset, high
energy in the “ms2” dataset, and metadata in the “metadata” dataset. The
`fromJSON` function from `jsonlite` package will import the metadata
json file (associated with the Parquet file) as a list of dataframes.

``` r
sampleparquet = arrow::read_parquet("sample.parquet")
metadataparquet = jsonlite::fromJSON("sample-metadata.json")

samplems1hdf5 = rhdf5::h5read("sample.h5", name = "ms1")
samplems2hdf5 = rhdf5::h5read("sample.h5", name = "ms2")
samplemetadatahdf5 = rhdf5::h5read("sample.h5", name = "samplemetadata")
spectrummetadatahdf5 = rhdf5::h5read("sample.h5", name = "spectrummetadata")
```

## Shiny App

A Shiny application is available to use the package easily. To run the
app, just use the following command (it might need to install a few
additional packages):

``` r
run_app()
```
