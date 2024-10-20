
# 🏹 arcMS <a href="https://leesulab.github.io/arcMS/"><img src="man/figures/logo.png" align="right" height="138" alt="arcMS website" /></a>

<!-- badges: start -->
<!-- badges: end -->

`arcMS` can convert (HD)MS<sup>E</sup> data acquired with Waters UNIFI
to tabular format for use in R or Python, with a small filesize when
saved on disk. It is compatible with data containing ion mobility
(HDMS<sup>E</sup>) or not (MS<sup>E</sup>).

Two output data file formats can be obtained:

- the [Apache Parquet](https://parquet.apache.org/) format for minimal
  filesize and fast access.

- the [HDF5](https://www.hdfgroup.org/solutions/hdf5/) format, with fast
  access but larger filesize.

`arcMS` stands for *accessible*, *rapid* and *compact*, and is also
based on the french word *arc*, which means *bow,* to emphasize that it
is compatible with the [Apache Arrow
library](https://arrow.apache.org/).

A companion app (R/Shiny app) is provided at
<https://github.com/leesulab/arcms-dataviz> for fast visualization of
the converted data (Parquet format) as 2D plots, TIC, BPI or EIC
chromatograms…

Also, check the `vignette("open-files")` for details on how converted
files can be opened in R or Python, and the [full
tutorial](articles/data-filtration-tutorial.html) on how to query,
filter, aggregate data (e.g. to obtain chromatograms or spectra).

## :arrow_down: Installation

You can install `arcMS` in R with the following command:

``` r
install.packages("pak")
pak::pak("leesulab/arcMS")
```

To use the HDF5 format, the `rhdf5` package needs to be installed:

``` r
pak::pak("rhdf5")
```

## 🚀 Usage

First load the package:

``` r
library("arcMS")
```

Then create connection parameters to the UNIFI API (retrieve token). See
`vignette("api-configuration")` to know how to configure the API and
register a client app.

``` r
con = create_connection_params(apihosturl = "http://localhost:50034/unifi/v1", identityurl = "http://localhost:50333/identity/connect/token")
```

If `arcMS` and the `R` session are run from another computer than where
the UNIFI API is installed, replace `localhost` by the IP address of the
UNIFI API.

``` r
con = create_connection_params(apihosturl = "http://192.0.2.0:50034/unifi/v1", identityurl = "http://192.0.2.0:50333/identity/connect/token")
```

Now these connection parameters will be used to access the UNIFI
folders. The following function will show the list of folders and their
IDs (e.g. `abe9c297-821e-4152-854a-17c73c9ff68c` in the example below).

``` r
folders = folders_search()
folders
```

    #>                                     id       name               path folderType
    #> 3 abe9c297-821e-4152-854a-17c73c9ff68c Christelle Company/Christelle    Project
    #> 4 abe7a0e6-99d2-4e57-a618-f4b085f48443 EMMANUELLE Company/EMMANUELLE    Project
    #>                               parentId
    #> 3 7c3a0fc7-3805-4c14-ab68-8da3e115702e
    #> 4 7c3a0fc7-3805-4c14-ab68-8da3e115702e

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

Once we get a sample ID, we can use it to download the sample data,
using the `future` framework for parallel processing:

``` r
library(future)
plan(multisession)
convert_one_sample_data(sample_id = "0134efbf-c75a-411b-842a-4f35e2b76347")
```

This command will get the sample name (`sample_name`) and its parent
analysis (`analysis_name`), create a folder named `analysis_name` in the
working directory and save the sample data with the name
`sample_name.parquet` and its metadata with the name
`sample_name-metadata.json` (metadata is also saved in the parquet
file).

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

Other functions are available to only collect the data from the API to
an R object, and then to save this R object to a Parquet file (see
`vignette("collect-save-functions")`). CCS values can also be retrieved
in addition to bin index and drift time values, see
`vignette("get-ccs-values")`.

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

## ✨ Shiny App

A Shiny application is available to use the package easily. To run the
app, just use the following command (it might need to install a few
additional packages):

``` r
run_app()
```
