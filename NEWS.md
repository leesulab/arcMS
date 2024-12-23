# arcMS 1.2.2

## Changes

- Conversion of mzML files to Parquet
- Bug fix: regenerating connection when lost during two conversions (getting new token)

# arcMS 1.2.1

## Changes

- In Shiny app: added option (checkbox) to overwrite or not the files already converted and present in target folder

# arcMS 1.2.0

## Changes

- `sample_dataset` object creation with on-disk method (not loading Parquet data in memory)
- Added vignette with full tutorial on how to open, filter and aggregate data from Parquet file (e.g. to obtain chromatograms or spectra)
- Added vignette to show how to query data from a distant Parquet file

# arcMS 1.1.0

## Changes

- Compatibility with waters_connect UNIFI
- Adding vignette to describe how to configure the API and register client app

# arcMS 1.0.0

## Changes

- Shiny app for easy use
- No need for connection parameters in each function (looking for existing object in the environment)

# arcMS 0.3.0

## Changes

- Creation of `sample_infos` class to store both sample metadata and spectrum metadata.
- Creation of `sample_dataset` class to store all data and metadata.

# arcMS 0.2.0

## Changes

- simplified arguments for collect and convert functions (only `sample_id` needed, other parameters needed - analysis name, sample name - are taken from an API request).
- adding a separate function to get sample information (get_sample_infos) and save as samplemetadata. Two metadata (samplemetadata and spectrummetadata) with different schemas/number of lines columns. For parquet: a metadata json file containing the samplemetadata and spectrummetadata. For HDF5: two separate datasets in the file.

# arcMS 0.1.0

- Initial github upload.
