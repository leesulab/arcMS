# arcMS 1.0.0

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
