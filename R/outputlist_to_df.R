#' Transform deserialized data to a dataframe
#'
#'The function adapts the data in S3 format obtained with the `deserialize_data()` function into a dataframe.
#'
#' @param outputlist Deserialized data in S3 format obtained with the deserialized_data() function
#' @return A dataframe containing the spectral data associated with each spectra.
#' @keywords internal

outputlist_to_df <- function(outputlist){
  data_all = data.frame(masses = double(),    # Create empty data frame
                        intensities = double(),
                        scan_size = double(),
                        rt = double(),
                        energy_level = double(),
                        ionization_polarity = double())
  for (i in 1:length(outputlist)){
    data_all[i,]$intensities = list(outputlist[[i]]$Intensities)
    data_all[i,]$masses = list(outputlist[[i]]$MassSpectrum$Masses)
    data_all[i,]$scan_size = list(outputlist[[i]]$MassSpectrum$ScanSize)
    data_all[i,]$rt = outputlist[[i]]$MassSpectrum$MSeMassSpectrum$RetentionTime
    data_all[i,]$energy_level = outputlist[[i]]$MassSpectrum$MSeMassSpectrum$EnergyLevel
    data_all[i,]$ionization_polarity = outputlist[[i]]$MassSpectrum$MSeMassSpectrum$IonizationPolarity

  }
  data_all$energy_level <- ifelse(data_all$energy_level == 1, "Low", "High")
  data_all$ionization_polarity <- ifelse(data_all$ionization_polarity == 2, 1, 0)


  return(data_all)
  }
