library(dplyr)
library(ggplot2)

# Function to process LFP dataset and generate all plots
lfp_dataset_and_plot <- function(dataset) {
  
  # Initialize an empty dataset frame to store the LFP dataset
  structured_lfp_dataset <- data.frame(
    Hemisphere = character(0),
    SensingElectrodes = character(0),
    ArtifactStatus = character(0),
    PeakFrequencyInHertz = numeric(0),
    PeakMagnitudeInMicroVolt = numeric(0),
    LFPFrequency = numeric(0),
    LFPMagnitude = numeric(0)
  )
  
  # Loop through each LFP configuration and extract the necessary dataset
  for (i in 1:length(dataset$LFPMontage$LFPFrequency)) {
    
    # Extract the relevant dataset for each configuration
    frequencies <- dataset$LFPMontage$LFPFrequency[[i]]
    magnitudes <- dataset$LFPMontage$LFPMagnitude[[i]]  # Include the magnitudes
    hemisphere <- dataset$LFPMontage$Hemisphere[i]
    electrodes <- dataset$LFPMontage$SensingElectrodes[i]
    artifact_status <- dataset$LFPMontage$ArtifactStatus[i]
    peak_freq <- dataset$LFPMontage$PeakFrequencyInHertz[i]
    peak_mag <- dataset$LFPMontage$PeakMagnitudeInMicroVolt[i]
    
    # Create a dataset frame for each configuration with the LFP dataset and associated metadataset
    temp_dataset <- data.frame(
      Hemisphere = rep(hemisphere, length(frequencies)),
      SensingElectrodes = rep(electrodes, length(frequencies)),
      ArtifactStatus = rep(artifact_status, length(frequencies)),
      PeakFrequencyInHertz = rep(peak_freq, length(frequencies)),
      PeakMagnitudeInMicroVolt = rep(peak_mag, length(frequencies)),
      LFPFrequency = frequencies,
      LFPMagnitude = magnitudes  # Include LFPMagnitudes
    )
    
    # Append to the structured dataset frame
    structured_lfp_dataset <- rbind(structured_lfp_dataset, temp_dataset)
  }
  
  
    # Define frequency bands
  frequency_bands <- list(
    Delta = c(0.5, 4),
    Theta = c(4, 8),
    Alpha = c(8, 12),
    Beta = c(12, 30),
    Gamma = c(30, 100)
  )

  # Function to compute power in each frequency band
  calculate_band_power <- function(frequencies, magnitudes, bands) {
    sapply(bands, function(band) {
      sum(magnitudes[frequencies >= band[1] & frequencies <= band[2]], na.rm = TRUE)
    })
  }
  
  # Compute band power for each electrode
  band_power_results <- data.frame()
  
  unique_electrodes <- unique(structured_lfp_dataset$SensingElectrodes)
  
  for (electrode in unique_electrodes) {
    electrode_dataset <- structured_lfp_dataset %>% filter(SensingElectrodes == electrode)
    
    # Ensure the dataset exists for processing
    if (nrow(electrode_dataset) > 0) {
      band_powers <- calculate_band_power(electrode_dataset$LFPFrequency, electrode_dataset$LFPMagnitude, frequency_bands)
      
      band_power_results <- rbind(band_power_results, data.frame(
        SensingElectrodes = electrode,
        Band = names(band_powers),
        Power = band_powers
      ))
    }
  }
  
  print(band_power_results)
  
  
  # Visualize band power distribution
  p1 <- band_power_results %>%
    ggplot(aes(x = Band, y = Power, fill = Band)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~SensingElectrodes) +  
    labs(title = "Frequency Band Power Distribution per Electrode",
         x = "\n Frequency Band",
         y = "Power \n")  +
    scale_colour_manual(values=c("deepskyblue4", "deeppink4", "bisque3", "darkslategray", "cadetblue4")) +
    scale_fill_manual(values=c("deepskyblue4", "deeppink4", "bisque3", "darkslategray", "cadetblue4")) +
    ggpubr::theme_pubr() 
  
  
  print(p1)
  
  
  
  # Loop through each unique electrode type and plot the LFP Frequency vs Magnitude
  unique_electrodes <- unique(structured_lfp_dataset$SensingElectrodes)
  
  for (electrode in unique_electrodes) {
  
    # Subset the dataset for the specific electrode
  electrode_dataset <- structured_lfp_dataset %>% filter(SensingElectrodes == electrode)
  
  # Create the ggplot for LFP Frequency vs Magnitude
  p2 <- electrode_dataset %>%
    ggplot(aes(LFPFrequency, LFPMagnitude, colour = Hemisphere, fill = Hemisphere)) +
    geom_point(size=2,shape=1,stroke=1.5 , alpha=0.9) +
    scale_colour_manual(values=c("firebrick", "midnightblue")) +
    geom_line(linewidth=2,  alpha=0.7) +  # Add lines connecting the points
    labs(
      title = paste("LFP Frequency vs Magnitude for Electrodes:", electrode),
      x = "\n LFP Frequency (Hz)",
      y = "LFP Magnitude (uV) \n"
    ) +
    theme_minimal()  # Optional: Adjust the theme
  
  # Explicitly print the plot to ensure it renders
  print(p2)
}
  
  
  
    # Plot the time-domain signals for all sequences in the LFP montage
  for (i in 1:length(dataset$LfpMontageTimeDomain$TimeDomainData)) {
    time_domain_sequence <- dataset$LfpMontageTimeDomain$TimeDomainData[[i]]
    plot(time_domain_sequence, type = "l", 
         main = paste("Time-Domain Signal for Pass", i),
         xlab = "Time (samples)", ylab = "Amplitude", 
         col = rainbow(30)[i], lwd = 2)
  }
  
  

  print(structured_lfp_dataset)
  
  # Return the structured LFP dataset
  return(structured_lfp_dataset)
  
  
}


  
# Example Usage: Extract LFP dataset and plot
lfp_dataset <- lfp_dataset_and_plot(dataset)

# View the structured dataset (optional)
head(lfp_dataset)
