library(dplyr)
library(ggplot2)

# Function to process LFP data and generate all plots
extract_lfp_data_and_plot <- function(data) {
  
  # Initialize an empty data frame to store the LFP data
  structured_lfp_data <- data.frame(
    Hemisphere = character(0),
    SensingElectrodes = character(0),
    ArtifactStatus = character(0),
    PeakFrequencyInHertz = numeric(0),
    PeakMagnitudeInMicroVolt = numeric(0),
    LFPFrequency = numeric(0),
    LFPMagnitude = numeric(0)
  )
  
  # Loop through each LFP configuration and extract the necessary data
  for (i in 1:length(data$LFPMontage$LFPFrequency)) {
    
    # Extract the relevant data for each configuration
    frequencies <- data$LFPMontage$LFPFrequency[[i]]
    magnitudes <- data$LFPMontage$LFPMagnitude[[i]]  # Include the magnitudes
    hemisphere <- data$LFPMontage$Hemisphere[i]
    electrodes <- data$LFPMontage$SensingElectrodes[i]
    artifact_status <- data$LFPMontage$ArtifactStatus[i]
    peak_freq <- data$LFPMontage$PeakFrequencyInHertz[i]
    peak_mag <- data$LFPMontage$PeakMagnitudeInMicroVolt[i]
    
    # Create a data frame for each configuration with the LFP data and associated metadata
    temp_data <- data.frame(
      Hemisphere = rep(hemisphere, length(frequencies)),
      SensingElectrodes = rep(electrodes, length(frequencies)),
      ArtifactStatus = rep(artifact_status, length(frequencies)),
      PeakFrequencyInHertz = rep(peak_freq, length(frequencies)),
      PeakMagnitudeInMicroVolt = rep(peak_mag, length(frequencies)),
      LFPFrequency = frequencies,
      LFPMagnitude = magnitudes  # Include LFPMagnitudes
    )
    
    # Append to the structured data frame
    structured_lfp_data <- rbind(structured_lfp_data, temp_data)
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
  
  unique_electrodes <- unique(structured_lfp_data$SensingElectrodes)
  
  for (electrode in unique_electrodes) {
    electrode_data <- structured_lfp_data %>% filter(SensingElectrodes == electrode)
    
    # Ensure the data exists for processing
    if (nrow(electrode_data) > 0) {
      band_powers <- calculate_band_power(electrode_data$LFPFrequency, electrode_data$LFPMagnitude, frequency_bands)
      
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
         x = "Frequency Band",
         y = "Power") +
    theme_minimal()
  
  
  print(p1)
  
  
  
  # Loop through each unique electrode type and plot the LFP Frequency vs Magnitude
  unique_electrodes <- unique(structured_lfp_data$SensingElectrodes)
  
  for (electrode in unique_electrodes) {
  
    # Subset the data for the specific electrode
  electrode_data <- structured_lfp_data %>% filter(SensingElectrodes == electrode)
  
  # Create the ggplot for LFP Frequency vs Magnitude
  p2 <- electrode_data %>%
    ggplot(aes(LFPFrequency, LFPMagnitude, colour = Hemisphere, fill = Hemisphere)) +
    geom_point() +
    geom_line() +  # Add lines connecting the points
    labs(
      title = paste("LFP Frequency vs Magnitude for Electrodes:", electrode),
      x = "LFP Frequency (Hz)",
      y = "LFP Magnitude (uV)"
    ) +
    theme_minimal()  # Optional: Adjust the theme
  
  # Explicitly print the plot to ensure it renders
  print(p2)
}
  
  
  
    # Plot the time-domain signals for all sequences in the LFP montage
  for (i in 1:length(data$LfpMontageTimeDomain$TimeDomainData)) {
    time_domain_sequence <- data$LfpMontageTimeDomain$TimeDomainData[[i]]
    plot(time_domain_sequence, type = "l", 
         main = paste("Time-Domain Signal for Pass", i),
         xlab = "Time (samples)", ylab = "Amplitude", 
         col = rainbow(30)[i], lwd = 2)
  }
  
  

  print(structured_lfp_data)
  
  # Return the structured LFP data
  return(structured_lfp_data)
}


  
# Example Usage: Extract LFP data and plot
lfp_data <- extract_lfp_data_and_plot(data)

# View the structured data (optional)
head(lfp_data)
