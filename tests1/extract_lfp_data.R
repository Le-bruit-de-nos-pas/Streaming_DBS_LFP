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
  
  
  
  
  # Loop through each unique electrode type and plot the LFP Frequency vs Magnitude
  unique_electrodes <- unique(structured_lfp_data$SensingElectrodes)
  
  for (electrode in unique_electrodes) {
  
    # Subset the data for the specific electrode
  electrode_data <- structured_lfp_data %>% filter(SensingElectrodes == electrode)
  
  # Create the ggplot for LFP Frequency vs Magnitude
  p <- electrode_data %>%
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
  print(p)
}
  
  
  
  

  
  # Return the structured LFP data
  return(structured_lfp_data)
}


  
# Example Usage: Extract LFP data and plot
lfp_data <- extract_lfp_data_and_plot(data)

# View the structured data (optional)
head(lfp_data)
