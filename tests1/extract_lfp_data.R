library(dplyr)
library(ggplot2)

# Function to process LFP data (frequency, magnitude, and time-domain)
extract_lfp_data <- function(data) {
  
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
  
  # Return the structured data
  return(structured_lfp_data)
}

# Example Usage: Extract LFP data
lfp_data <- extract_lfp_data(data)

# View the structured data
head(lfp_data)

# Plot LFP Frequency vs Magnitude for specific conditions
lfp_data %>% 
  filter(ArtifactStatus == "ArtifactStatusDef.SQC_ARTIFACT_PRESENT") %>%
  filter(SensingElectrodes == "SensingElectrodeConfigDef.TWO_AND_THREE") %>%
  filter(Hemisphere == "HemisphereLocationDef.Left") %>%
  select(LFPFrequency, LFPMagnitude) %>%
  ggplot(aes(LFPFrequency, LFPMagnitude)) +
  geom_point() +
  theme_minimal() +
  labs(title = "LFP Frequency vs Magnitude for Left Hemisphere (Artifact Present)")

# Explore distinct sensing electrodes for non-artifact data
lfp_data %>%
  filter(ArtifactStatus != "ArtifactStatusDef.SQC_ARTIFACT_PRESENT") %>%
  select(SensingElectrodes) %>%
  distinct()

# Plot the time-domain signals for all sequences in the LFP montage
for (i in 1:length(data$LfpMontageTimeDomain$TimeDomainData)) {
  plot(data$LfpMontageTimeDomain$TimeDomainData[[i]], type = "l", 
       main = paste("Time-Domain Signal for Pass", i),
       xlab = "Samples", ylab = "Amplitude", col = rainbow(12)[i])
}

# Plotting the first time-domain signal
time_domain_first_sequence <- data$LfpMontageTimeDomain$TimeDomainData[[1]]

plot(time_domain_first_sequence, type = "l", 
     main = "Time-Domain Signal for First Pass and First Sequence",
     xlab = "Time (samples)", ylab = "Amplitude",
     col = "blue", lwd = 2)
