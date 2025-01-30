# Load required libraries
library(jsonlite)  # For JSON file handling
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation

# Step 1: Load the JSON file
file_path <- "../data/Report_Json_Session_Report_20240323T174345.json"

data <- fromJSON(file_path)

rm(file_path)


data$SenseChannelTests


# Step 2: Extract Stimulation Parameters
stimulation_status <- list(
  initial_status = data$Stimulation$InitialStimStatus,
  final_status = data$Stimulation$FinalStimStatus
)

# Initial and Final Groups
initial_groups <- data$Groups$Initial
final_groups <- data$Groups$Final

str(initial_groups)

lapply(initial_groups, names)

# Summarize groups
summarize_groups <- function(groups_df) {
  groups_df %>%
    transmute(
      GroupId = GroupId,
      Active = ActiveGroup,
      Left_Programs = sapply(ProgramSettings$LeftHemisphere$Programs, length),
      Right_Programs = sapply(ProgramSettings$RightHemisphere$Programs, length)
    )
}



# Apply the function to initial and final groups
initial_group_summary <- summarize_groups(initial_groups)

final_group_summary <- summarize_groups(final_groups)


# View summaries
print("Initial Group Summary:")
print(initial_group_summary)

print("Final Group Summary:")
print(final_group_summary)



# Step 5: Battery Information
battery_info <- tibble(
  Percentage = data$BatteryInformation$BatteryPercentage,
  EstimatedLifeMonths = data$BatteryInformation$EstimatedBatteryLifeMonths,
  Status = data$BatteryInformation$BatteryStatus
)




# Step 4: Analyze Signal Data (Frequency Bands)
# Extract PSD values and frequencies
frequencies <- unlist(data$Groups$Initial$ProgramSettings$SensingChannel[[2]]$SensingSetup$ChannelSignalResult$SignalFrequencies)
psd_values <- unlist(data$Groups$Initial$ProgramSettings$SensingChannel[[2]]$SensingSetup$ChannelSignalResult$SignalPsdValues)

contacts <- data$Groups$Initial$ProgramSettings$SensingChannel[[2]]$SensingSetup$ChannelSignalResult$Channel

# Define frequency bands
frequency_bands <- list(
  Delta = c(0.5, 4),
  Theta = c(4, 8),
  Alpha = c(8, 12),
  Beta = c(12, 30),
  Gamma = c(30, 100)
)

# Calculate power for each frequency band
calculate_band_power <- function(frequencies, psd_values, bands) {
  sapply(bands, function(band) {
    sum(psd_values[frequencies >= band[1] & frequencies <= band[2]])
  })
}

# Compute band power for the available data
band_powers <- calculate_band_power(frequencies, psd_values, frequency_bands)


# Prepare the results as a data frame for visualization
band_powers_df <- data.frame(
  Band = names(band_powers),
  Power = band_powers
)


# Visualize the band power distribution

ggplot(band_powers_df, aes(x = Band, y = Power, fill = Band)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Frequency Band Power Distribution",
    x = "Frequency Band",
    y = "Power"
  ) +
  theme_minimal()


plot(frequencies, psd_values)








# Step 1: Parse Impedance Data
# Assuming the impedance data is available as `data$Impedance`

# Step 1: Extract Left and Right Hemisphere Data
impedance_data <- data$Impedance$Hemisphere[[1]]  # Full data frame


# Separate Left and Right based on the 'Hemisphere' column
left_data <- subset(impedance_data, Hemisphere == "HemisphereLocationDef.Left")
right_data <- subset(impedance_data, Hemisphere == "HemisphereLocationDef.Right")




# Step 2: Extract Monopolar and Bipolar Data
extract_impedance <- function(data, label) {
  # Extract Monopolar
  monopolar <- do.call(rbind, data$SessionImpedance$Monopolar)
  monopolar$Type <- "Monopolar"
  monopolar$Hemisphere <- label
  
  # Extract Bipolar
  bipolar <- do.call(rbind, data$SessionImpedance$Bipolar)
  bipolar$Type <- "Bipolar"
  bipolar$Hemisphere <- label
  
  # Combine Monopolar and Bipolar
  rbind(monopolar, bipolar)
}

# Step 3: Apply Extraction Function to Left and Right Data
left_impedance <- extract_impedance(left_data, "Left")
right_impedance <- extract_impedance(right_data, "Right")

# Step 4: Combine Left and Right Data
combined_impedance_df <- rbind(left_impedance, right_impedance)

# Step 5: View the Combined Data
print(head(combined_impedance_df))

# Optional: Summarize Impedance by Hemisphere and Type
library(dplyr)
combined_impedance_df %>%
  group_by(Hemisphere, Type) %>%
  summarize(MeanImpedance = mean(ResultValue, na.rm = TRUE), .groups = "drop")


# LFPs

data$LFPMontage$LFPFrequency
data$LFPMontage$SensingElectrodes
data$LFPMontage$ArtifactStatus
data$LFPMontage$PeakFrequencyInHertz
data$LFPMontage$PeakMagnitudeInMicroVolt



# Create an empty data frame to store all the structured data
structured_data <- data.frame(
  Hemisphere = character(0),
  SensingElectrodes = character(0),
  ArtifactStatus = character(0),
  PeakFrequencyInHertz = numeric(0),
  PeakMagnitudeInMicroVolt = numeric(0),
  LFPFrequency = numeric(0),
  LFPMagnitude = numeric(0)
)

# Loop through each of the 12 configurations
for (i in 1:length(data$LFPMontage$LFPFrequency)) {
  
  # Extract the relevant data for this configuration
  frequencies <- data$LFPMontage$LFPFrequency[[i]]
  magnitudes <- data$LFPMontage$LFPMagnitude[[i]]  # Include the magnitudes
  hemisphere <- data$LFPMontage$Hemisphere[i]
  electrodes <- data$LFPMontage$SensingElectrodes[i]
  artifact_status <- data$LFPMontage$ArtifactStatus[i]
  peak_freq <- data$LFPMontage$PeakFrequencyInHertz[i]
  peak_mag <- data$LFPMontage$PeakMagnitudeInMicroVolt[i]
  
  # For this configuration, create a data frame of frequencies, magnitudes, and associated metadata
  temp_data <- data.frame(
    Hemisphere = rep(hemisphere, length(frequencies)),
    SensingElectrodes = rep(electrodes, length(frequencies)),
    ArtifactStatus = rep(artifact_status, length(frequencies)),
    PeakFrequencyInHertz = rep(peak_freq, length(frequencies)),
    PeakMagnitudeInMicroVolt = rep(peak_mag, length(frequencies)),
    LFPFrequency = frequencies,
    LFPMagnitude = magnitudes  # Add the magnitudes here
  )
  
  # Append this data to the structured data frame
  structured_data <- rbind(structured_data, temp_data)
}

# View the structured data
head(structured_data)

structured_data %>% filter(ArtifactStatus =="ArtifactStatusDef.SQC_ARTIFACT_PRESENT") %>%
  filter(SensingElectrodes=="SensingElectrodeConfigDef.TWO_AND_THREE") %>%
  filter(Hemisphere=="HemisphereLocationDef.Left") %>%
  select(LFPFrequency , LFPMagnitude) %>%
  ggplot(aes(LFPFrequency , LFPMagnitude) ) +
  geom_point()



structured_data %>% filter(ArtifactStatus !="ArtifactStatusDef.SQC_ARTIFACT_PRESENT") %>%
  select(SensingElectrodes) %>% distinct()
  



data$LfpMontageTimeDomain


# Extract the first sequence of time-domain data
time_domain_first_sequence <- data$LfpMontageTimeDomain$TimeDomainData[[1]]

# Plotting the first time-domain sequence
plot(time_domain_first_sequence, type = "l", 
     main = "Time-Domain Signal for First Pass and First Sequence",
     xlab = "Time (samples)", ylab = "Amplitude",
     col = "blue", lwd = 2)



# Loop through all 12 sequences and plot each
for (i in 1:length(data$LfpMontageTimeDomain$TimeDomainData)) {
  plot(data$LfpMontageTimeDomain$TimeDomainData[[i]], type = "l", 
       main = paste("Time-Domain Signal for Pass", i),
       xlab = "Samples", ylab = "Amplitude", col = rainbow(12)[i])
}
