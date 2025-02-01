# Load required libraries
library(jsonlite)  # For JSON file handling
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation

# Step 1: Load the JSON file
file_path <- "../data/Report_Json_Session_Report_20240323T174345.json"


data <- fromJSON(file_path)

rm(file_path)



names(data)

#  [1] "AbnormalEnd"                    "FullyReadForSession"           
#  [3] "FeatureInformationCode"         "SessionDate"                   
#  [5] "SessionEndDate"                 "ProgrammerTimezone"            
#  [7] "ProgrammerUtcOffset"            "ProgrammerLocale"              
#  [9] "ProgrammerVersion"              "PatientInformation"            
# [11] "DeviceInformation"              "BatteryInformation"            
# [13] "GroupUsagePercentage"           "LeadConfiguration"             
# [15] "Stimulation"                    "Groups"                        
# [17] "BatteryReminder"                "MostRecentInSessionSignalCheck"
# [19] "Impedance"                      "GroupHistory"                  
# [21] "SenseChannelTests"              "CalibrationTests"              
# [23] "LfpMontageTimeDomain"           "BrainSenseTimeDomain"          
# [25] "BrainSenseLfp"                  "LFPMontage"                    
# [27] "DiagnosticData"      

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







# Step 4: Analyze Signal Data (Frequency Bands)

# Safely extract signal data
extract_signal_data <- function(data) {
  if (!is.null(data$Groups$Initial$ProgramSettings$SensingChannel) &&
      length(data$Groups$Initial$ProgramSettings$SensingChannel) >= 2 &&
      !is.null(data$Groups$Initial$ProgramSettings$SensingChannel[[2]]$SensingSetup$ChannelSignalResult)) {
    
    frequencies <- unlist(data$Groups$Initial$ProgramSettings$SensingChannel[[2]]$SensingSetup$ChannelSignalResult$SignalFrequencies)
    psd_values <- unlist(data$Groups$Initial$ProgramSettings$SensingChannel[[2]]$SensingSetup$ChannelSignalResult$SignalPsdValues)
    contacts <- data$Groups$Initial$ProgramSettings$SensingChannel[[2]]$SensingSetup$ChannelSignalResult$Channel
    
    return(list(frequencies = frequencies, psd_values = psd_values, contacts = contacts))
  } else {
    return(NULL)
  }
}

# Extract signal data
signal_data <- extract_signal_data(data)

if (!is.null(signal_data) && length(signal_data$frequencies) > 0 && length(signal_data$psd_values) > 0) {
  
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
      sum(psd_values[frequencies >= band[1] & frequencies <= band[2]], na.rm = TRUE)
    })
  }
  
  # Compute band power
  band_powers <- calculate_band_power(signal_data$frequencies, signal_data$psd_values, frequency_bands)
  
  # Prepare data frame
  band_powers_df <- data.frame(
    Band = names(band_powers),
    Power = band_powers
  )
  
  # Visualize band power distribution
  ggplot(band_powers_df, aes(x = Band, y = Power, fill = Band)) +
    geom_bar(stat = "identity", color = "black") +
    labs(
      title = "Frequency Band Power Distribution",
      x = "Frequency Band",
      y = "Power"
    ) +
    theme_minimal()
  
  # Plot raw frequency spectrum
  plot(signal_data$frequencies, signal_data$psd_values, type = "l", col = "blue", 
       main = "Power Spectral Density", xlab = "Frequency (Hz)", ylab = "Power")
  
} else {
  print("No valid signal data available for this patient.")
}







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





library(dplyr)

# Step 1: Check if Impedance Data Exists
if (!is.null(data$Impedance) && !is.null(data$Impedance$Hemisphere)) {
  
  impedance_data <- data$Impedance$Hemisphere  # Full list
  
  # Function to extract and clean impedance data
  extract_impedance <- function(hemisphere_data, label) {
    if (is.null(hemisphere_data$SessionImpedance)) return(NULL)  # Skip if missing
    
    # Extract Monopolar if it exists
    if (!is.null(hemisphere_data$SessionImpedance$Monopolar)) {
      monopolar <- do.call(rbind, hemisphere_data$SessionImpedance$Monopolar)
      monopolar$Type <- "Monopolar"
      monopolar$Hemisphere <- label
    } else {
      monopolar <- NULL
    }
    
    # Extract Bipolar if it exists
    if (!is.null(hemisphere_data$SessionImpedance$Bipolar)) {
      bipolar <- do.call(rbind, hemisphere_data$SessionImpedance$Bipolar)
      bipolar$Type <- "Bipolar"
      bipolar$Hemisphere <- label
    } else {
      bipolar <- NULL
    }
    
    # Combine Monopolar and Bipolar data
    rbind(monopolar, bipolar)
  }
  
  # Extract Left and Right Hemisphere Data
  left_data <- impedance_data[sapply(impedance_data, function(x) x$Hemisphere == "HemisphereLocationDef.Left")]
  right_data <- impedance_data[sapply(impedance_data, function(x) x$Hemisphere == "HemisphereLocationDef.Right")]
  
  # Apply function (only if data exists)
  left_impedance <- if (length(left_data) > 0) extract_impedance(left_data[[1]], "Left") else NULL
  right_impedance <- if (length(right_data) > 0) extract_impedance(right_data[[1]], "Right") else NULL
  
  # Combine left and right hemisphere data
  combined_impedance_df <- rbind(left_impedance, right_impedance)
  
  if (!is.null(combined_impedance_df)) {
    
    # Step 5: View the Combined Data
    print(head(combined_impedance_df))
    
    # Step 6: Summarize Impedance by Hemisphere and Type
    impedance_summary <- combined_impedance_df %>%
      group_by(Hemisphere, Type) %>%
      summarize(MeanImpedance = mean(ResultValue, na.rm = TRUE), .groups = "drop")
    
    print(impedance_summary)
    
  } else {
    print("No valid impedance data available for this patient.")
  }
  
} else {
  print("Impedance data is missing for this patient.")
}




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



# Other fields

data$AbnormalEnd
data$FullyReadForSession
data$FeatureInformationCode
data$SessionDate
data$SessionEndDate
data$ProgrammerTimezone
data$ProgrammerUtcOffset
data$ProgrammerLocale
data$ProgrammerVersion

data$PatientInformation$Initial$PatientFirstName
data$PatientInformation$Initial$PatientLastName
data$PatientInformation$Initial$PatientGender
data$PatientInformation$Initial$PatientDateOfBirth
data$PatientInformation$Initial$PatientId
data$PatientInformation$Initial$ClinicianNotes
data$PatientInformation$Initial$Diagnosis


data$PatientInformation$Final$PatientFirstName
data$PatientInformation$Final$PatientLastName
data$PatientInformation$Final$PatientGender
data$PatientInformation$Final$PatientDateOfBirth
data$PatientInformation$Final$PatientId
data$PatientInformation$Final$ClinicianNotes



data$DeviceInformation$Initial$Neurostimulator
data$DeviceInformation$Initial$NeurostimulatorModel
data$DeviceInformation$Initial$NeurostimulatorSerialNumber
data$DeviceInformation$Initial$NeurostimulatorLocation
data$DeviceInformation$Initial$ImplantDate
data$DeviceInformation$Initial$DeviceDateTime
data$DeviceInformation$Initial$OverdischargeCount
data$DeviceInformation$Initial$ProductApplicationId
data$DeviceInformation$Initial$ProductVersion
data$DeviceInformation$Initial$DeviceApplicationId
data$DeviceInformation$Initial$ProductVersion
data$DeviceInformation$Initial$DeviceApplicationId
data$DeviceInformation$Initial$DeviceVersion
data$DeviceInformation$Initial$CoreApplicationId
data$DeviceInformation$Initial$CoreVersion
data$DeviceInformation$Initial$AccumulatedTherapyOnTimeSinceImplant
data$DeviceInformation$Initial$AccumulatedTherapyOnTimeSinceFollowup
data$DeviceInformation$Initial$DeviceName

data$DeviceInformation$Final$Neurostimulator
data$DeviceInformation$Final$NeurostimulatorModel
data$DeviceInformation$Final$NeurostimulatorSerialNumber
data$DeviceInformation$Final$NeurostimulatorLocation
data$DeviceInformation$Final$ImplantDate
data$DeviceInformation$Final$DeviceDateTime
data$DeviceInformation$Final$OverdischargeCount
data$DeviceInformation$Final$ProductApplicationId
data$DeviceInformation$Final$ProductVersion
data$DeviceInformation$Final$DeviceApplicationId
data$DeviceInformation$Final$ProductVersion
data$DeviceInformation$Final$DeviceApplicationId
data$DeviceInformation$Final$DeviceVersion
data$DeviceInformation$Final$CoreApplicationId
data$DeviceInformation$Final$CoreVersion
data$DeviceInformation$Final$AccumulatedTherapyOnTimeSinceImplant
data$DeviceInformation$Final$AccumulatedTherapyOnTimeSinceFollowup
data$DeviceInformation$Final$DeviceName


data$BatteryInformation$BatteryPercentage
data$BatteryInformation$EstimatedBatteryLifeMonths
data$BatteryInformation$TTEData
data$BatteryInformation$BatteryStatus

data$Stimulation$InitialStimStatus
data$Stimulation$FinalStimStatus


data$LeadConfiguration$Initial
data$LeadConfiguration$Final

data$BatteryReminder$Enabled

data$MostRecentInSessionSignalCheck$Channel

data$MostRecentInSessionSignalCheck$SignalFrequencies

data$MostRecentInSessionSignalCheck$SignalPsdValues



# Loop through the lists and combine them
combined_data <- mapply(function(channel, frequencies, psdvalues) {
  data.frame(Channel = channel, Frequencies = frequencies, PsdValues = psdvalues)
}, data$MostRecentInSessionSignalCheck$Channel, 
   data$MostRecentInSessionSignalCheck$SignalFrequencies, 
   data$MostRecentInSessionSignalCheck$SignalPsdValues, 
   SIMPLIFY = FALSE)


combined_data


# Create a list of plots for each combination in combined_data
plots <- lapply(combined_data, function(data_frame) {
  ggplot(data_frame, aes(x = Frequencies, y = PsdValues)) +
    geom_line() +                     # You can also use geom_point() for scatter
    geom_point() +                    # Adds points to the line plot
    labs(title = paste("Signal Plot for", data_frame$Channel[1]),
         x = "Frequency (Hz)",
         y = "Power Spectral Density (PSD)") +
    theme_minimal()
})

# Show all plots
plots


data$BrainSenseTimeDomain$TicksInMses[4]

data$BrainSenseTimeDomain$TimeDomainData[4]



# Check the lengths of both
ticks_length <- length(as.numeric(strsplit(data$BrainSenseTimeDomain$TicksInMses[4], ",")[[1]]))
time_domain_length <- length(data$BrainSenseTimeDomain$TimeDomainData[[4]])

# Print lengths to understand the issue
print(paste("TicksInMses Length: ", ticks_length))
print(paste("TimeDomainData Length: ", time_domain_length))


# Extract TimeDomainData for index 4
time_domain_data <- data$BrainSenseTimeDomain$TimeDomainData[[1]]

# Plot the data
plot(time_domain_data, type="l", col="blue", 
     xlab="Index", ylab="Amplitude", 
     main="Time Domain Data (Index 4)")
