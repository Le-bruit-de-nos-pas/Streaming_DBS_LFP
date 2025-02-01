# Define function to extract basic summary information in long format
extract_basic_summary_long <- function(data) {
  # Initialize an empty data frame with defined column names
  summary_df <- data.frame(Variable = character(0), Value = character(0), stringsAsFactors = FALSE)
  
  # Function to safely add data to the summary dataframe
  add_to_summary <- function(variable_name, value) {
    # Ensure the 'Value' is coerced to a character type to avoid type mismatch
    value <- as.character(value)
    
    # Append the new data to the summary_df
    summary_df <<- rbind(summary_df, data.frame(Variable = variable_name, Value = value, stringsAsFactors = FALSE))
  }
  
  # Extract fields if they exist and handle missing fields with NA or other defaults
  add_to_summary("AbnormalEnd", ifelse(!is.null(data$AbnormalEnd), data$AbnormalEnd, NA))
  add_to_summary("FullyReadForSession", ifelse(!is.null(data$FullyReadForSession), data$FullyReadForSession, NA))
  add_to_summary("FeatureInformationCode", ifelse(!is.null(data$FeatureInformationCode), data$FeatureInformationCode, NA))
  add_to_summary("SessionDate", ifelse(!is.null(data$SessionDate), data$SessionDate, NA))
  add_to_summary("SessionEndDate", ifelse(!is.null(data$SessionEndDate), data$SessionEndDate, NA))
  add_to_summary("ProgrammerTimezone", ifelse(!is.null(data$ProgrammerTimezone), data$ProgrammerTimezone, NA))
  add_to_summary("ProgrammerUtcOffset", ifelse(!is.null(data$ProgrammerUtcOffset), data$ProgrammerUtcOffset, NA))
  add_to_summary("ProgrammerLocale", ifelse(!is.null(data$ProgrammerLocale), data$ProgrammerLocale, NA))
  add_to_summary("ProgrammerVersion", ifelse(!is.null(data$ProgrammerVersion), data$ProgrammerVersion, NA))
  
  # Patient Information Initial
  add_to_summary("PatientFirstNameInitial", ifelse(!is.null(data$PatientInformation$Initial$PatientFirstName), 
                                                   data$PatientInformation$Initial$PatientFirstName, NA))
  add_to_summary("PatientLastNameInitial", ifelse(!is.null(data$PatientInformation$Initial$PatientLastName), 
                                                  data$PatientInformation$Initial$PatientLastName, NA))
  add_to_summary("PatientGenderInitial", ifelse(!is.null(data$PatientInformation$Initial$PatientGender), 
                                                data$PatientInformation$Initial$PatientGender, NA))
  add_to_summary("PatientDateOfBirthInitial", ifelse(!is.null(data$PatientInformation$Initial$PatientDateOfBirth), 
                                                     data$PatientInformation$Initial$PatientDateOfBirth, NA))
  add_to_summary("PatientIdInitial", ifelse(!is.null(data$PatientInformation$Initial$PatientId), 
                                             data$PatientInformation$Initial$PatientId, NA))
  add_to_summary("ClinicianNotesInitial", ifelse(!is.null(data$PatientInformation$Initial$ClinicianNotes), 
                                                 data$PatientInformation$Initial$ClinicianNotes, NA))
  add_to_summary("DiagnosisInitial", ifelse(!is.null(data$PatientInformation$Initial$Diagnosis), 
                                            data$PatientInformation$Initial$Diagnosis, NA))
  
  # Patient Information Final
  add_to_summary("PatientFirstNameFinal", ifelse(!is.null(data$PatientInformation$Final$PatientFirstName), 
                                                 data$PatientInformation$Final$PatientFirstName, NA))
  add_to_summary("PatientLastNameFinal", ifelse(!is.null(data$PatientInformation$Final$PatientLastName), 
                                                data$PatientInformation$Final$PatientLastName, NA))
  add_to_summary("PatientGenderFinal", ifelse(!is.null(data$PatientInformation$Final$PatientGender), 
                                              data$PatientInformation$Final$PatientGender, NA))
  add_to_summary("PatientDateOfBirthFinal", ifelse(!is.null(data$PatientInformation$Final$PatientDateOfBirth), 
                                                  data$PatientInformation$Final$PatientDateOfBirth, NA))
  add_to_summary("PatientIdFinal", ifelse(!is.null(data$PatientInformation$Final$PatientId), 
                                          data$PatientInformation$Final$PatientId, NA))
  add_to_summary("ClinicianNotesFinal", ifelse(!is.null(data$PatientInformation$Final$ClinicianNotes), 
                                               data$PatientInformation$Final$ClinicianNotes, NA))
  
  # Device Information Initial
  add_to_summary("NeurostimulatorInitial", ifelse(!is.null(data$DeviceInformation$Initial$Neurostimulator), 
                                                   data$DeviceInformation$Initial$Neurostimulator, NA))
  add_to_summary("NeurostimulatorModelInitial", ifelse(!is.null(data$DeviceInformation$Initial$NeurostimulatorModel), 
                                                      data$DeviceInformation$Initial$NeurostimulatorModel, NA))
  add_to_summary("NeurostimulatorSerialNumberInitial", ifelse(!is.null(data$DeviceInformation$Initial$NeurostimulatorSerialNumber), 
                                                             data$DeviceInformation$Initial$NeurostimulatorSerialNumber, NA))
  add_to_summary("NeurostimulatorLocationInitial", ifelse(!is.null(data$DeviceInformation$Initial$NeurostimulatorLocation), 
                                                          data$DeviceInformation$Initial$NeurostimulatorLocation, NA))
  add_to_summary("ImplantDateInitial", ifelse(!is.null(data$DeviceInformation$Initial$ImplantDate), 
                                              data$DeviceInformation$Initial$ImplantDate, NA))
  add_to_summary("DeviceDateTimeInitial", ifelse(!is.null(data$DeviceInformation$Initial$DeviceDateTime), 
                                                 data$DeviceInformation$Initial$DeviceDateTime, NA))
  add_to_summary("OverdischargeCountInitial", ifelse(!is.null(data$DeviceInformation$Initial$OverdischargeCount), 
                                                      data$DeviceInformation$Initial$OverdischargeCount, NA))
  
  # Device Information Final
  add_to_summary("NeurostimulatorFinal", ifelse(!is.null(data$DeviceInformation$Final$Neurostimulator), 
                                                data$DeviceInformation$Final$Neurostimulator, NA))
  add_to_summary("NeurostimulatorModelFinal", ifelse(!is.null(data$DeviceInformation$Final$NeurostimulatorModel), 
                                                     data$DeviceInformation$Final$NeurostimulatorModel, NA))
  
  # Battery Information
  add_to_summary("BatteryPercentage", ifelse(!is.null(data$BatteryInformation$BatteryPercentage), 
                                             data$BatteryInformation$BatteryPercentage, NA))
  add_to_summary("EstimatedBatteryLifeMonths", ifelse(!is.null(data$BatteryInformation$EstimatedBatteryLifeMonths), 
                                                      data$BatteryInformation$EstimatedBatteryLifeMonths, NA))
  add_to_summary("TTEData", ifelse(!is.null(data$BatteryInformation$TTEData), 
                                   data$BatteryInformation$TTEData, NA))
  add_to_summary("BatteryStatus", ifelse(!is.null(data$BatteryInformation$BatteryStatus), 
                                         data$BatteryInformation$BatteryStatus, NA))
  
  # Stimulation Information
  add_to_summary("InitialStimStatus", ifelse(!is.null(data$Stimulation$InitialStimStatus), 
                                             data$Stimulation$InitialStimStatus, NA))
  add_to_summary("FinalStimStatus", ifelse(!is.null(data$Stimulation$FinalStimStatus), 
                                           data$Stimulation$FinalStimStatus, NA))
  
  # Lead Configuration Information
  add_to_summary("LeadConfigurationInitial", ifelse(!is.null(data$LeadConfiguration$Initial), 
                                                    data$LeadConfiguration$Initial, NA))
  add_to_summary("LeadConfigurationFinal", ifelse(!is.null(data$LeadConfiguration$Final), 
                                                  data$LeadConfiguration$Final, NA))
  
  # Battery Reminder Information
  add_to_summary("BatteryReminderEnabled", ifelse(!is.null(data$BatteryReminder$Enabled), 
                                                  data$BatteryReminder$Enabled, NA))
  
  # Return the summary dataframe in long format
  return(summary_df)
}


# Example of calling the function
summary_long_table <- extract_basic_summary_long(data)

print(summary_long_table)
