library(dplyr)

# Function to extract and summarize impedance data
extract_impedance_summary <- function(data) {
  
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
      combined_data <- rbind(monopolar, bipolar)
      return(combined_data)
    }
    
    # Step 2: Extract Left and Right Hemisphere Data
    left_data <- impedance_data[sapply(impedance_data, function(x) x$Hemisphere == "HemisphereLocationDef.Left")]
    right_data <- impedance_data[sapply(impedance_data, function(x) x$Hemisphere == "HemisphereLocationDef.Right")]
    
    # Apply function (only if data exists)
    left_impedance <- if (length(left_data) > 0) extract_impedance(left_data[[1]], "Left") else NULL
    right_impedance <- if (length(right_data) > 0) extract_impedance(right_data[[1]], "Right") else NULL
    
    # Step 3: Combine left and right hemisphere data
    combined_impedance_df <- rbind(left_impedance, right_impedance)
    
    # If we have combined impedance data
    if (!is.null(combined_impedance_df)) {
      
      # Step 4: View the Combined Data
      print(head(combined_impedance_df))
      
      # Step 5: Summarize Impedance by Hemisphere and Type
      impedance_summary <- combined_impedance_df %>%
        group_by(Hemisphere, Type) %>%
        summarize(MeanImpedance = mean(ResultValue, na.rm = TRUE), .groups = "drop")
      
      # Print the summary
      print(impedance_summary)
      
      # Return both the combined data and summary
      return(list(
        "combined_impedance_df" = combined_impedance_df,
        "impedance_summary" = impedance_summary
      ))
      
    } else {
      print("No valid impedance data available for this patient.")
    }
    
  } else {
    print("Impedance data is missing for this patient.")
  }
}

# Example Usage
impedance_results <- extract_impedance_summary(data)

# View combined impedance data
head(impedance_results$combined_impedance_df)

# View summarized impedance data
print(impedance_results$impedance_summary)

impedance_results$combined_impedance_df
impedance_results$impedance_summary
