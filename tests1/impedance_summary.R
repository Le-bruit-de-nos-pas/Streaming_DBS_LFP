library(dplyr)

# Function to extract and summarize impedance dataset
impedance_summary <- function(dataset) {

  # Step 1: Check if Impedance dataset Exists
  if (!is.null(dataset$Impedance) && !is.null(dataset$Impedance$Hemisphere)) {

    impedance_dataset <- dataset$Impedance$Hemisphere  # Full list

    # Function to extract and clean impedance dataset
    extract_impedance <- function(hemisphere_dataset, label) {
      if (is.null(hemisphere_dataset$SessionImpedance)) return(NULL)  # Skip if missing

      # Extract Monopolar if it exists
      if (!is.null(hemisphere_dataset$SessionImpedance$Monopolar)) {
        monopolar <- do.call(rbind, hemisphere_dataset$SessionImpedance$Monopolar)
        monopolar$Type <- "Monopolar"
        monopolar$Hemisphere <- label
      } else {
        monopolar <- NULL
      }

      # Extract Bipolar if it exists
      if (!is.null(hemisphere_dataset$SessionImpedance$Bipolar)) {
        bipolar <- do.call(rbind, hemisphere_dataset$SessionImpedance$Bipolar)
        bipolar$Type <- "Bipolar"
        bipolar$Hemisphere <- label
      } else {
        bipolar <- NULL
      }

      # Combine Monopolar and Bipolar dataset
      combined_dataset <- rbind(monopolar, bipolar)
      return(combined_dataset)
    }

    # Step 2: Extract Left and Right Hemisphere dataset
    left_dataset <- impedance_dataset[sapply(impedance_dataset, function(x) x$Hemisphere == "HemisphereLocationDef.Left")]
    right_dataset <- impedance_dataset[sapply(impedance_dataset, function(x) x$Hemisphere == "HemisphereLocationDef.Right")]

    # Apply function (only if dataset exists)
    left_impedance <- if (length(left_dataset) > 0) extract_impedance(left_dataset[[1]], "Left") else NULL
    right_impedance <- if (length(right_dataset) > 0) extract_impedance(right_dataset[[1]], "Right") else NULL

    # Step 3: Combine left and right hemisphere dataset
    combined_impedance_df <- rbind(left_impedance, right_impedance)

    # If we have combined impedance dataset
    if (!is.null(combined_impedance_df)) {

      # Step 4: View the Combined dataset
      print(head(combined_impedance_df))

      # Step 5: Summarize Impedance by Hemisphere and Type
      impedance_summary <- combined_impedance_df %>%
        group_by(Hemisphere, Type) %>%
        summarize(MeanImpedance = mean(ResultValue, na.rm = TRUE), .groups = "drop")

      # Print the summary
      print(impedance_summary)

      # Return both the combined dataset and summary
      return(list(
        "combined_impedance_df" = combined_impedance_df,
        "impedance_summary" = impedance_summary
      ))

    } else {
      print("No valid impedance dataset available for this patient.")
    }

  } else {
    print("Impedance dataset is missing for this patient.")
  }
}

# Example Usage
impedance_results <- impedance_summary(dataset)

# View combined impedance dataset
head(impedance_results$combined_impedance_df)

# View summarized impedance dataset
print(impedance_results$impedance_summary)

impedance_results$combined_impedance_df
impedance_results$impedance_summary
