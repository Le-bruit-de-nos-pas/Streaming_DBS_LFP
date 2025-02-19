# Load the required library
library(signal)
library(ggplot2)


# Function to plot the spectrogram
plotSpectrogram <- function(dataset) {
  
  # Extract the time-domain signals and sampling rates
  time_domain_data <- dataset$LfpMontageTimeDomain$TimeDomainData

  sample_rates <- dataset$LfpMontageTimeDomain$SampleRateInHz

  
  # Define spectrogram parameters (common for all, but we'll adjust Fs)
  window_fraction <- 0.5  # Window size as a fraction of the sampling rate
  fmin <- 1  # Min frequency for spectrogram
  fmax <- 125  # Max frequency for spectrogram
  
  # Plot spectrogram for each time-domain signal (i.e., each sequence)
  for (i in 1:length(time_domain_data)) {
    
    signal <- time_domain_data[[i]]
    Fs <- sample_rates[i]  # Get the specific sampling rate for the current sequence
    
    # Define window and overlap based on sampling rate
    window <- round(window_fraction * Fs)  # Window size: 0.5 seconds
    noverlap <- round(window / 2)  # 50% overlap
    
    # Compute spectrogram using the specgram function from the signal package
    spec <- specgram(signal, n = window, Fs = Fs, window = hamming(window), overlap = noverlap)
    
    # Extract the magnitude of the spectrogram (S)
    power <- abs(spec$S)  # Take the magnitude of the FFT results (complex numbers)
    
    # Convert power to dB (log scale)
    power_db <- 10 * log10(power)
    
    # Prepare data for ggplot
    time_points <- spec$t
    frequencies <- spec$f
    spectro_data <- expand.grid(Time = time_points, Frequency = frequencies)
    spectro_data$Power <- as.vector(power_db)  # Power in dB
    
    # Create a ggplot of the spectrogram
    p <- ggplot(spectro_data, aes(x = Time, y = Frequency, fill = Power)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = paste("Spectrogram for Pass", i), x = "Time (s)", y = "Frequency (Hz)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Display the plot
    print(p)
  }
}

# Example call to plot the spectrogram
plotSpectrogram(dataset)  # Assuming `dataset` is already loaded with your data
