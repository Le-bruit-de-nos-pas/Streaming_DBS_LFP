function  [tf, t, f] = perceive_raw_tf(data, fs, timewindow, timestep)

    % Calculate the number of FFT points based on timewindow and sampling frequency
    nfft = round(timewindow / fs * 1000);
    %  Convert timestep from seconds to milliseconds
    timestep = round(timestep / fs * 1000); 

    % Compute the spectrogram of the input data
    % uses a Hanning window of length nfft (window length for FFT) to window the data before computing the FFT
    % window is shifted by nfft - timestep samples between consecutive segments
    % parameters 'yaxis' and 'power' specify that the frequency axis is plotted along the y-axis and the output should be in power spectral density
    %The function returns four outputs: S, f, t, and P.
    % S is the spectrogram intensity (discarded using the tilde ~)
    % f contains the frequencies
    % t contains the time instants
    % P is a matrix containing the power spectral density
    [~, f, t, tf] = spectrogram(data, hann(nfft), nfft-timestep, nfft, fs, 'yaxis', 'power');
    
end
