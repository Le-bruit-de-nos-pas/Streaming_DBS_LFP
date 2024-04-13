function [pow, f, rpow, lpow] = perceive_fft(data, fs, tw)
    
    % Check if 'tw' (time window) is provided, otherwise set it to 'fs'
    if ~exist('tw', 'var')
        tw = fs;
    end

     % Iterate through each row in 'data'
     for a = 1:size(data, 1)
        % Compute power spectral density using Welch's method
        % applies a Hanning window to the data, with the window length rounded to the nearest integer
        % sets the overlap between segments to be half the window length
        %  returns the power spectral density (pow) and the corresponding frequencie
        [pow(a,:), f] = pwelch(data(a,:), hanning(round(tw)), round(tw*0.5), round(tw) fs);
         
        % Compute relative power in specific frequency bands
        % uses the function perceive_sc to find the indices corresponding to frequencies between 5-45 Hz and 55-95 Hz
        % sums the power spectral density in these frequency bands and calculates the relative power as a percentage
        rpow(a,:) = 100 .* pow(a,:) ./ sum(pow(a, perceive_sc(f, [5:45 55:95])));
        
         % Try to fit power spectrum to a logarithmic function
        try
            lpow(a,:) = perceive_fftlogfitter(f, pow(a,:));
        end
    end
end
        