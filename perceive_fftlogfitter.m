function [nyd, nyl] = perceive_fftlogfitter(f, data, flow, fhigh)
    % Fits a logarithmic function to the power spectrum
    % % Data should be in channels x frequencies

    % Check if 'flow' and 'fhigh' are provided, otherwise set default values
    if exist('flow', 'var')
        flowl = flow(1);
        flowh = flow(2);
    else
        flowl = 3;
        flowh = 5;
    end
    if exist('fhigh', 'var')
        fhighl = fhigh(1);
        fhighh = fhigh(2);
    else
        fhighl = 35;
        fhighh = 40;
    end

    % Initialize output variables
    nyd = zeros(length(f), size(data, 1));
    nyl = zeros(size(data));

    % Loop through each channel in the data
    for a = 1:size(data, 1)
        y = data(a, :);
        fl = log(f);
        yl = log(y)';

        % Select frequency range for fitting
        fvector = [fl(find(round(f) == flowl):find(round(f) == flowh)); fl(find(round(f) == fhighl):find(round(f) == fhighh))];
        yvector = [yl(find(round(f) == flowl):find(round(f) == flowh)); yl(find(round(f) == fhighl):find(round(f) == fhighh))];

        nyl=exp(polyval(polyfit(fvector,yvector,1),fl));
        ny=exp(yl-polyval(polyfit(fvector,yvector,1),fl));
        
        % Store the denoised power spectrum
        nyd(:, a) = ny;
    end
end