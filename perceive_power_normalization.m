function [spow, rpow] = perceive_power_normalization(pow, f, lim)

    % Set default frequency limits if 'lim' is not provided
    if ~exist('lim', 'var')
        lim = [5 45 55 95];
    end

    
    % Determine frequency range based on 'lim'
    if size(lim, 2) == 4 && size(lim, 1) == 1
        frange = [perceive_sc(f, lim(1)):perceive_sc(f, lim(2)), perceive_sc(f, lim(3)):perceive_sc(f, lim(4)) ];
    elseif size(lim, 1) > 1
        frange = [];
        for a = 1:size(lim, 1)
            frange = [frange, perceive_sc(f, lim(a,1 )):perceive_sc(f, lim(a, 2)) ];
        end
    else
        frange = lim;
    end
    
    % Normalize power spectra by dividing by the standard deviation
    if length(size(pow)) == 2
        for a = 1:size(pow, 1)
            spow(a, :) = bsxfun(@rdivide, pow(a,:), nanstd(pow(a, frange)));
        end
    else
        for a = 1:size(pow, 1)
            for b = 1:size(pow, 3)
                spow(a, :, b) = bsxfun(@rdivide, pow(a, :, b), nanstd(pow(a, frange, b)));
            end
        end
    end

    % Normalize power spectra by dividing by the sum
    if length(size(pow)) == 2
        for a = 1:size(pow, 1)
            rpow(a,:) = bsxfun(@rdivide, pow(a,:), nansum(pow(a, frange)));
        end
    else
        for a = 1:size(pow, 1)
            for b = 1:size(pow, 3)
                rpow(a, : , b) = bsxfun(@rdivide, pow(a, :, b), nansum(pow(a, frange, b)));
            end
        end
    end

    % Convert rpow to percentage
    rpow = rpow .* 100;
end