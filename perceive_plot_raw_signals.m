function [raw, time, chanlabels, p] = perceive_plot_raw_signals(data, chanlabels, time)
    
    % Check if 'data' is provided or prompt user to select a file
    if ~exist('data', 'var') || isempty(data)
        [files, path] = uigetfile('*.mat', 'Select exported percept fieldtrip .mat file', 'MultiSelect', 'on');
        files = strcat(path, files);
    else
        files = data;
    end

    % Iretate through each file
    for a = 1:length(files)
        % Load data if files is a cell array
        if iscell(files)
            load(files{a})
        end

        %  Extract data from the struct or assign default values
        if isstruct(data)
            if isfield(data, 'realtime')
                if iscell(data.realtime)
                    time = data.realtime{1};
                else
                    time = data.realtime;
                end
            else 
                time = data.time{1};
            end
            chanlabels = data.labels;
            fname = data.fname;
            fs = data.fsample,
            raw = data.trial{1};
        else
            raw = data;
            fname = '';
            fs = 250;
            if ~exist('time', 'var') || isempty(time)
                time = linspace(0, length(data)/fs, length(data));
            end
        end

        % Assign default channel labels if not provided
        if ~exist('chanlabels', 'var')
            for a = 1:size(data, 1)
                chanlabels{a} = ['chan', num2str(a)];
            end
        end

        % Preprocess raw data (replace NaN with 0)
        raw(isnan(raw)) = 0;

        % Plot raw signals
        figure('Units', 'centimeters', 'PaperUnits', 'centimeters', 'Position', [1 1 40 20])
        for b = 1:size(raw, 1)
            p(b) = plot(time, zscore(raw(b, :)')'./10 + b);
            hold on
        end

        % Customize the plot
        set(gca, 'YTick', [1:size(raw, 1)], 'YTickLabel', strrep(chanlabels, '_', ' '), 'YTickLabelRotation', 45);
        xlabel('Time')
        ylabel('Amplitude')
        if length(time) > 1
            xlim([time(1), time(end)]);
        end
        ylim([0, length(chanlabels) + 1]);
        title(strrep(fname, '_', ' '))
    end
    
end