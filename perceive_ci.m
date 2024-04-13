function [channelindex, channelnames] = perceive_ci(channels, possible_list, exact)
    
    % Check if 'exact' is provided, otherwise set it to 0
    if ~exist('exact', 'var')
        exact = 0;
    end

     % Convert 'channels' to cell array if it's a character array
     if ischar(channels)
        channels = {channels};
     end

     x = []
     % Iterate through each channel in 'channels'
     for a = 1:length(channels)
         % Iterate through each possible list in 'possible_list'
        for b = 1:length(possible_list)
            % Determine the number of characters to search
            if ~exact
                nsearch = length(possible_list{b});
            elseif exact > 0
                nsearch = 1;
            end
            % Iterate through the characters in 'possible_list'
            for c = 1:nsearch
                % Compare 'channels' with 'possible_list' based on 'exact'
                if exact == 1
                    x(a, b, c) = strcmpi(channels{a}, possible_list{b}(c:end));
                else
                    x(a, b, c) = strncmpi(channels{a}, possible_list{b}(c:end), length(channels{a}));
                end
            end
        end
    end

    % Sum the matches across the third dimension
    sx = sum(x, 3);
    i = [];
    % Iterate through each channel in 'channels' again
    for a = 1:length(channels)
        % Find the indices where matches occur
        index = find(sx(a, :));
        i = [i index];
        index = [];
    end

    % Get the corresponding channel names from 'possible_list'
    channelnames = possible_list(i);

    % Check for duplicates in 'i'
    if numel(i) ~= numel(unique(i))
        warning('Duplicates found and erased');
        % Keep unique indices while maintaining their order
        channelindex = unique(i, 'stable');
    else
        channelindex = i;
    end
end

                
