function perceive_print(filename)
    % Extract folder path, file name, and file extension
    [fold, file, ext] = fileparts(filename);

    % Create folder if it doesn't exist
    if ~exist(fold, 'dir')
        mkdir(fold);
    end
  
    % print figure to a PNG file with specific settings
    print(gcf, fullfile(fold, file), '-dpng', '-r300', '-opengl');
    % save the current orientation to reset it to default later

    or = get(gcf, 'PaperOrientation');
    % Set landscape orientation for pdf print

    set(gcf, 'PaperOrientation', 'Landscape');
    % Print figure to pdf with specific settings to make figure visible

    print(gcf, fullfile(fold, file), '-dpdf', '-r80', '-bestfit');
    
    % restore the original orientation
    set(gcf, 'PaperOrientation', or);
end