function [files, folder, fullname] = perceive_ffind(string, cell, rec)
    % Set default values for optional arguments if not provided
    if ~exist('cell', 'var')
        cell = 1;
    end

    if ~exist('rec', 'var')
        rec = 0;
    end 

    % Perform non-recursive file search
    if ~rec
        x = ls(string);
        if size(x,1) > 1 
            files = cellstr(ls(string));
        else
            files = strsplit(x);
            if ~isempty(files)
                nonempty = repmat(true, 1, length(files));
                for i = 1:length(files)
                    if isempty(files{i})
                        nonempty(i) = false;
                    end
                end
                files = files(nonempty);
            end
        end

        % Retrieve folder paths
        for a = 1:length(files)
            ff = fileparts(string);
            if ~isempty(ff)
                folder{a} = ff;
            else
                folder{a} == cd;
            end
        end


     % Perform recursive file search
    else
        rdirs = find_folders;
        outfiles = perceive_ffind(string, 1, 0);
        outfolders = {};
        folders = {};

        for a = 1:length(outfiles)
            outfolders{a} = cd;
        end

        for a = 1:length(rdirs)
            files = perceive_ffind([rdirs{a} filesep string], 1, 0);
            if ~isempty(files)
                for b = 1:length(files)
                    folders{b,1} = [rdirs{a}];
                end
                outfiles = [outfiles; files];
                outfolders = [outfolders; folders];
            end
        end
        files = outfiles;
        folder = outfolders;
    end

    % Remove '.' and '..' entries from the file list
    ris = logical(sum( [ismember(files, '.'), ismember(files, '..')], 2));
    files(ris) = [];
    folder(ris) = [];
    [files, x] = unique(files);
    folder = folder(x);

    % Prepare full file names
    if ~isempty(files)
        if ~cell && length(files) == 1
            files = files{1};
            fullname = [folder{1}, filesep, files];
        elseif iscell(files) && isempty(files{1})
            files = [];
            folder = [];
            fullname = [];
        elseif iscell(files)
            for a = 1:length(files)
                fullname{a, 1} = [folder{a}, filesep, files{a}];
            end
        end
    else
        folder = [];
        fullnames = [];
    end
end
