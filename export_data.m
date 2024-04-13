% Load the .mat file
data = load('your_file.mat');

% Extract the 'time' and 'trial' fields from the struct
time_data = data.data.time;
trial_data = data.data.trial;

% Convert cell arrays to arrays
time_data = cell2mat(time_data);
trial_data = cell2mat(trial_data);

% Transpose the arrays if needed
% time_data = time_data';  % Uncomment if transpose is needed
% trial_data = trial_data'; % Uncomment if transpose is needed

% Write the data to a text file
dlmwrite('time_data.txt', time_data, 'delimiter', ' ', 'precision', '%.6f');
dlmwrite('trial_data.txt', trial_data, 'delimiter', ' ', 'precision', '%.6f');