%% Script for creating group data file 
% Takes individual data and creates one group file

%% Preparation
addpath(genpath(pwd)) % add subfolder functions to path

%% Specify data folder
[dataFolder]=sm_inputPath();

folder=[dataFolder '\WP10_results\PostTest'];
d = dir(fullfile(folder, '*.xls')); % every .xls is detected
files = {d.name}; 
formatOut='yymmdd';
date=datestr(now, formatOut);

new_name1 = ['WP10_post_results_table_' date];
new_file1 = fullfile(folder, [new_name1 '.xlsx']);

%% Write data sheets 
for k=1:numel(files)
    name = files{k}; 
       name = strrep(name,'xls','xls');
       new_file = fullfile(folder, name);
    data=readtable(new_file,'Sheet',2,'Range','A:X');  
    table=vertcat(table,data);
end
writetable(table,new_file1,'Sheet','post_vars');
clear table data
    
 clear
