%% Script for creating group data file 
% Takes individual data and creates one group file

%% Preparation
addpath(genpath(pwd)) % add subfolder functions to path

%% Specify data folder
[dataFolder]=sm_inputPath();

folder=[dataFolder '\WP10_results'];
d = dir(fullfile(folder, '*.xls')); % every .xls is detected
files = {d.name}; 
formatOut='yymmdd';
date=datestr(now, formatOut);

new_name1 = ['WP10_results_table_' date];
new_file1 = fullfile(folder, [new_name1 '.xlsx']);

%% Write data sheets 
for k=1:numel(files)
    name = files{k}; 
       name = strrep(name,'xls','xls');
       new_file = fullfile(folder, name);
    data=readtable(new_file,'Sheet',2,'Range','A:AL');  
    table=vertcat(table,data);
 end

writetable(table,new_file1,'Sheet','path');

    clear table data
    
for k=1:numel(files)
    name = files{k}; 
       new_name2 = strrep(name,'xls','xls');
       new_file = fullfile(folder, new_name2);
    data=readtable(new_file,'Sheet',3,'Range','A:BV');  
    table=vertcat(table,data);
 end
 writetable(table,new_file1,'Sheet','zone');
 clear table data   
    
 for k=1:numel(files)
    name = files{k}; 
       new_name2 = strrep(name,'xls','xls');
       new_file = fullfile(folder, new_name2);
    data=readtable(new_file,'Sheet',4,'Range','A:AL');  
    table=vertcat(table,data);
 end
 writetable(table,new_file1,'Sheet','exploration');
 clear table data

  
%  for k=1:numel(files)
%     name = files{k}; 
%        new_name2 = strrep(name,'xls','xls');
%        new_file = fullfile(folder, new_name2);
%     data=readtable(new_file,'Sheet',5,'Range','A:AL');  
%     table=vertcat(table,data);
%  end
%  writetable(table,new_file1,'Sheet','summary_path');
%  clear table data
%  
%  for k=1:numel(files)
%     name = files{k}; 
%        new_name2 = strrep(name,'xls','xls');
%        new_file = fullfile(folder, new_name2);
%     data=readtable(new_file,'Sheet',12,'Range','A:AL');  
%     table=vertcat(table,data);
%  end
%  writetable(table,new_file1,'Sheet','summary_path_allo');
%  clear table data
%  
%      for k=1:numel(files)
%     name = files{k}; 
%        new_name2 = strrep(name,'xls','xls');
%        new_file = fullfile(folder, new_name2);
%     data=readtable(new_file,'Sheet',6,'Range','A:AL');  
%     table=vertcat(table,data);
%  end
%  writetable(table,new_file1,'Sheet','summary_path_ego');
%  clear table data
%  
%      for k=1:numel(files)
%     name = files{k}; 
%        new_name2 = strrep(name,'xls','xls');
%        new_file = fullfile(folder, new_name2);
%     data=readtable(new_file,'Sheet',7,'Range','A:AL');  
%     table=vertcat(table,data);
%  end
%  writetable(table,new_file1,'Sheet','summary_path_training');
%  clear table data
%  
%  for k=1:numel(files)
%     name = files{k}; 
%        new_name2 = strrep(name,'xls','xls');
%        new_file = fullfile(folder, new_name2);
%     data=readtable(new_file,'Sheet',8,'Range','A:BV');  
%     table=vertcat(table,data);
%  end
%  writetable(table,new_file1,'Sheet','summary_exploration');
%  clear table data
%  
%  for k=1:numel(files)
%     name = files{k}; 
%        new_name2 = strrep(name,'xls','xls');
%        new_file = fullfile(folder, new_name2);
%     data=readtable(new_file,'Sheet',9,'Range','A:BV');  
%     table=vertcat(table,data);
%  end
%  writetable(table,new_file1,'Sheet','summary_exploration_allo');
%  clear table data
%  
%  for k=1:numel(files)
%     name = files{k}; 
%        new_name2 = strrep(name,'xls','xls');
%        new_file = fullfile(folder, new_name2);
%     data=readtable(new_file,'Sheet',10,'Range','A:BV');  
%     table=vertcat(table,data);
%  end
%  writetable(table,new_file1,'Sheet','summary_exploration_ego');
%  clear table data
%  
%  for k=1:numel(files)
%     name = files{k}; 
%        new_name2 = strrep(name,'xls','xls');
%        new_file = fullfile(folder, new_name2);
%     data=readtable(new_file,'Sheet',11,'Range','A:BV');  
%     table=vertcat(table,data);
%  end
%  writetable(table,new_file1,'Sheet','summary_exploration_training');

 clear table data
 
 clear
