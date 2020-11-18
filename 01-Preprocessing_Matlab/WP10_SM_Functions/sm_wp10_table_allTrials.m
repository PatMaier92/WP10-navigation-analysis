function sm_wp10_table_allTrials(folderIn, new_file, col_header, col_header_2, col_header_3)
% SM_WP10_TABLE_ALLTRIALS Write table for all trials of the current ID 
% for Starmaze WP10. Save in individual folder. 
%
% Input:
% Folder path, file name for saving the file, sheet names. 
%
% Returns: Writes a summary file.

d = dir(fullfile(folderIn, '*.xls')); % every .xlsx is detected
files = {d.name};
for k=1:numel(files)
    name = files{k};
    row=k+1;
    row=int2str(row);
    x=['A' row ':AZ' row];
    y=['A' row ':CR' row];
    
    % reading in data for table per ID
    d_data=xlsread(fullfile(folderIn, name),'data_vars','A2:AZ2'); 
    d_support=xlsread(fullfile(folderIn, name),'support_vars','A2:CR2'); %%
    
    % header
    xlswrite(new_file,[col_header col_header_2 ],'data_vars','A1');
    xlswrite(new_file,[col_header col_header_3 ],'support_vars','A1');

    % data
    xlswrite(new_file,d_data,'data_vars',x);
    xlswrite(new_file,d_support,'support_vars',y);
end 

end