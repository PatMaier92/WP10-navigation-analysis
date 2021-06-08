function sm_wp10_table_allTrials_post(folderIn, new_file, col_header, col_header_2)
% SM_WP10_TABLE_ALLTRIALS_post Write table for all trials of the current ID 
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
    x=['A' row ':X' row];
    
    % reading in data for table per ID
    d_data=xlsread(fullfile(folderIn, name),'post_vars','A2:X2'); 
    
    % header
    xlswrite(new_file,[col_header col_header_2 ],'post_vars','A1');

    % data
    xlswrite(new_file,d_data,'post_vars',x);
end 

end