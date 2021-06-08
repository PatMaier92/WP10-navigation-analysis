function sm_wp10_table_allTrials_post(folderIn, new_file, col_header, col_header_2, column, sheet)
% SM_WP10_TABLE_ALLTRIALS_post Write table for all trials of the current ID 
% for Starmaze WP10. Save in individual folder. 
%
% Input:
% Folder path, file name for saving the file, sheet names. 
%
% Returns: Writes a summary file.

d=dir(fullfile(folderIn, '*.xls')); % every .xlsx is detected
files={d.name};

for k=1:numel(files)
    name = files{k};
    row=k+1;
    row=int2str(row);
    r=['A' row ':' column row];
    
    % reading in data for table per ID
    range=['A2:' column '2'];
    d_data=xlsread(fullfile(folderIn, name),sheet,range); 
    
    % header
    xlswrite(new_file,[col_header col_header_2 ],sheet,'A1');

    % data
    xlswrite(new_file,d_data,sheet,r);
end 

end