function sm_wp10_table_allTrials(folderIn, new_file, col_header, col_header_2, col_header_3, column1, column2, sheet1, sheet2)
% SM_WP10_TABLE_ALLTRIALS Write table for all trials of the current ID 
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
    r1=['A' row ':' column1 row];
    r2=['A' row ':' column2 row];
    
    % reading in data for table per ID
    range1=['A2:' column1 '2'];
    d_data=xlsread(fullfile(folderIn, name),sheet1,range1); 
    range2=['A2:' column2 '2'];
    d_support=xlsread(fullfile(folderIn, name),sheet2,range2);
    
    % header
    xlswrite(new_file,[col_header col_header_2 ],sheet1,'A1');
    xlswrite(new_file,[col_header col_header_3 ],sheet2,'A1');

    % data
    xlswrite(new_file,d_data,sheet1,r1);
    xlswrite(new_file,d_support,sheet2,r2);
end 

end