function sm_wp10_table_allTrials(folderIn, new_file, col_header, col_header_2, col_header_3, col_header_4)
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
    x=['A' row ':AD' row];
    y=['A' row ':AI' row];
    z=['A' row ':CR' row];
    
    % reading in data for table per ID
    d_path=xlsread(fullfile(folderIn, name),'path','A2:AD2'); 
    d_explore=xlsread(fullfile(folderIn, name),'exploration','A2:AI2');
    d_support=xlsread(fullfile(folderIn, name),'support_vars','A2:CR2');
    
    % header
    xlswrite(new_file,[col_header col_header_2 ],'path','A1');
    xlswrite(new_file,[col_header col_header_3 ],'exploration','A1');
    xlswrite(new_file,[col_header col_header_4 ],'support_vars','A1');

    % data
    xlswrite(new_file,d_path,'path',x);
    xlswrite(new_file,d_explore,'exploration',y);
    xlswrite(new_file,d_support,'support_vars',z);
end 

end