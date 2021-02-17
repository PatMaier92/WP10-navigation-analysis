function sm_wp10_dataPrep(folderIn,ID,Group, Session)
% SM_WP10_DATAPREP Data preparation, cleaning, re-saving of trial tracker files. 
%
% Input: 
% folderIn is input folder path (string)
% ID is participant identifier (integer)
% Group is group condition (string)
% Session is session number (integer)
%
% Returns: Saves cleaned files as new, renamed .xlsx files.  

% current folder & files
d = dir(fullfile(folderIn, '*.csv')); % every .csv is detected
files = {d.name};

% adjust data for later analysis
for k=1:numel(files)
oldname = files{k}; 

% jump unnecessary files
jump1 = strfind(oldname,'trial_results');
jump2 = strfind(oldname,'log');

if (~isempty(jump1))||(~isempty(jump2))
    continue
end
    
% read-in data into cell-array a, split and reshape into cell-array c
[~,~,a] = xlsread(fullfile(folderIn, oldname));
        b=regexp(a,',','split');
        c=reshape([b{:}],numel(b{1}),[])';
        % delete rows
        c(1:3,:)=[]; % delete first three rows to get rid of header and info from last trial
        c(strcmp(c(:,5),'True'),:) = []; % remove gameIsPaused == True
        if Session~="3"
            c(strcmp(c(:,6),'1'),:) = []; % trialEvent ausschneiden, not for motor control task (session 3)
        end
        % delete columns 
        c(:,8)=[]; % delete 8th column, containing gameIsPaused
        c(:,7)=[]; % delete 7th column, containing z-rotation
        c(:,5)=[]; % delete 5th column, containing x-rotation
        c(:,3)=[]; % delete 3rh column, containing y-movement (upwards) 
        % delimiter 
        c = strrep(c, '.', ','); % comma, not points as decimal character 
    new_name = strrep(['dt_' oldname],'csv','xlsx');
    new_file = fullfile(folderIn, [Group '_' ID '_' Session '_' new_name]);

% save 
xlswrite(new_file,c);

end
end
    
