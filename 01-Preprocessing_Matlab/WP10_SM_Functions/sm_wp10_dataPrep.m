function sm_wp10_dataPrep(folderIn,ID,Group, Session)
% SM_WP10_DATAPREP Data preparation, cleaning, re-saving of trial tracker files. 
%
% Input: 
% folderIn is input folder path (string)
% ID is participant identifier (integer)
% Group is group condition (string)
% Session is session number (integer)
%
% Returns: Cleanes and saves files as .xlsx

% current folder & files
d = dir(fullfile(folderIn, '*.csv')); % every .csv is detected
files = {d.name};

% adjust data for later analysis
for k=1:numel(files)
oldname = files{k}; 

% jump trial_results and log file
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
        c(2:3,:)=[]; % delete first two data rows to get rid of info from last trial
        c(strcmp(c(:,5),'True'),:) = []; % remove rows when gameIsPaused == True
        if Session~=3 % skip this for Motor Control Task
            c(strcmp(c(:,6),'1'),:) = []; % remove row when trialEvent == 1 (i.e. after goal was found)
        end
        % delete columns 
        c(:,8)=[]; % delete 8th column, containing gameIsPaused 
        c(:,7)=[]; % delete 7th column, containing z-rotation
        c(:,5)=[]; % delete 5th column, containing x-rotation
        c(:,3)=[]; % delete 3rh column, containing y-movement (upwards in Unity3D) 
        % change delimiter (compatibility with German Excel)
        c = strrep(c, '.', ','); % comma, not points as decimal character
    new_name = strrep(['dt_' oldname],'csv','xlsx');
    new_file = fullfile(folderIn, [Group '_' num2str(ID) '_' num2str(Session) '_' new_name]);

% save 
xlswrite(new_file,c);

end
end
    
