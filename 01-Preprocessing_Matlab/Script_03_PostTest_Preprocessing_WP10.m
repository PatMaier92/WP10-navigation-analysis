%% Preparation
clear; close all; clc; format long;
addpath(genpath(pwd)) % add subfolder functions to path

%% PostTest Data Processing
% @ date 20210521 update by @ Patrizia Maier & tracked by git 
% for starmaze version WP10 Frankfurt

% The script requires .csv as input-files. 
% for trial info trial_results.csv is required
% TBD: GMDA files 

% Script starts with input: 
%   1. Provide the span of subjects you would like to analyse, start with
%   the first subjectnumber, end with the last subjectnumber (Attention:
%   the folder with S001 has to be in a folder that is named after the
%   subjectnumber)

%% Block 1: Input/output

%% Get folders and subject list 
% folder with all input data 
[dataFolder]=sm_inputPath(); 

% folder for output results 
folderOut2=[dataFolder '\WP10_results'];
if ~exist(folderOut2, 'dir')
    mkdir(folderOut2);
    disp('Your outputfolder for results didn''t exist, a new folder was created')
end

% load/save data
targetFileName         = '\wp10_results_table_posttest.mat';
if isfile(fullfile(folderOut2, targetFileName))
    load(fullfile(folderOut2, targetFileName))
end
save(fullfile(folderOut2, targetFileName));
 
if ~exist('pt.sub')
    pt.sub={};
end
[row,participant]=size(pt.sub);
p=participant+1;

[subject_start,subject_end]=sm_inputSubjectsNo();
session=4; % session default

wp=10; % work package default 
 
%% Block 2: Data analysis
for subject=subject_start:subject_end
    pstr=['p',num2str(subject)];
    sstr=['s',num2str(session)];
    [finalFolderString]=sm_wp10_getFolderstring(session);
    [folderIn,folderOut]=sm_wp10_folder(dataFolder,subject,finalFolderString);
    % save data
    targetFileName_Subject         = ['\' num2str(subject) '_results_table_post.mat'];
    save(fullfile(folderOut, targetFileName_Subject));

    % read-in trial file 
    opts = detectImportOptions([folderIn, '\trial_results.csv']);
    opts = setvaropts(opts,'timestamp','InputFormat','MM/dd/uuuu hh:mm:ss aa'); 
    trial_data=readtable([folderIn, '\trial_results.csv'],opts);
    
    %% Participant,  Workpackage, Group information
    s=session; % s=session --> row, p=participant --> coloumn
    pt.sub{p}.session{s}.session=s;
    pt.sub{p}.wp=wp; 
    pt.sub{p}.id=subject;
    [pt.sub{p}.group,pt.sub{p}.Group,pt.sub{p}.sex,pt.sub{p}.Sex]=sm_wp10_callGroup(pt.sub{p}.id); % provide group information
    pt.sub{p}.session{s}.session_duration=round(minutes(trial_data.timestamp(4,1) - trial_data.timestamp(1,1))); % provide session duration

    pt.sub{p}.session{s}.lm_MB=0;
    pt.sub{p}.session{s}.lm_MD=0;
    pt.sub{p}.session{s}.lm_MF=0;
    pt.sub{p}.session{s}.lm_MH=0;
    pt.sub{p}.session{s}.lm_MJ=0;
    pt.sub{p}.session{s}.obj_MA=0;
    pt.sub{p}.session{s}.obj_MC=0;
    pt.sub{p}.session{s}.obj_MI=0;

%% Trial-wise data analysis
for k=1:size(trial_data,1)
    %% General info
    pt.sub{p}.session{s}.trial{k}.trial_num=trial_data.trial_num(k,1);
    pt.sub{p}.session{s}.trial{k}.trial_type=trial_data.trial_num(k,1); % same as trial number, string info in R
    
    %% Time analysis using timestamp
    b=trial_data.end_time(k,1); a=trial_data.start_time(k,1);
    pt.sub{p}.session{s}.trial{k}.result.time=sm_time(a,b); % total amount of time

    fprintf('Time analysis done for %d, session %d, file no %d.\n', subject, session, k);

    %% Performance analysis
    if k==1
        % shape recognition
        if trial_data.suc_1{k,1}=="True"
            points=1;
        else 
            points=0;
        end
        
        pt.sub{p}.session{s}.trial{k}.result.score=points;
            
        fprintf('Shape recognition done for %d, session %d, file no %d.\n', subject, session, k);
    elseif k==2
        % landmark recogntion
        % setup
        points=0; 
        corr_list={ 'Mountain-House', 'Forest-House', 'Tower', 'Forest', 'Mountain' };
            
        % get chosen landmarks
        obj_1=trial_data.obj_1{k,1}(4:end);
        obj_1=strsplit(obj_1, '_');

        obj_2=trial_data.obj_2{k,1}(4:end);
        obj_2=strsplit(obj_2, '_');

        obj_3=trial_data.obj_3{k,1}(4:end);
        obj_3=strsplit(obj_3, '_');

        obj_4=trial_data.obj_4{k,1}(4:end);
        obj_4=strsplit(obj_4, '_');

        obj_5=trial_data.obj_5{k,1}(4:end);
        obj_5=obj_5(4:end);
        obj_5=strsplit(obj_5, '_');

        % save in cell list 
        obj_list=[obj_1; obj_2; obj_3; obj_4; obj_5]; 
        
        % score 1 point for correct item
        log_cor=contains(obj_list(:,2), 'corr');
        points=points+sum(log_cor);
        
        % remove correct items from list --> similar items only score points
        % if not same semantic category 
        corr_list(ismember(corr_list,obj_list(log_cor,1))) = [];
        
        % score 0.5 points for similar but semantically distinct items 
        % no points for dissimilar items 
        points=points+(sum(ismember(obj_list(:,1), corr_list) & contains(obj_list(:,2), 'sim'))/2);
        
        % calculate average 
        points=points/5; 

        % save 
        pt.sub{p}.session{s}.trial{k}.result.score=points;
        
        fprintf('Landmark recognition done for %d, session %d, file no %d.\n', subject, session, k);
    elseif k==3
        % goal recognition
        % setup
        points=0; 
        corr_list={ trial_data.obj_MA{k,1}, trial_data.obj_MC{k,1}, trial_data.obj_MI{k,1} };
            
        % get chosen goals
        obj_1=trial_data.obj_1{k,1};

        obj_2=trial_data.obj_2{k,1};

        obj_3=trial_data.obj_3{k,1};

        % save in cell list 
        obj_list={ obj_1; obj_2; obj_3 }; 
        
        % score 1 point for correct item
        log_cor=contains(obj_list, corr_list);
        points=points+sum(log_cor);
        
        % calculate average 
        points=points/3; 

        % save 
        pt.sub{p}.session{s}.trial{k}.result.score=points;
        
        fprintf('Goal recognition done for %d, session %d, file no %d.\n', subject, session, k);
    else
        % landmark positioning --> GMDA TBD
        % save 
        pt.sub{p}.session{s}.trial{k}.result.score=999;
        
    end 
    
    % save object info 
    pt.sub{p}.session{s}.obj_1=0;
    pt.sub{p}.session{s}.obj_2=0;
    pt.sub{p}.session{s}.obj_3=0;
    pt.sub{p}.session{s}.obj_4=0;
    pt.sub{p}.session{s}.obj_5=0;
    
    % save data
    save(fullfile(folderOut, targetFileName_Subject),'pt', '-append'); 

    %% Block 3: Writing data ---> result sheet XLSX for single trials %%
    % header
    col_header={'wp','date_analysis','id','sex','group','session','session_duration',...
        'trial','trial_condition' };

    % main variables
    col_header_2={'score','time',...
        'obj_1', 'obj_2', 'obj_3', 'obj_4', 'obj_5',...
        'lm_MB', 'lm_MD', 'lm_MF', 'lm_MH', 'lm_MJ',...
        'obj_MA', 'obj_MC', 'obj_MI' };

    % name of excel-file
    Trial=num2str(pt.sub{p}.session{s}.trial{k}.trial_num);
    Session=num2str(pt.sub{p}.session{s}.session);
    group_var=[pt.sub{p}.wp string(yyyymmdd(datetime)) pt.sub{p}.id pt.sub{p}.sex pt.sub{p}.group ...  
        pt.sub{p}.session{s}.session pt.sub{p}.session{s}.session_duration ...
        pt.sub{p}.session{s}.trial{k}.trial_num pt.sub{p}.session{s}.trial{k}.trial_type ];
    file_name = ['results_post_' num2str(wp) '_' pt.sub{p}.Group '_' num2str(subject) '_' Session '_' Trial '.xls'];
    new_file = fullfile(folderOut, file_name);

    % write data
    xlswrite(new_file,strrep([group_var ...
        pt.sub{p}.session{s}.trial{k}.result.score pt.sub{p}.session{s}.trial{k}.result.time ...
        pt.sub{p}.session{s}.obj_1 pt.sub{p}.session{s}.obj_2 pt.sub{p}.session{s}.obj_3 ...
        pt.sub{p}.session{s}.obj_4 pt.sub{p}.session{s}.obj_5 ...
        pt.sub{p}.session{s}.lm_MB pt.sub{p}.session{s}.lm_MD pt.sub{p}.session{s}.lm_MF ...
        pt.sub{p}.session{s}.lm_MH pt.sub{p}.session{s}.lm_MJ ...
        pt.sub{p}.session{s}.obj_MA pt.sub{p}.session{s}.obj_MC pt.sub{p}.session{s}.obj_MI ],'.', ','),'post_vars','A2');

    xlswrite(new_file,[col_header col_header_2],'post_vars','A1');

end

%% Write summaries for a selection of variables
new_name2 = [pt.sub{p}.Group '_' num2str(pt.sub{p}.id)  '_results_post'];
new_file = fullfile(folderOut2, new_name2);
sm_wp10_table_allTrials_post(folderOut, new_file, col_header, col_header_2, 'X', 'post_vars');

p=p+1;

end

%% Save data
targetFilePath         = [folderOut2, targetFileName];  
save(targetFilePath, 'pt', '-append')

clear