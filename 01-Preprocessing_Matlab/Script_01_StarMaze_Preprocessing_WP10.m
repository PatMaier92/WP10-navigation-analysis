%% Preparation
clear; close all; clc; format long;
addpath(genpath(pwd)) % add subfolder functions to path

%% Starmaze Data Processing
% @ date 20191001 @ author Deetje Iggena
% @ date 20201106 update by @ Patrizia Maier & now tracked by git 
% for starmaze version WP10 Frankfurt

% The script requires .csv as input-files. 
% For the path analysis and body-rotations, xy-coordinates are required, 
% for the time analysis, a timestamp, for head-rotations, z-coordinates in degree are required.
% for trial info trial_results.csv is required

% Script starts with input: 
%   1. Provide the span of subjects you would like to analyse, start with
%   the first subjectnumber, end with the last subjectnumber (Attention:
%   the folder with S001 has to be in a folder that is named after the
%   subjectnumber)
%   2. Provide the total number of recorded sessions

% BE AWARE:
% In case you change the tracker input-format, please check which coloumns do
% contain your data and adjust the script accordingly to ensure the right
% input.

% Block 1: 
% Preparation of input and output folders and participant list. 
% In "test-figure" the script creates a starmaze depending on the
% provided coordinates of the corners of the starmaze. The coordinates are
% taken from a provided .csv.

% Block 2: 
% Data Preprocessing: In this block, the script will read-in csv-data, 
% clean and rewrite tracker trial files to xlsx-file.
% If you have xls/xlsx-data already, you can skip this step. 
% Besides, the script assumes you have a "log.csv" and a
% "trial_results.csv" file.

% Block 3:
% Read-in of cleaned tracker trial xlsx-files. 
% The actual analysis starts, i.e. calculation of different variables. 

% Block 4: 
% Writing data in xlsx-files.
% In case you would like to have different headlines or a different order
% change the headlines and order in this part

%% Block 1: Input/output and Starmaze definition 
disp('Welcome to the star-maze-analysis-tool. Please follow the instructions. Press CTRL+C to quit')

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
targetFileName         = '\wp10_results_table.mat';
if isfile(fullfile(folderOut2, targetFileName))
    load(fullfile(folderOut2, targetFileName))
end
save(fullfile(folderOut2, targetFileName));
 
if ~exist('sm.sub')
    sm.sub={};
end
[row,participant]=size(sm.sub);
p=participant+1;

[subject_start,subject_end]=sm_inputSubjectsNo();
[sessionNo]=sm_wp10_inputSessionNo();

wp=10; % work package default 
 
%% Starmaze creation 
% Min-Max values
values=table2array(readtable('wp10_values.csv'));
[sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax]=sm_wp10_minMaxValues(values);
% start positions(normalized!)
start=table2array(readtable('wp10_start.csv'));
[sm.coord.start_x,sm.coord.start_y]=sm_wp10_start(start,sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax);
% goal positions(normalized!)
goal=table2array(readtable('wp10_goal.csv'));
[sm.coord.goal_x,sm.coord.goal_y]=sm_wp10_goal(goal,sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax);

% coordinates alley-corner (normalized!)
alley_x=table2array(readtable('wp10_alley_x.csv'));
[cornerNo,alleyNo] = size(alley_x);
for alley=1:alleyNo
    for corner=1:cornerNo
        alley_x(corner,alley)=datanorm(alley_x(corner,alley),sm.coord.xmin,sm.coord.xmax);
    end
end
alley_y=table2array(readtable('wp10_alley_y.csv'));
for alley=1:alleyNo
    for corner=1:cornerNo
        alley_y(corner,alley)=datanorm(alley_y(corner,alley),sm.coord.ymin,sm.coord.ymax);
    end
end
% combined pentagon
pentagon_x=table2array(readtable('wp10_pentagon_x.csv'));
pentagon_y=table2array(readtable('wp10_pentagon_y.csv'));
[cP_x,cP_y,cP,pentagon_x,pentagon_y]=sm_wp10_pentagon(alley_x,alley_y,pentagon_x, pentagon_y,sm.coord.xmin,...
    sm.coord.xmax,sm.coord.ymin,sm.coord.ymax);

% alley_polyshape
[alley_full_x,alley_full_y,alley_polyshape, alley_half_out_x, alley_half_out_y, alley_polyshape_1,...
    alley_half_in_x, alley_half_in_y, alley_polyshape_2]=sm_wp10_alleyPolyshape(alley_x,alley_y);
% rectangle_polyshape
[rec_x,rec_y,rec]=sm_wp10_rectPolyshape(alleyNo, alley_x, alley_y, pentagon_x, pentagon_y);
% triangle_polyshape
[tri_x,tri_y,tri]=sm_wp10_trianglePolyshape(alleyNo, alley_x, alley_y, pentagon_x, pentagon_y);

polyshape_array=[alley_polyshape_1{1,1} alley_polyshape_2{1,1} alley_polyshape_1{1,2} alley_polyshape_2{1,2}...
    alley_polyshape_1{1,3} alley_polyshape_2{1,3} alley_polyshape_1{1,4} alley_polyshape_2{1,4}...
    alley_polyshape_1{1,5} alley_polyshape_1{1,5} alley_polyshape_2{1,5} cP];

% information (ordered)
goal_locs=["MA", "MC", "MI"]; 
start_locs=["Player_MA" "Player_MB" "Player_MC" "Player_MD" "Player_ME" "Player_MF" "Player_MG" ...
    "Player_MH" "Player_MI", "Player_MJ", "Player_MX"];
alley_locs=["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"];

% graph (for automated shortest path calculation) 
% requires Matlab 2021a
[myGraph,graph_x,graph_y]=sm_wp10_createGraph(sm.coord.start_x, sm.coord.start_y,...
    tri_x, tri_y, sm.coord.goal_x, sm.coord.goal_y);

% create test figure plot 
% sm_wp10_testfig("s_maze",polyshape_array,sm.coord.goal_x,sm.coord.goal_y,sm.coord.start_x,sm.coord.start_y,goal_locs,myGraph,graph_x,graph_y);

%% Practise maze creation (motor control) 
% Min-Max-values
pract_values=table2array(readtable('wp10_practise_values.csv'));
[pm.coord.xmin,pm.coord.xmax,pm.coord.ymin,pm.coord.ymax]=sm_wp10_minMaxValues(pract_values);
% start position(normalized!)
pract_start=table2array(readtable('wp10_practise_start.csv'));
[pm.coord.start_x,pm.coord.start_y]=sm_wp10_start(pract_start,pm.coord.xmin,pm.coord.xmax,pm.coord.ymin,pm.coord.ymax);
% goal-positions(normalized!)
pract_goal=table2array(readtable('wp10_practise_goal.csv'));
[pm.coord.goal_x,pm.coord.goal_y]=sm_wp10_goal(pract_goal,pm.coord.xmin,pm.coord.xmax,pm.coord.ymin,pm.coord.ymax);

% coordinates alley-corner (normalized!)
pract_alley_x=table2array(readtable('wp10_practise_x.csv'));
[cornerNo,~] = size(pract_alley_x);
for corner=1:cornerNo
    pract_alley_x(corner,1)=datanorm(pract_alley_x(corner,1),pm.coord.xmin,pm.coord.xmax);
end

pract_alley_y=table2array(readtable('wp10_practise_y.csv'));
for corner=1:cornerNo
    pract_alley_y(corner,1)=datanorm(pract_alley_y(corner,1),pm.coord.ymin,pm.coord.ymax);
end

% alley_polyshape
pract_polyshape=polyshape(pract_alley_x(:),pract_alley_y(:));

% information (ordered)
pract_goal_locs=["1", "2", "3", "4", "5", "6", "7", "8" , "9", "10" ]; 
pract_start_locs="Player_P0"; 

% create test figure plot 
% sm_wp10_testfig("p_maze",pract_polyshape,pm.coord.goal_x,pm.coord.goal_y,pm.coord.start_x,pm.coord.start_y,pract_goal_locs,myGraph,graph_x,graph_y);

%% Block 2: Data preprocessing
rand_dict={};
for subject=subject_start:subject_end
    pstr=['p',num2str(subject)];

for session=1:sessionNo
    sstr=['s',num2str(session)];
    [finalFolderString]=sm_wp10_getFolderstring(session);
    [folderIn,folderOut]=sm_wp10_folder(dataFolder,subject,finalFolderString);
    % save data
    targetFileName_Subject         = ['\' num2str(subject) '_results_table.mat'];
    save(fullfile(folderOut, targetFileName_Subject));

    % read-in trial file 
    opts = detectImportOptions([folderIn, '\trial_results.csv']);
    opts = setvaropts(opts,'timestamp','InputFormat','MM/dd/uuuu hh:mm:ss aa'); 
    trial_data=readtable([folderIn, '\trial_results.csv'],opts);
    
    % read-in log file and preprocess 
    log_data=readtable([folderIn, '\log.csv'], 'ReadVariableNames', true, 'Delimiter', ','); % read in log-file-info
    log_data=log_data.message; % extract & clean relevant data 
    log_data=log_data(contains(log_data,'ID is')); 
    [rand_dict]=sm_wp10_preprocLogData(log_data, subject, pstr, sstr); 
    
    % TBD: rand_dict data check proper randomization 

%% Participant,  Workpackage, Group information
s=session; % s=session --> row, p=participant --> coloumn
sm.sub{p}.id=subject;
ID=num2str(sm.sub{p}.id);
sm.sub{p}.wp=wp; 
[sm.sub{p}.group,sm.sub{p}.Group,sm.sub{p}.sex,sm.sub{p}.Sex]=sm_wp10_callGroup(sm.sub{p}.id); % provide group information
sm.sub{p}.session{s}.session=trial_data.session_num(1,1); % provide session information

%% Read-in and data preparation of individual tracker trial csv-files
% read-in individual trial tracker files, remove unneeded columns and save as .xlsx
sm_wp10_dataPrep(folderIn,ID,sm.sub{p}.Group,num2str(s));
 
%% Read in tracker trial xlsx-files   
d = dir(fullfile(folderIn, '*.xlsx')); % every .xlsx is detected --> change if different file-format
files = {d.name};

for k=1:numel(files)
name = files{k}; 

% exclude practise trials, except motor control 
if session == 3
    jump1 = strfind(name,'_T001');
    jump3 = strfind(name,'_T003'); 
    jump4 = strfind(name,'_T004');
    jump5 = strfind(name,'_T005');
    jump6 = strfind(name,'_T006');
    jump7 = strfind(name,'_T007');
end
    
if session == 3
    if (~isempty(jump1)) || (~isempty(jump3)) || (~isempty(jump4)) || ...
            (~isempty(jump5)) || (~isempty(jump6)) || (~isempty(jump7))
        continue
    end
end

% read-in data 
M = xlsread(fullfile(folderIn, name),'A:E'); % Reading in data into matrix M
t=M(:,1);% time analysis
x=M(:,2); y=M(:,3); % coordinate analysis
data_length=length(x); sdata_length=(size(x))-1;% vector length & shortend length (for distance calculation)
r=M(:,4); % head rotations

% normalization of coordinates 
if session==3 % motor control trial (practise maze)
    x=datanorm(x,pm.coord.xmin,pm.coord.xmax); y=datanorm(y,pm.coord.ymin,pm.coord.ymax);
else % other trials (star maze) 
    x=datanorm(x,sm.coord.xmin,sm.coord.xmax); y=datanorm(y,sm.coord.ymin,sm.coord.ymax);
end
  
sm.sub{p}.session{s}.trial{k}.x_start=x(1,1); sm.sub{p}.session{s}.trial{k}.y_start=y(1,1);
sm.sub{p}.session{s}.trial{k}.x_end=x(end,1); sm.sub{p}.session{s}.trial{k}.y_end=y(end,1);
      
%% Get single trial info from trial_results.csv
sm.sub{p}.session{s}.session_duration=round(minutes(trial_data.timestamp(numel(files),1) - trial_data.timestamp(1,1))); % provide session duration

sm.sub{p}.session{s}.trial{k}.block=trial_data.block_num(k,1);
sm.sub{p}.session{s}.trial{k}.trial_num=trial_data.trial_num(k,1);
sm.sub{p}.session{s}.trial{k}.trial_in_block=trial_data.trial_num_in_block(k,1);     

sm.sub{p}.session{s}.trial{k}.feedback=trial_data.trial_feedback(k,1);
[sm.sub{p}.session{s}.trial{k}.fb]=sm_wp10_feedback(sm.sub{p}.session{s}.trial{k}.feedback);

sm.sub{p}.session{s}.trial{k}.trial_type=trial_data.trial_type(k,1);
sm.sub{p}.session{s}.trial{k}.trial_condition=sm_wp10_trialCondition(sm.sub{p}.session{s}.trial{k}.trial_type,sm.sub{p}.session{s}.trial{k}.fb);

n_goals=4; 
sm.sub{p}.session{s}.trial{k}.trial_goal_identity=sm_wp10_trialGoalIdentity(n_goals, char(trial_data.trial_goal_identity(k,1)));

sm.sub{p}.session{s}.trial{k}.trial_goal=trial_data.trial_goal(k,1);
[sm.sub{p}.session{s}.trial{k}.goal_x,sm.sub{p}.session{s}.trial{k}.goal_y,...
    sm.sub{p}.session{s}.trial{k}.goal_int,sm.sub{p}.session{s}.trial{k}.goal_str,...
    sm.sub{p}.session{s}.trial{k}.goal_alley]=sm_wp10_trialGoal(char(sm.sub{p}.session{s}.trial{k}.trial_goal),...
    sm.coord.goal_x,sm.coord.goal_y,goal_locs,alley_locs);

sm.sub{p}.session{s}.trial{k}.trial_startpos=trial_data.trial_player(k,1);
[sm.sub{p}.session{s}.trial{k}.start]=sm_wp10_trialStart(sm.sub{p}.session{s}.trial{k}.trial_startpos,start_locs);

%% Time analysis using timestamp
b=t(end); a=t(1);
sm.sub{p}.session{s}.trial{k}.result.time=sm_time(a,b); % total amount of time
     
fprintf('Time analysis done for %d, session %d, file no %d.\n', subject, session, k);
    
%% For motor control navigation trial 
% analysis of path and time 
if sm.sub{p}.session{s}.trial{k}.trial_condition==4
    %% Calculate variables depending on single trial settings
    % ideal path coordinates & length    
    x_line_motor=[pm.coord.start_x; pm.coord.goal_x];
    y_line_motor=[pm.coord.start_y; pm.coord.goal_y]; 
    sm.sub{p}.session{s}.trial{k}.ideal_path_length=sm_wp10_idealPathLength(x_line_motor, y_line_motor);
    
    % interpolate data for further analysis
    % using 'interparc' function by John D'Errico (Matlab File Exchanger) 
    [xi_al,yi_al]=sm_wp10_dataInterpolation(x_line_motor, y_line_motor,...
        sm.sub{p}.session{s}.trial{k}.ideal_path_length); 
    
    %% Time analysis: already done 
    %% Coordinate analysis using x-/y-coordinates
    % Path analysis
    sm.sub{p}.session{s}.trial{k}.result.path_length=0; % reset/initiate variables
    for i=1:sdata_length
        % PATH to all TARGETS
        % cumulative distance traveled (used in path accuracy)
        sm.sub{p}.session{s}.trial{k}.result.path_length=sm.sub{p}.session{s}.trial{k}.result.path_length+sum(sm_distance(x(i),x(i+1),y(i),y(i+1)));
    end
    
    % Interpolated IDEAL PATH LENGTH value
    sm.sub{p}.session{s}.trial{k}.ideal_path_length_interpol=0; % start-initiation
    xi_length=length(xi_al)-1;
    for i=1:xi_length
        % ideal path traveled (based on interpolated values)
        sm.sub{p}.session{s}.trial{k}.ideal_path_length_interpol=sm.sub{p}.session{s}.trial{k}.ideal_path_length_interpol+sum(sm_distance(xi_al(i),xi_al(i+1),yi_al(i),yi_al(i+1)));
    end
    
    % PATH ACCURACY to all TARGETS 
    sm.sub{p}.session{s}.trial{k}.result.path_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.path_length,sm.sub{p}.session{s}.trial{k}.ideal_path_length_interpol);
    % sm.sub{p}.session{s}.trial{k}.result.path_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.path_length,sm.sub{p}.session{s}.trial{k}.ideal_path_length); 
    
    % VELOCITY 
    sm.sub{p}.session{s}.trial{k}.result.velocity=sm.sub{p}.session{s}.trial{k}.result.path_length/sm.sub{p}.session{s}.trial{k}.result.time;
    
    % dummy values for all other results variables 
    sm.sub{p}.session{s}.trial{k}.ego_alley=0; 
    sm.sub{p}.session{s}.trial{k}.result.chosen_goal_int=0;
    sm.sub{p}.session{s}.trial{k}.result.chosen_alley_int=0;
    sm.sub{p}.session{s}.trial{k}.result.obj_at_chosen_loc=0;
    sm.sub{p}.session{s}.trial{k}.exclude_trial_matlab=0;
    
    sm.sub{p}.session{s}.trial{k}.result.success=999;
    sm.sub{p}.session{s}.trial{k}.result.success_ego=999;
    sm.sub{p}.session{s}.trial{k}.result.correct_final_alley=999;
    sm.sub{p}.session{s}.trial{k}.result.correct_final_alley_ego=999;
    sm.sub{p}.session{s}.trial{k}.result.final_distance=999;
    sm.sub{p}.session{s}.trial{k}.result.final_distance_ego=999;
    sm.sub{p}.session{s}.trial{k}.result.path_accuracy=999;
    sm.sub{p}.session{s}.trial{k}.result.path_accuracy_chosen=999;
    sm.sub{p}.session{s}.trial{k}.result.path_accuracy_ego=999;
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_target=999; 
    sm.sub{p}.session{s}.trial{k}.result.total_distance_target=999;
    sm.sub{p}.session{s}.trial{k}.result.sum_data_points=999;
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_target=999; 
    sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_target=999; 
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_target=999; 
    sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_target=999; 
    sm.sub{p}.session{s}.trial{k}.result.distance_accuracy=999;
    sm.sub{p}.session{s}.trial{k}.result.distance_accuracy_chosen=999;
    sm.sub{p}.session{s}.trial{k}.result.distance_accuracy_ego=999;
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_path_pure=999; 
    sm.sub{p}.session{s}.trial{k}.result.total_distance_path_pure=999; 
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_path=999; 
    sm.sub{p}.session{s}.trial{k}.result.total_distance_path=999; 
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_path=999; 
    sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_path=999; 
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_path_pure=999;
    sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_path_pure=999;
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_path=999; 
    sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_path=999; 
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_path_pure=999;
    sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_path_pure=999;
    sm.sub{p}.session{s}.trial{k}.result.direct_path=999;
    sm.sub{p}.session{s}.trial{k}.result.direct_path_percent=999; 
    sm.sub{p}.session{s}.trial{k}.result.arm_explored=999; 
    sm.sub{p}.session{s}.trial{k}.result.arm_score=999;
    sm.sub{p}.session{s}.trial{k}.result.path_explored=999;
    sm.sub{p}.session{s}.trial{k}.result.path_score=999;
    sm.sub{p}.session{s}.trial{k}.result.search_strategy_no=999; 
    sm.sub{p}.session{s}.trial{k}.result.searchStrategy.direct=999;
    sm.sub{p}.session{s}.trial{k}.result.searchStrategy.detour=999;
    sm.sub{p}.session{s}.trial{k}.result.searchStrategy.reoriented=999;
    sm.sub{p}.session{s}.trial{k}.result.sum_head_rotation=999;
    sm.sub{p}.session{s}.trial{k}.result.no_full_head_rotation=999;
    sm.sub{p}.session{s}.trial{k}.result.no_head_turn=999;
    sm.sub{p}.session{s}.trial{k}.result.head_turn_accuracy=999;
    sm.sub{p}.session{s}.trial{k}.result.sum_body_rotation=999;
    sm.sub{p}.session{s}.trial{k}.result.body_rotation_accuracy=999;
    sm.sub{p}.session{s}.trial{k}.result.no_body_turn=999;
    sm.sub{p}.session{s}.trial{k}.result.body_turn_accuracy=999;
    
    success_criterium=0; 
    sm.sub{p}.session{s}.trial{k}.goal_x_ego=0; sm.sub{p}.session{s}.trial{k}.goal_y_ego=0; 
    sm.sub{p}.session{s}.trial{k}.x_end=0; sm.sub{p}.session{s}.trial{k}.y_end=0; 
    sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen=999;
    sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen_interpol=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego=999;
    sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego_interpol=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_target=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_total_distance_target=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_chosen_target=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_total_distance_chosen_target=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points_chosen=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_ego_target=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_total_distance_ego_target=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points_ego=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_sum_body_rotation=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_no_body_turn=999; 
    sm.sub{p}.session{s}.trial{k}.ideal_headturnNo=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_zone(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_zone(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_zone(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_zone(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_zone(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.pentagon_zone=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_zone(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_zone(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_zone(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_zone(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_zone(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_pentagon_zone=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.pentagon_entry=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_entry(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_entry(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_entry(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_entry(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_entry(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.time.alley_time(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.time.alley_time(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.time.alley_time(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.time.alley_time(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.time.alley_time(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.time.pentagon_time=999; 
    sm.sub{p}.session{s}.trial{k}.time.triangle_time(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.time.triangle_time(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.time.triangle_time(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.time.triangle_time(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.time.triangle_time(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_rotations(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_rotations(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_rotations(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_rotations(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.alley_rotations(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations(1,5)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations(1,1)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations(1,2)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations(1,3)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations(1,4)=999; 
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations(1,5)=999; 
    
    fprintf('Motor control analysis done for %d, session %d, file no %d.\n', subject, session, k);

else 
%% For all other navigation trials 
    %% Calculate variables depending on single trial settings
    % ideal path coordinates & length, ideal egocentric path coordinates & length
    % Caution: dummy values for egocentric for inner starts (because no clear ideal egocentric path)   
    [sm.sub{p}.session{s}.trial{k}.goal_x_ego, sm.sub{p}.session{s}.trial{k}.goal_y_ego, ...
        x_line, y_line, x_line_chosen, y_line_chosen, x_line_ego, y_line_ego, ...
        sm.sub{p}.session{s}.trial{k}.ideal_path_length, sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen,...
        sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego, sm.sub{p}.session{s}.trial{k}.ego_alley, ...
        sm.sub{p}.session{s}.trial{k}.ideal_headturnNo]=sm_wp10_depStartVariables(myGraph,graph_x,graph_y,...
        sm.sub{p}.session{s}.trial{k}.start, sm.sub{p}.session{s}.trial{k}.goal_int,...
        sm.sub{p}.session{s}.trial{k}.x_end, sm.sub{p}.session{s}.trial{k}.y_end, ...
        alley_full_x, alley_full_y, rec_x, rec_y, cP, polyshape_array);

    % interpolate data for further analysis
    % using 'interparc' function by John D'Errico (Matlab File Exchanger) 
    [xi_al,yi_al]=sm_wp10_dataInterpolation(x_line, ...
        y_line, sm.sub{p}.session{s}.trial{k}.ideal_path_length);
    
    [xi_ch,yi_ch]=sm_wp10_dataInterpolation(x_line_chosen, ...
        y_line_chosen, sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen);
    
    [xi_eg,yi_eg]=sm_wp10_dataInterpolation(x_line_ego, ...
        y_line_ego, sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego);
    
%     % test plot
%     figure;
%     plot(polyshape_array);
%     hold on
%     plot(x_line, y_line, 'k+', xi_al, yi_al, 'k-',...
%         x_line_ego, y_line_ego, 'rx', xi_eg, yi_eg, 'r-',...
%         x_line_chosen, y_line_chosen, 'bx', xi_ch, yi_ch, 'b-');
% %     plot(x_line, y_line, 'k-', x_line_chosen, y_line_chosen, 'r-');
%     xlim([0 1]);
%     ylim([0 1]);
%     hold off
      
    % zone analysis for ideal paths
    [ideal_alley_zone, ideal_rel_alley_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.ideal_alley_entry,...
        dummy1]=sm_wp10_coordinatesZonesStatic(xi_al,...
        yi_al,zeros(length(xi_al),1),alley_full_x,alley_full_y,length(xi_al));
    
    [ideal_rectangle_zone, ideal_rel_rectangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.ideal_rectangle_entry,...
        dummy2]= sm_wp10_coordinatesZonesStatic(xi_al,...
        yi_al,zeros(length(xi_al)),rec_x,rec_y,length(xi_al));
    
    [ideal_alley_entry_mat]=sm_wp10_coordinatesZonesDynamic(xi_al,...
        yi_al, alley_full_x, alley_full_y, length(xi_al));
    [uniq_alley]=unique(ideal_alley_entry_mat,'rows');
    
    [ideal_rectangle_entry_mat]=sm_wp10_coordinatesZonesDynamic(xi_al,...
        yi_al, rec_x, rec_y, length(xi_al));
    [uniq_rect]=unique(ideal_rectangle_entry_mat,'rows');
    if mod(sm.sub{p}.session{s}.trial{k}.start,2) % even number = outer start
        uniq_rect=uniq_rect(2:end,:); % remove first row (start) for outer starts, as it's always zero 
    end 
    
    %% Block 3: Data analysis, i.e. calculcation of variables
    %% Time analysis: already done
    %% Chosen goal location
    [sm.sub{p}.session{s}.trial{k}.result.chosen_goal_int,...
        sm.sub{p}.session{s}.trial{k}.result.chosen_alley_str,...
        sm.sub{p}.session{s}.trial{k}.result.chosen_alley_int,...
        sm.sub{p}.session{s}.trial{k}.result.obj_at_chosen_loc]=sm_wp10_chosenGoal(rand_dict,...
        pstr, sstr, char(trial_data.chosen_goal(k,1)), goal_locs, alley_locs);
    
    %% Coordinate analysis using x-/y-coordinates
    % Path analysis
    sm.sub{p}.session{s}.trial{k}.result.path_length=0; total_dist_to_goal=0; total_dist_to_goal_ego=0; total_dist_to_chosen_goal=0; % reset/initiate variables
    for i=1:sdata_length
        % PATH to target
        % cumulative distance traveled (used in path accuracy)
        sm.sub{p}.session{s}.trial{k}.result.path_length=sm.sub{p}.session{s}.trial{k}.result.path_length+sm_distance(x(i),x(i+1),y(i),y(i+1));
        % DISTANCE to CORRECT target
        % cumulative distance to target (used in distance analysis)
        total_dist_to_goal=total_dist_to_goal+sm_distance(x(i),sm.sub{p}.session{s}.trial{k}.goal_x,y(i),sm.sub{p}.session{s}.trial{k}.goal_y); 
        % DISTANCE to EGOCENTRIC target
        % cumulative distance to egocentric target (used in distance analysis)
        total_dist_to_goal_ego=total_dist_to_goal_ego+sm_distance(x(i),sm.sub{p}.session{s}.trial{k}.goal_x_ego,y(i),sm.sub{p}.session{s}.trial{k}.goal_y_ego);
        % DISTANCE to CHOSEN target
        % cumulative distance to chosen target (used in distance analysis)
        total_dist_to_chosen_goal=total_dist_to_chosen_goal+sm_distance(x(i),x(end),y(i),y(end)); 
    end
    
    % Distance analysis
    % FINAL DISTANCE to CORRECT target
    sm.sub{p}.session{s}.trial{k}.result.final_distance=0; 
    if sm.sub{p}.session{s}.trial{k}.fb==0
        sm.sub{p}.session{s}.trial{k}.result.final_distance=sm_distance(sm.sub{p}.session{s}.trial{k}.goal_x,sm.sub{p}.session{s}.trial{k}.x_end,sm.sub{p}.session{s}.trial{k}.goal_y,sm.sub{p}.session{s}.trial{k}.y_end);
    end 
    
    % AVERAGE DISTANCE to CORRECT path (full trajectory including waiting/rotation)
    [~,distance_to_path] = dsearchn([xi_al, yi_al],[x,y]); % returns euclidian distance to nearest neighbour on interpolated ideal path
    distance_to_path = [distance_to_path; sm.sub{p}.session{s}.trial{k}.result.final_distance]; % add final distance as last data point
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_path=mean(distance_to_path);
    sm.sub{p}.session{s}.trial{k}.result.total_distance_path=sum(distance_to_path);
    
    % AVERAGE DISTANCE to CORRECT path (only path, remove duplicates due to waiting/rotation)
    xy_unique = unique([x y],'rows'); % remove row duplicates 
    [~,distance_to_path_unique] = dsearchn([xi_al, yi_al],xy_unique); % returns euclidian distance to nearest neighbour on interpolated ideal path
    distance_to_path_unique = [distance_to_path_unique; sm.sub{p}.session{s}.trial{k}.result.final_distance]; % add final distance as last data point
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_path_pure=mean(distance_to_path_unique);
    sm.sub{p}.session{s}.trial{k}.result.total_distance_path_pure=sum(distance_to_path_unique);
    
    % AVERAGE DISTANCE to CORRECT target
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_target=total_dist_to_goal/sdata_length(1);
    sm.sub{p}.session{s}.trial{k}.result.total_distance_target=total_dist_to_goal;
    sm.sub{p}.session{s}.trial{k}.result.sum_data_points=sdata_length(1);
            
    % Cumulative IDEAL DISTANCE to CORRECT target
    sm.sub{p}.session{s}.trial{k}.ideal_path_length_interpol=0;id_total_dist_to_goal=0; id_total_dist_to_goal_chosen=0; % start-initiation
    xi_length=length(xi_al)-1;
    for i=1:xi_length
        % ideal cumulative distance traveled (based on interpolated values)
        sm.sub{p}.session{s}.trial{k}.ideal_path_length_interpol=sm.sub{p}.session{s}.trial{k}.ideal_path_length_interpol+sm_distance(xi_al(i),xi_al(i+1),yi_al(i),yi_al(i+1));
        % ideal cumulative distance to allocentric target
        id_total_dist_to_goal=id_total_dist_to_goal+sm_distance(xi_al(i),sm.sub{p}.session{s}.trial{k}.goal_x,yi_al(i),sm.sub{p}.session{s}.trial{k}.goal_y);
    end
    
    % IDEAL AVERAGE DISTANCE to CORRECT target
    sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_target=id_total_dist_to_goal/xi_length;
    sm.sub{p}.session{s}.trial{k}.ideal_total_distance_target=id_total_dist_to_goal;
    sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points=xi_length; 
    
    % PATH ACCURACY to CORRECT target
    sm.sub{p}.session{s}.trial{k}.result.path_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.path_length,sm.sub{p}.session{s}.trial{k}.ideal_path_length_interpol);
    % sm.sub{p}.session{s}.trial{k}.result.path_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.path_length,sm.sub{p}.session{s}.trial{k}.ideal_path_length); 
    
    % DISTANCE ACCURACY to CORRECT target
    sm.sub{p}.session{s}.trial{k}.result.distance_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.avg_distance_target,sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_target);
    
    % VELOCITY 
    sm.sub{p}.session{s}.trial{k}.result.velocity=sm.sub{p}.session{s}.trial{k}.result.path_length/sm.sub{p}.session{s}.trial{k}.result.time;

    fprintf('Path, distance and velocity analysis done for %d, session %d, file no %d.\n', subject, session, k);
    
    %% Additional analysis for allocentric probe trials with potential egocentric response   
    % excludes allocentric trials with inner starts (even integer) as there
    % is no clearly identifiable egocentric path/goal location from these starts 
    
    if sm.sub{p}.session{s}.trial{k}.trial_condition==1 && mod(sm.sub{p}.session{s}.trial{k}.start,2)  
        % Path analysis to EGOCENTRIC target: same as above
        % PATH to CHOSEN target: same as above
        % DISTANCE to EGO target: see as above
    
        % Distance analysis 
        % FINAL DISTANCE to EGOCENTRIC target 
        sm.sub{p}.session{s}.trial{k}.result.final_distance_ego=sm_distance(sm.sub{p}.session{s}.trial{k}.goal_x_ego,sm.sub{p}.session{s}.trial{k}.x_end,sm.sub{p}.session{s}.trial{k}.goal_y_ego,sm.sub{p}.session{s}.trial{k}.y_end);

        % AVERAGE DISTANCE to EGOCENTRIC PATH (full trajectory including waiting/rotation)
        [~,distance_to_ego_path] = dsearchn([xi_eg, yi_eg],[x,y]); % returns euclidian distance to nearest neighbour on interpolated ideal path
        distance_to_ego_path = [distance_to_ego_path; sm.sub{p}.session{s}.trial{k}.result.final_distance_ego]; % add final distance as last data point
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_path=mean(distance_to_ego_path);
        sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_path=sum(distance_to_ego_path);
        
        % AVERAGE DISTANCE to EGOCENTRIC PATH (only path, remove duplicates due to waiting/rotation)
        [~,distance_to_ego_path_unique] = dsearchn([xi_eg, yi_eg],xy_unique); % returns euclidian distance to nearest neighbour on interpolated ideal path
        distance_to_ego_path_unique = [distance_to_ego_path_unique; sm.sub{p}.session{s}.trial{k}.result.final_distance_ego]; % add final distance as last data point
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_path_pure=mean(distance_to_ego_path_unique);
        sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_path_pure=sum(distance_to_ego_path_unique);

        % AVERAGE DISTANCE to EGOCENTRIC target 
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_target=total_dist_to_goal_ego/sdata_length(1);
        sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_target=total_dist_to_goal_ego;
        % sum data points: same as in analyis above

        % Cumulative IDEAL DISTANCE to EGOCENTRIC target 
        sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego_interpol=0;id_total_dist_to_goal_ego=0; % start-initiation
        xi_eg_length=length(xi_eg)-1;
        for i=1:xi_eg_length
            % ideal cumulative distance traveled (based on interpolated values)
            sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego_interpol=sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego_interpol+sm_distance(xi_eg(i),xi_eg(i+1),yi_eg(i),yi_eg(i+1));% cumulative distance traveled
            % ideal cumulative distance to egocentric target 
            id_total_dist_to_goal_ego=id_total_dist_to_goal_ego+sm_distance(xi_eg(i),sm.sub{p}.session{s}.trial{k}.goal_x_ego,yi_eg(i),sm.sub{p}.session{s}.trial{k}.goal_y_ego);
        end

        % IDEAL AVERAGE DISTANCE to EGOCENTRIC target 
        sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_ego_target=id_total_dist_to_goal_ego/xi_eg_length;
        sm.sub{p}.session{s}.trial{k}.ideal_total_distance_ego_target=id_total_dist_to_goal_ego;
        sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points_ego=xi_eg_length; 

        % PATH ACCURACY to EGOCENTRIC target 
        sm.sub{p}.session{s}.trial{k}.result.path_accuracy_ego=sm_ac(sm.sub{p}.session{s}.trial{k}.result.path_length,sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego_interpol);
        
        % DISTANCE ACCURACY to EGOCENTRIC target 
        sm.sub{p}.session{s}.trial{k}.result.distance_accuracy_ego=sm_ac(sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_target,sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_ego_target);       
    else
        % set dummy values     
        sm.sub{p}.session{s}.trial{k}.result.final_distance_ego=999; 
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_path=999; 
        sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_path=999;
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_path_pure=999;
        sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_path_pure=999;
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_target=999; 
        sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_target=999;
        sm.sub{p}.session{s}.trial{k}.result.path_accuracy_ego=999;
        sm.sub{p}.session{s}.trial{k}.result.distance_accuracy_ego=999; 
        
        sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego=999; 
        sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego_interpol=999;
        sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_ego_target=999;
        sm.sub{p}.session{s}.trial{k}.ideal_total_distance_ego_target=999; 
        sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points_ego=999;
    end
    
    fprintf('Additional: Distance in relation to egocentric target done for %d, session %d, file no %d.\n', subject, session, k);
    
    %% Additional analysis for probe trials: Distance in relation to chosen target 
    
    if sm.sub{p}.session{s}.trial{k}.fb==0
        % Path analysis to CHOSEN target: same as above
        % PATH to CHOSEN target: same as above
        % DISTANCE to CHOSEN target: see as above
    
        % AVERAGE DISTANCE to ideal PATH to CHOSEN target (full trajectory including waiting/rotation)
        [~,distance_to_chosen_path] = dsearchn([xi_ch, yi_ch],[x,y]); % returns euclidian distance to nearest neighbour on interpolated ideal path
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_path=mean(distance_to_chosen_path);
        sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_path=sum(distance_to_chosen_path);
        
        % AVERAGE DISTANCE to ideal PATH to CHOSEN target (only path, remove duplicates due to waiting/rotation)
        [~,distance_to_chosen_path_unique] = dsearchn([xi_ch, yi_ch],xy_unique); % returns euclidian distance to nearest neighbour on interpolated ideal path
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_path_pure=mean(distance_to_chosen_path_unique);
        sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_path_pure=sum(distance_to_chosen_path_unique);
    
        % AVERAGE DISTANCE to CHOSEN target
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_target=total_dist_to_chosen_goal/sdata_length(1);
        sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_target=total_dist_to_chosen_goal;
        % sum data points: same as in analyis above
            
        % Cumulative IDEAL DISTANCE to CHOSEN target  
        sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen_interpol=0; id_total_dist_to_goal_chosen=0; % start-initiation
        xi_ch_length=length(xi_ch)-1;
        for i=1:xi_ch_length
            % ideal cumulative distance traveled (based on interpolated values)
            sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen_interpol=sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen_interpol+sm_distance(xi_ch(i),xi_ch(i+1),yi_ch(i),yi_ch(i+1));% cumulative distance traveled
            % ideal cumulative distance to chosen target
            id_total_dist_to_goal_chosen=id_total_dist_to_goal_chosen+sm_distance(xi_ch(i),x(end),yi_ch(i),y(end));
        end
    
        % IDEAL AVERAGE DISTANCE to CHOSEN target
        sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_chosen_target=id_total_dist_to_goal_chosen/xi_ch_length;
        sm.sub{p}.session{s}.trial{k}.ideal_total_distance_chosen_target=id_total_dist_to_goal_chosen;
        sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points_chosen=xi_ch_length; 
    
        % PATH ACCURACY to CHOSEN target 
        sm.sub{p}.session{s}.trial{k}.result.path_accuracy_chosen=sm_ac(sm.sub{p}.session{s}.trial{k}.result.path_length,sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen_interpol);
    
        % DISTANCE ACCURACY to CHOSEN target
        sm.sub{p}.session{s}.trial{k}.result.distance_accuracy_chosen=sm_ac(sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_target,sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_chosen_target);
    else
        % set dummy variables 
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_path=999;
        sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_path=999;
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_path_pure=999;
        sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_path_pure=999;
        sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_target=999;
        sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_target=999;
        sm.sub{p}.session{s}.trial{k}.result.path_accuracy_chosen=999;
        sm.sub{p}.session{s}.trial{k}.result.distance_accuracy_chosen=999;
        
        sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen=999;
        sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen_interpol=999; 
        sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_chosen_target=999;
        sm.sub{p}.session{s}.trial{k}.ideal_total_distance_chosen_target=999;
        sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points_chosen=999;
    end 

    fprintf('Additional: Distance in relation to chosen target done for %d, session %d, file no %d.\n', subject, session, k);
    
    %% Body rotation analysis
    % Body rotation analysis
    sm.sub{p}.session{s}.trial{k}.result.sum_body_rotation=0; br=zeros(1,data_length);
    for i=2:(data_length-2)
        br(i)=sm_b_rot(y(i-1),x(i-1),y(i),x(i));
    end
    sm.sub{p}.session{s}.trial{k}.result.sum_body_rotation=sum(br);
    
    % Ideal sum of body roatations
    l_xi_al=length(xi_al);
    sm.sub{p}.session{s}.trial{k}.ideal_sum_body_rotation=0; br_i=zeros(1,l_xi_al);
    for i=2:(l_xi_al-2)
        br_i(i)=sm_b_rot(yi_al(i-1),xi_al(i-1),yi_al(i),xi_al(i));
    end
    sm.sub{p}.session{s}.trial{k}.ideal_sum_body_rotation=sum(br_i);
    
    % Body-rotation-accuracy
    sm.sub{p}.session{s}.trial{k}.result.body_rotation_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.sum_body_rotation,sm.sub{p}.session{s}.trial{k}.ideal_sum_body_rotation);
    
    fprintf('Body rotation analysis done for %d, session %d, file no %d.\n', subject, session, k);
    
    %% Body turn analysis
    % Cumulative body turns
    body_turn=zeros(1,data_length);
    sm.sub{p}.session{s}.trial{k}.result.body_turn_left=0;sm.sub{p}.session{s}.trial{k}.result.body_turn_right=0;body_walk_straight=0;
    for j=2:(length(br)-1)
        body_turn(j)=heaviside((br(j+1)-br(j)));
        if body_turn(j)==1 && (body_turn(j-1)==0 || body_turn(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.result.body_turn_right=sm.sub{p}.session{s}.trial{k}.result.body_turn_right+1;
        elseif body_turn(j)==0 &&(body_turn(j-1)==1 || body_turn(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.result.body_turn_left=sm.sub{p}.session{s}.trial{k}.result.body_turn_left+1;
        else
            body_walk_straight=body_walk_straight+1;
        end
    end
    sm.sub{p}.session{s}.trial{k}.result.no_body_turn=sm.sub{p}.session{s}.trial{k}.result.body_turn_right+sm.sub{p}.session{s}.trial{k}.result.body_turn_left;
    
    % Cumulative ideal body turns
    body_turn_i=zeros(1,l_xi_al);
    sm.sub{p}.session{s}.trial{k}.ideal_body_turn_left=0;sm.sub{p}.session{s}.trial{k}.ideal_body_turn_right=0;ideal_body_walk_straight=0;
    for j=2:(length(br_i)-1)
        body_turn_i(j)=heaviside((br_i(j+1)-br_i(j)));
        if body_turn_i(j)==1 && (body_turn_i(j-1)==0 || body_turn_i(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.ideal_body_turn_right=sm.sub{p}.session{s}.trial{k}.ideal_body_turn_right+1;
        elseif body_turn_i(j)==0 &&(body_turn_i(j-1)==1 || body_turn_i(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.ideal_body_turn_left=sm.sub{p}.session{s}.trial{k}.ideal_body_turn_left+1;
        else
            ideal_body_walk_straight=ideal_body_walk_straight+1;
        end
    end
    sm.sub{p}.session{s}.trial{k}.ideal_no_body_turn=sm.sub{p}.session{s}.trial{k}.ideal_body_turn_right+sm.sub{p}.session{s}.trial{k}.ideal_body_turn_left;
    
    % Body-turn-accuracy
    sm.sub{p}.session{s}.trial{k}.result.body_turn_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.no_body_turn,sm.sub{p}.session{s}.trial{k}.ideal_no_body_turn);
    
    fprintf('Body turn analysis done for %d, session %d, file no %d.\n', subject, session, k);
    
    %% Head rotation analysis
    % Head rotation information from column 4 of M stored as r
    
    ro=length(r); sm.sub{p}.session{s}.trial{k}.result.final_deviation=0; % final deviation from start to target angle
    sm.sub{p}.session{s}.trial{k}.result.sum_head_rotation=0; % cumulative rotation, sum of head rotations
    for j=1:(ro-1)
        sm.sub{p}.session{s}.trial{k}.result.final_deviation=sm.sub{p}.session{s}.trial{k}.result.final_deviation+(r(j+1)-r(j)); % deviation
        sm.sub{p}.session{s}.trial{k}.result.sum_head_rotation=sm.sub{p}.session{s}.trial{k}.result.sum_head_rotation+abs(r(j+1)-r(j)); % sum of head roations
    end
    sm.sub{p}.session{s}.trial{k}.result.no_full_head_rotation=sm.sub{p}.session{s}.trial{k}.result.sum_head_rotation/360;
    
    fprintf('Head rotation analysis done for %d, session %d, file no %d.\n', subject, session, k);
    
    %% Head turn analysis
    % Cumulative amount of completed head-turns
    head_turn=zeros(1, ro);
    sm.sub{p}.session{s}.trial{k}.result.head_turn_left=0;sm.sub{p}.session{s}.trial{k}.result.head_turn_right=0;head_walk_straight=0;head_left=0; head_right=0;
    for j=2:(ro-1)
        head_turn(j)=heaviside((r(j+1)-r(j)));
        if head_turn(j)==1 && (head_turn(j-1)==0 || head_turn(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.result.head_turn_right=sm.sub{p}.session{s}.trial{k}.result.head_turn_right+1;
        elseif head_turn(j)==0 && (head_turn(j-1)==1 || head_turn(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.result.head_turn_left=sm.sub{p}.session{s}.trial{k}.result.head_turn_left+1;
        elseif (head_turn(j)==0 && head_turn(j-1)==0)
            head_straight=head_walk_straight+1;
        elseif head_turn(j)==1 && head_turn(j-1)==1
            head_right=head_right+1;
        elseif head_turn(j)==0.5 && head_turn(j-1)==0.5
            head_left=head_left+1;
        end
    end
    
    % Head-turn total
    sm.sub{p}.session{s}.trial{k}.result.no_head_turn=sm.sub{p}.session{s}.trial{k}.result.head_turn_right+sm.sub{p}.session{s}.trial{k}.result.head_turn_left;
    
    % Head-turn-accuracy
    sm.sub{p}.session{s}.trial{k}.result.head_turn_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.no_head_turn,sm.sub{p}.session{s}.trial{k}.ideal_headturnNo);
    
    fprintf('Head turn analysis done for %d, session %d, file no %d.\n', subject, session, k);
    
    %% Zone analysis
    [sm.sub{p}.session{s}.trial{k}.zone.alley_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_entry,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_rotations]=sm_wp10_coordinatesZonesStatic(x,...
        y,r,alley_full_x,alley_full_y,data_length);
    
    [alley_entry_mat]=sm_wp10_coordinatesZonesDynamic(x,...
        y,alley_full_x,alley_full_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.zone.alley_zone_out,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone_out,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_rotations_out]=sm_wp10_coordinatesZonesStatic(x,...
        y,r,alley_half_out_x,alley_half_out_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.zone.alley_zone_in,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone_in,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_rotations_in]=sm_wp10_coordinatesZonesStatic(x,...
        y,r,alley_half_in_x,alley_half_in_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.zone.pentagon_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_pentagon_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.pentagon_entry,...
        sm.sub{p}.session{s}.trial{k}.zone.pentagon_rotations]=sm_wp10_coordinatesPentagon(x,...
        y,r,cP_x,cP_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.zone.triangle_zone,....
        sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.triangle_entry,...
        sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations]=sm_wp10_coordinatesZonesStatic(x,...
        y,r,tri_x,tri_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry,...
        sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations]=sm_wp10_coordinatesZonesStatic(x,...
        y,r,rec_x,rec_y,data_length);
    
    [rectangle_entry_mat]=sm_wp10_coordinatesZonesDynamic(x,...
        y,rec_x,rec_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.time.alley_time,...
        sm.sub{p}.session{s}.trial{k}.time.pentagon_time,...
        sm.sub{p}.session{s}.trial{k}.time.triangle_time,...
        sm.sub{p}.session{s}.trial{k}.time.rectangle_time]=sm_wp10_timeInZone(sm.sub{p}.session{s}.trial{k}.result.time,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_pentagon_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone);
    
    fprintf('Zone analysis done for %d, session %d, file no %d.\n', subject, session, k);
    
    %% Exploration-Analysis
    % Arm score and Path score as indicators of alley-exploration
    sm.sub{p}.session{s}.trial{k}.result.arm_explored=sm_wp10_armExplored(sm.sub{p}.session{s}.trial{k}.zone.alley_zone);
    sm.sub{p}.session{s}.trial{k}.result.arm_score=sm_wp10_armScore(sm.sub{p}.session{s}.trial{k}.zone.alley_entry);
    sm.sub{p}.session{s}.trial{k}.result.path_explored=sm_wp10_pathExplored(sm.sub{p}.session{s}.trial{k}.zone.alley_zone_out,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_zone_in,...
        sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.triangle_zone);
    sm.sub{p}.session{s}.trial{k}.result.path_score=sm_wp10_pathScore(sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in,...
        sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry,...
        sm.sub{p}.session{s}.trial{k}.zone.triangle_entry);
    
    % Success
    success_criterium = 0.1; % cut-off proximity to to goal; change value if necessary
    [sm.sub{p}.session{s}.trial{k}.result.success, sm.sub{p}.session{s}.trial{k}.result.success_ego,...
        sm.sub{p}.session{s}.trial{k}.result.correct_final_alley,...
        sm.sub{p}.session{s}.trial{k}.result.correct_final_alley_ego]=sm_wp10_success(success_criterium,...
        sm.sub{p}.session{s}.trial{k}.result.final_distance,...
        sm.sub{p}.session{s}.trial{k}.result.final_distance_ego,...
        sm.sub{p}.session{s}.trial{k}.goal_alley,...
        sm.sub{p}.session{s}.trial{k}.result.chosen_alley_int,...
        sm.sub{p}.session{s}.trial{k}.ego_alley); 
    
    % Direct path to target
    [ideal_no, match_abs, ...
        sm.sub{p}.session{s}.trial{k}.result.direct_path_percent, ...
        sm.sub{p}.session{s}.trial{k}.result.direct_path]=sm_wp10_directPathCalc(uniq_alley,...
        uniq_rect, alley_entry_mat, rectangle_entry_mat);
    
    % Search strategies
    [sm.sub{p}.session{s}.trial{k}.result.searchStrategy.direct,...
        sm.sub{p}.session{s}.trial{k}.result.searchStrategy.detour,...
        sm.sub{p}.session{s}.trial{k}.result.searchStrategy.reoriented,...
        sm.sub{p}.session{s}.trial{k}.result.search_strategy_no]=sm_wp10_searchStrategy(sm.sub{p}.session{s}.trial{k}.result.direct_path,...
        sm.sub{p}.session{s}.trial{k}.result.path_explored,...
        sm.sub{p}.session{s}.trial{k}.result.path_score); 
    
    fprintf('Exploration analysis done for %d, session %d, file no %d.\n', subject, session, k);
    
    %% Marker for excluding trials 
    % Criteria: timeout, or no movement/very short trial time (i.e. path_length=0, body_rot_abs=0, or time < 3)
    sm.sub{p}.session{s}.trial{k}.exclude_trial_matlab=0;
    if sm.sub{p}.session{s}.trial{k}.result.chosen_alley_int==999
        sm.sub{p}.session{s}.trial{k}.exclude_trial_matlab=1;
        fprintf('Trial %d marked for exclusion due to timeout.\n',k); 
    elseif sm.sub{p}.session{s}.trial{k}.trial_condition ~=4 % not for motor control task
        if (sm.sub{p}.session{s}.trial{k}.result.path_length<=0.1 ...
                || sm.sub{p}.session{s}.trial{k}.result.sum_body_rotation==0 ...
                || sm.sub{p}.session{s}.trial{k}.result.time < 3)
            sm.sub{p}.session{s}.trial{k}.exclude_trial_matlab=1;
            fprintf('Trial %d marked for exclusion due lack of movement/short trial time.\n',k);
        end
    end
end 

% save data
save(fullfile(folderOut, targetFileName_Subject),'sm', '-append'); 

%% Block 4: Writing data ---> result sheet XLSX for single trials %%
% header
col_header={'wp','date_analysis','id','sex','group','session','session_duration',...
    'trial','block','trial_in_block','trial_condition',...
    'start_pos','goal_loc','goal_alley','goal_object','goal_vis',...
    'goal_alley_ego','chosen_goal_loc','chosen_alley_loc','obj_at_chosen_loc',...
    'exclude_trial_matlab'};

% main variables
col_header_2={'correct_goal','correct_goal_ego','correct_final_alley','correct_final_alley_ego',...
    'time','velocity','final_distance','final_distance_ego',...
    'path_length','dev_ideal_path','dev_ideal_path_chosen','dev_ideal_path_ego',...
    'avg_distance_path', 'total_distance_path', ...
    'avg_distance_path_pure', 'total_distance_path_pure', ...
    'avg_distance_chosen_path','total_distance_chosen_path',...
    'avg_distance_chosen_path_pure', 'total_distance_chosen_path_pure', ...
    'avg_distance_ego_path', 'total_distance_ego_path', ...  
    'avg_distance_ego_path_pure', 'total_distance_ego_path_pure', ...
    'avg_distance_target','total_distance_target','sum_data_points',...
    'avg_distance_chosen_target','total_distance_chosen_target',...
    'avg_distance_ego_target','total_distance_ego_target', ...
    'dev_ideal_avg_dist','dev_ideal_avg_dist_chosen','dev_ideal_avg_dist_ego',...
    'direct_path','direct_path_percent','arm_explored','arm_score','path_explored','path_score',...
    'search_strategy_no','search_direct','search_detour','search_reoriented',...
    'sum_head_rotation','no_full_head_rotation','no_head_turn','dev_ideal_head_turn',...
    'sum_body_rotation','dev_ideal_body_rotation', 'no_body_turn','dev_ideal_body_turn' };

% support analysis variables 
col_header_3={'correct_crit','goal_x','goal_y','goal_x_ego','goal_y_ego',...
    'chosen_x','chosen_y','ideal_path_length','ideal_path_length_interpol',...
    'ideal_path_length_chosen', 'ideal_path_length_chosen_interpol',...
    'ideal_path_length_ego','ideal_path_length_ego_interpol',...
    'ideal_avg_distance_target','ideal_total_distance_target','ideal_sum_data_points',...
    'ideal_avg_distance_chosen_target','ideal_total_distance_chosen_target','ideal_sum_data_points_chosen',...
    'ideal_avg_distance_ego_target','ideal_total_distance_ego_target','ideal_sum_data_points_ego',...
    'ideal_body_rot','ideal_body_turn','ideal_headturn_no',...  
    'alley_1_abs','alley_2_abs','alley_3_abs','alley_4_abs','alley_5_abs','pent_abs',...
    'tri_1_abs', 'tri_2_abs', 'tri_3_abs', 'tri_4_abs', 'tri_5_abs',...
    'rec_1_abs','rec_2_abs','rec_3_abs','rec_4_abs','rec_5_abs',...
    'alley_1_rel','alley_2_rel','alley_3_rel','alley_4_rel','alley_5_rel','pent_rel',...
    'tri_1_rel', 'tri_2_rel', 'tri_3_rel', 'tri_4_rel', 'tri_5_rel',...
    'rec_1_rel','rec_2_rel','rec_3_rel','rec_4_rel','rec_5_rel',...
    'entry_alley_1', 'entry_alley_2', 'entry_alley_3', 'entry_alley_4', 'entry_alley_5',...
    'entry_alley_1_out', 'entry_alley_2_out', 'entry_alley_3_out', 'entry_alley_4_out', 'entry_alley_5_out',...
    'entry_alley_1_in', 'entry_alley_2_in', 'entry_alley_3_in', 'entry_alley_4_in', 'entry_alley_5_in',...
    'entry_pent','entry_tri_1', 'entry_tri_2', 'entry_tri_3', 'entry_tri_4', 'entry_tri_5',...
    'entry_rec_1', 'entry_rec_2', 'entry_rec_3', 'entry_rec_4', 'entry_rec_5',...
    'time_a1', 'time_a2', 'time_a3', 'time_a4', 'time_a5', 'time_pent',...
    'time_tri_1', 'time_tri_2', 'time_tri_3', 'time_tri_4', 'time_tri_5',...
    'time_rec_1', 'time_rec_2', 'time_rec_3', 'time_rec_4', 'time_rec_5',...
    'rotation_a1', 'rotation_a2', 'rotation_a3', 'rotation_a4', 'rotation_a5',...
    'rotation_tri_5', 'rotation_tri_2', 'rotation_tri_3', 'rotation_tri_4', 'rotation_tri_5',...
    'rotation_rec_1', 'rotation_rec_2', 'rotation_rec_3', 'rotation_rec_4', 'rotation_rec_5' };

% name of excel-file
Trial=num2str(sm.sub{p}.session{s}.trial{k}.trial_num);
Session=num2str(sm.sub{p}.session{s}.session);
group_var=[sm.sub{p}.wp string(yyyymmdd(datetime)) sm.sub{p}.id sm.sub{p}.sex sm.sub{p}.group ...  
    sm.sub{p}.session{s}.session sm.sub{p}.session{s}.session_duration ...
    sm.sub{p}.session{s}.trial{k}.trial_num sm.sub{p}.session{s}.trial{k}.block ...
    sm.sub{p}.session{s}.trial{k}.trial_in_block sm.sub{p}.session{s}.trial{k}.trial_condition ...    
    sm.sub{p}.session{s}.trial{k}.start sm.sub{p}.session{s}.trial{k}.goal_int ...
    sm.sub{p}.session{s}.trial{k}.goal_alley sm.sub{p}.session{s}.trial{k}.trial_goal_identity ...
    sm.sub{p}.session{s}.trial{k}.fb sm.sub{p}.session{s}.trial{k}.ego_alley ...
    sm.sub{p}.session{s}.trial{k}.result.chosen_goal_int sm.sub{p}.session{s}.trial{k}.result.chosen_alley_int ...
    sm.sub{p}.session{s}.trial{k}.result.obj_at_chosen_loc sm.sub{p}.session{s}.trial{k}.exclude_trial_matlab ];
file_name = ['results_' num2str(wp) '_' sm.sub{p}.Group '_' ID '_' Session '_' Trial '.xls'];
new_file = fullfile(folderOut, file_name);

% write data
xlswrite(new_file,strrep([group_var ...
    sm.sub{p}.session{s}.trial{k}.result.success sm.sub{p}.session{s}.trial{k}.result.success_ego ...
    sm.sub{p}.session{s}.trial{k}.result.correct_final_alley sm.sub{p}.session{s}.trial{k}.result.correct_final_alley_ego ...
    sm.sub{p}.session{s}.trial{k}.result.time sm.sub{p}.session{s}.trial{k}.result.velocity ...
    sm.sub{p}.session{s}.trial{k}.result.final_distance sm.sub{p}.session{s}.trial{k}.result.final_distance_ego ...
    sm.sub{p}.session{s}.trial{k}.result.path_length sm.sub{p}.session{s}.trial{k}.result.path_accuracy ...
    sm.sub{p}.session{s}.trial{k}.result.path_accuracy_chosen sm.sub{p}.session{s}.trial{k}.result.path_accuracy_ego ...
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_path sm.sub{p}.session{s}.trial{k}.result.total_distance_path ...
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_path_pure sm.sub{p}.session{s}.trial{k}.result.total_distance_path_pure ...
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_path sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_path ...
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_path_pure sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_path_pure ...
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_path sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_path ...
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_path_pure sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_path_pure ...
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_target sm.sub{p}.session{s}.trial{k}.result.total_distance_target ...
    sm.sub{p}.session{s}.trial{k}.result.sum_data_points ...
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_chosen_target sm.sub{p}.session{s}.trial{k}.result.total_distance_chosen_target ...
    sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego_target sm.sub{p}.session{s}.trial{k}.result.total_distance_ego_target ...
    sm.sub{p}.session{s}.trial{k}.result.distance_accuracy  sm.sub{p}.session{s}.trial{k}.result.distance_accuracy_chosen ...
    sm.sub{p}.session{s}.trial{k}.result.distance_accuracy_ego ...
    sm.sub{p}.session{s}.trial{k}.result.direct_path sm.sub{p}.session{s}.trial{k}.result.direct_path_percent ...
    sm.sub{p}.session{s}.trial{k}.result.arm_explored sm.sub{p}.session{s}.trial{k}.result.arm_score ...
    sm.sub{p}.session{s}.trial{k}.result.path_explored sm.sub{p}.session{s}.trial{k}.result.path_score ...
    sm.sub{p}.session{s}.trial{k}.result.search_strategy_no sm.sub{p}.session{s}.trial{k}.result.searchStrategy.direct ...
    sm.sub{p}.session{s}.trial{k}.result.searchStrategy.detour sm.sub{p}.session{s}.trial{k}.result.searchStrategy.reoriented ...
    sm.sub{p}.session{s}.trial{k}.result.sum_head_rotation sm.sub{p}.session{s}.trial{k}.result.no_full_head_rotation ...
    sm.sub{p}.session{s}.trial{k}.result.no_head_turn sm.sub{p}.session{s}.trial{k}.result.head_turn_accuracy ...
    sm.sub{p}.session{s}.trial{k}.result.sum_body_rotation sm.sub{p}.session{s}.trial{k}.result.body_rotation_accuracy ...
    sm.sub{p}.session{s}.trial{k}.result.no_body_turn sm.sub{p}.session{s}.trial{k}.result.body_turn_accuracy ],'.', ','),'data_vars','A2');

xlswrite(new_file,strrep([group_var success_criterium ...
    sm.sub{p}.session{s}.trial{k}.goal_x sm.sub{p}.session{s}.trial{k}.goal_y ...
    sm.sub{p}.session{s}.trial{k}.goal_x_ego sm.sub{p}.session{s}.trial{k}.goal_y_ego ...
    sm.sub{p}.session{s}.trial{k}.x_end sm.sub{p}.session{s}.trial{k}.y_end ...
    sm.sub{p}.session{s}.trial{k}.ideal_path_length sm.sub{p}.session{s}.trial{k}.ideal_path_length_interpol ...
    sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen sm.sub{p}.session{s}.trial{k}.ideal_path_length_chosen_interpol ...
    sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego sm.sub{p}.session{s}.trial{k}.ideal_path_length_ego_interpol ...
    sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_target sm.sub{p}.session{s}.trial{k}.ideal_total_distance_target ...
    sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points ...
    sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_chosen_target sm.sub{p}.session{s}.trial{k}.ideal_total_distance_chosen_target ...
    sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points_chosen ...
    sm.sub{p}.session{s}.trial{k}.ideal_avg_distance_ego_target sm.sub{p}.session{s}.trial{k}.ideal_total_distance_ego_target ...
    sm.sub{p}.session{s}.trial{k}.ideal_sum_data_points_ego ...
    sm.sub{p}.session{s}.trial{k}.ideal_sum_body_rotation ...
    sm.sub{p}.session{s}.trial{k}.ideal_no_body_turn sm.sub{p}.session{s}.trial{k}.ideal_headturnNo ...
    sm.sub{p}.session{s}.trial{k}.zone.alley_zone(1,1) sm.sub{p}.session{s}.trial{k}.zone.alley_zone(1,2) sm.sub{p}.session{s}.trial{k}.zone.alley_zone(1,3) sm.sub{p}.session{s}.trial{k}.zone.alley_zone(1,4) sm.sub{p}.session{s}.trial{k}.zone.alley_zone(1,5)...
    sm.sub{p}.session{s}.trial{k}.zone.pentagon_zone...
    sm.sub{p}.session{s}.trial{k}.zone.triangle_zone(1,1) sm.sub{p}.session{s}.trial{k}.zone.triangle_zone(1,2) sm.sub{p}.session{s}.trial{k}.zone.triangle_zone(1,3) sm.sub{p}.session{s}.trial{k}.zone.triangle_zone(1,4) sm.sub{p}.session{s}.trial{k}.zone.triangle_zone(1,5)...
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone(1,1) sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone(1,2) sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone(1,3) sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone(1,4) sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone(1,5)...
    sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone(1,1) sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone(1,2) sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone(1,3) sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone(1,4) sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone(1,5)...
    sm.sub{p}.session{s}.trial{k}.zone.rel_pentagon_zone...
    sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone(1,1) sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone(1,2) sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone(1,3) sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone(1,4) sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone(1,5)...
    sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone(1,1) sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone(1,2) sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone(1,3) sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone(1,4) sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone(1,5)...
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry(1,1) sm.sub{p}.session{s}.trial{k}.zone.alley_entry(1,2) sm.sub{p}.session{s}.trial{k}.zone.alley_entry(1,3) sm.sub{p}.session{s}.trial{k}.zone.alley_entry(1,4) sm.sub{p}.session{s}.trial{k}.zone.alley_entry(1,5)...
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out(1,1) sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out(1,2) sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out(1,3) sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out(1,4) sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out(1,5)...
    sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in(1,1) sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in(1,2) sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in(1,3) sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in(1,4) sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in(1,5)...
    sm.sub{p}.session{s}.trial{k}.zone.pentagon_entry...
    sm.sub{p}.session{s}.trial{k}.zone.triangle_entry(1,1) sm.sub{p}.session{s}.trial{k}.zone.triangle_entry(1,2) sm.sub{p}.session{s}.trial{k}.zone.triangle_entry(1,3) sm.sub{p}.session{s}.trial{k}.zone.triangle_entry(1,4) sm.sub{p}.session{s}.trial{k}.zone.triangle_entry(1,5)...
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry(1,1) sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry(1,2) sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry(1,3) sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry(1,4) sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry(1,5)...
    sm.sub{p}.session{s}.trial{k}.time.alley_time(1,1) sm.sub{p}.session{s}.trial{k}.time.alley_time(1,2) sm.sub{p}.session{s}.trial{k}.time.alley_time(1,3) sm.sub{p}.session{s}.trial{k}.time.alley_time(1,4) sm.sub{p}.session{s}.trial{k}.time.alley_time(1,5)...
    sm.sub{p}.session{s}.trial{k}.time.pentagon_time...
    sm.sub{p}.session{s}.trial{k}.time.triangle_time(1,1) sm.sub{p}.session{s}.trial{k}.time.triangle_time(1,2) sm.sub{p}.session{s}.trial{k}.time.triangle_time(1,3) sm.sub{p}.session{s}.trial{k}.time.triangle_time(1,4) sm.sub{p}.session{s}.trial{k}.time.triangle_time(1,5)...
    sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,1) sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,2) sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,3) sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,4) sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,5) ...
    sm.sub{p}.session{s}.trial{k}.zone.alley_rotations(1,1) sm.sub{p}.session{s}.trial{k}.zone.alley_rotations(1,2) sm.sub{p}.session{s}.trial{k}.zone.alley_rotations(1,3) sm.sub{p}.session{s}.trial{k}.zone.alley_rotations(1,4) sm.sub{p}.session{s}.trial{k}.zone.alley_rotations(1,5) ...
    sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations(1,1) sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations(1,2) sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations(1,3) sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations(1,4) sm.sub{p}.session{s}.trial{k}.zone.triangle_rotations(1,5) ...
    sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations(1,1) sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations(1,2) sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations(1,3) sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations(1,4) sm.sub{p}.session{s}.trial{k}.zone.rectangle_rotations(1,5) ] ,'.', ','),'support_vars','A2');

xlswrite(new_file,[col_header col_header_2],'data_vars','A1');
xlswrite(new_file,[col_header col_header_3],'support_vars','A1');

%% Create plots 
if session==3
    sm_wp10_plotMotorControl(sm.sub{p}.session{s}.trial{k}.trial_num,sm.sub{p}.session{s}.session,...
        sm.sub{p}.id,sm.sub{p}.Group,...
        pract_polyshape,pm.coord.goal_x,pm.coord.goal_y,pm.coord.start_x,pm.coord.start_y,...
        pract_goal_locs,x,y,xi_al,yi_al,folderOut)
else
    sm_wp10_plotTrack(sm.sub{p}.session{s}.trial{k}.trial_num,sm.sub{p}.session{s}.session,...
        sm.sub{p}.session{s}.trial{k}.trial_condition,sm.sub{p}.session{s}.trial{k}.start,...
        sm.sub{p}.id,sm.sub{p}.Group,sm.sub{p}.session{s}.trial{k}.result.success,...
        sm.sub{p}.session{s}.trial{k}.result.direct_path,sm.sub{p}.session{s}.trial{k}.result.search_strategy_no,...
        alley_polyshape_1,alley_polyshape_2,tri,rec,x,y,x_line_ego,y_line_ego,x_line,y_line,...
        sm.sub{p}.session{s}.trial{k}.goal_x,sm.sub{p}.session{s}.trial{k}.goal_y,folderOut)
end
end

end

%% Write summaries for a selection of variables
new_name2 = [sm.sub{p}.Group '_' num2str(sm.sub{p}.id)  '_results'];
new_file = fullfile(folderOut2, new_name2);
sm_wp10_table_allTrials(folderOut,new_file,col_header,col_header_2,col_header_3,'BU','EE','data_vars','support_vars');

p=p+1;

end 

%% Save data
targetFilePath         = [folderOut2, targetFileName];  
save(targetFilePath, 'sm', '-append')

clear