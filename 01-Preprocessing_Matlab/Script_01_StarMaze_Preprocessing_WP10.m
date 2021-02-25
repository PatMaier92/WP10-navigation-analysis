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
%   1. In case you add an analysis, change p for loop p is the next bumber in
%   the starmaze-table
%   2. Provide the span of subjects you would like to analyse, start with
%   the first subjectnumber, end with the last subjectnumber (Attention:
%   the folder with S001 has to be in a folder that is named after the
%   subjectnumber)
%   3. Provide the total number of recorded sessions

% BE AWARE:
% In case you change the input-format, please check which coloumns do
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
[sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax]=sm_wp10_MinMaxValues(values);
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

% % information (ordered)
goal_locs=["MA", "MC", "MI"]; 
start_locs=["Player_MA" "Player_MB" "Player_MC" "Player_MD" "Player_ME" "Player_MF" "Player_MG" ...
    "Player_MH" "Player_MI", "Player_MJ", "Player_MX"];
alley_locs=["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"];

% Create Test-Figure plot 
% sm_wp10_testfig("s_maze",polyshape_array,sm.coord.goal_x,sm.coord.goal_y,sm.coord.start_x,sm.coord.start_y,goal_locs);

%% Practise maze creation (motor control) 
% Min-Max-values
pract_values=table2array(readtable('wp10_practise_values.csv'));
[pm.coord.xmin,pm.coord.xmax,pm.coord.ymin,pm.coord.ymax]=sm_wp10_MinMaxValues(pract_values);
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

% Create Test-Figure plot 
% sm_wp10_testfig("p_maze",pract_polyshape,pm.coord.goal_x,pm.coord.goal_y,pm.coord.start_x,pm.coord.start_y,pract_goal_locs);

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
% id=trial_data.ppid(1,1);
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
    
M = xlsread(fullfile(folderIn, name),'A:D'); % Reading in data into matrix M
t=M(:,1);% reading data for time analysis
x=M(:,2); y=M(:,3); x=datanorm(x,sm.coord.xmin,sm.coord.xmax); y=datanorm(y,sm.coord.ymin,sm.coord.ymax);   % data normalization for coordinates
data_length=length(x); sdata_length=(size(x))-1;% vector length & shortend length (for distance calculation)
    
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

%% For motor control navigation trial 
% analysis of path accuracy and time 
if sm.sub{p}.session{s}.trial{k}.trial_condition==4
    disp('To do'); 
    % TBD 
    % total time for trial, cumulative path length 
    % add dummies for all other variables
else 
%% For all other navigation trials 
    %% Calculate variables depending on single trial settings
    % ideal path coordinates & length, ideal egocentric path coordinates & length
    % TBD: egocentric paths for inner starts, currently copy of original path 
    [sm.sub{p}.session{s}.trial{k}.goal_x_ego, sm.sub{p}.session{s}.trial{k}.goal_y_ego, x_line, y_line, x_line_ego, y_line_ego,...
        sm.sub{p}.session{s}.trial{k}.ideal_distance, sm.sub{p}.session{s}.trial{k}.ideal_distance_ego,...
        sm.sub{p}.session{s}.trial{k}.ideal_headturnNo,...
        sm.sub{p}.session{s}.trial{k}.ego_alley]=sm_wp10_depStartVariables(sm.sub{p}.session{s}.trial{k}.start,...
        sm.sub{p}.session{s}.trial{k}.goal_x, sm.sub{p}.session{s}.trial{k}.goal_y, sm.sub{p}.session{s}.trial{k}.goal_int,...
        sm.sub{p}.session{s}.trial{k}.x_start, sm.sub{p}.session{s}.trial{k}.y_start, sm.coord.start_x, ...
        sm.coord.start_y, alley_x, alley_y, pentagon_x, pentagon_y, alley_full_x, alley_full_y, rec_x, rec_y, cP);

    % interpolate data for further analysis
    % using 'interparc' function by John D'Errico (Matlab File Exchanger) 
    [xi_al,yi_al,xi_eg,yi_eg]=sm_wp10_dataInterpolation(x_line, ...
        y_line, sm.sub{p}.session{s}.trial{k}.ideal_distance, ...
        x_line_ego, y_line_ego, sm.sub{p}.session{s}.trial{k}.ideal_distance_ego);
    
    % test plot
    figure;
    plot(polyshape_array);
    hold on
    plot(x_line, y_line, 'k+', xi_al, yi_al, 'k-',...
        x_line_ego, y_line_ego, 'rx', xi_eg, yi_eg, 'r-');
    xlim([0 1]);
    ylim([0 1]);
    hold off
    
    % zone analysis for ideal paths
    [ideal_alley_zone, ideal_rel_alley_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.ideal_alley_entry]=sm_wp10_coordinatesZonesStatic(xi_al,...
        yi_al, alley_full_x, alley_full_y, length(xi_al));
    
    [ideal_rectangle_zone, ideal_rel_rectangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.ideal_rectangle_entry]= sm_wp10_coordinatesZonesStatic(xi_al,...
        yi_al, rec_x, rec_y, length(xi_al));
    
    [ideal_alley_entry_mat]=sm_wp10_coordinatesZonesDynamic(xi_al,...
        yi_al, alley_full_x, alley_full_y, length(xi_al));
    [uniq_alley]=unique(ideal_alley_entry_mat,'rows');
    
    [ideal_rectangle_entry_mat]=sm_wp10_coordinatesZonesDynamic(xi_al,...
        yi_al, rec_x, rec_y, length(xi_al));
    [uniq_rect]=unique(ideal_rectangle_entry_mat,'rows');
    uniq_rect=uniq_rect(2:end,:); % remove first row (start), always zeroes
    
    % zone analysis for ideal ego paths
    [ideal_ego_alley_zone, ideal_ego_rel_alley_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.ideal_ego_alley_entry]=sm_wp10_coordinatesZonesStatic(xi_eg,...
        yi_eg, alley_full_x, alley_full_y, length(xi_eg));
    
    [ideal_ego_rectangle_zone, ideal_ego_rel_rectangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.ideal_ego_rectangle_entry]= sm_wp10_coordinatesZonesStatic(xi_eg,...
        yi_eg, rec_x, rec_y, length(xi_eg));
    
    [ideal_ego_alley_entry_mat]=sm_wp10_coordinatesZonesDynamic(xi_eg,...
        yi_eg, alley_full_x, alley_full_y, length(xi_eg));
    [uniq_e_alley]=unique(ideal_ego_alley_entry_mat,'rows');
    
    [ideal_ego_rectangle_entry_mat]=sm_wp10_coordinatesZonesDynamic(xi_eg,...
        yi_eg, rec_x, rec_y, length(xi_eg));
    [uniq_e_rect]=unique(ideal_ego_rectangle_entry_mat,'rows');
    uniq_e_rect=uniq_e_rect(2:end,:); % remove first row (start), always zeroes

    %% Block 3: Data analysis, i.e. calculcation of variables
    %% Chosen goal location
        %% Chosen goal location
    [sm.sub{p}.session{s}.trial{k}.chosen_goal_int,...
        sm.sub{p}.session{s}.trial{k}.chosen_goal_str,...
        sm.sub{p}.session{s}.trial{k}.chosen_alley_int,...
        sm.sub{p}.session{s}.trial{k}.obj_at_chosen_loc]=sm_wp10_chosenGoal(rand_dict,...
        pstr, sstr, char(trial_data.chosen_goal(k,1)), goal_locs, alley_locs);
      
    %% Time analysis
    b=t(end,:); a=t(1,1);
    sm.sub{p}.session{s}.trial{k}.result.time=sm_time(a,b); % total amount of time
     
    fprintf('Time analysis done for %d, session %d, file no %d.\n', subject, session, k);
    %% Coordinate analysis using x-/y-coordinates
    % Path analysis
    sm.sub{p}.session{s}.trial{k}.result.distance_traveled=0;dist_to_goal=0; % reset/initiate variables
    for i=1:sdata_length
        % PATH TO TARGET
        % actual cumulative distance traveled (used in path accuracy)
        sm.sub{p}.session{s}.trial{k}.result.distance_traveled=sm.sub{p}.session{s}.trial{k}.result.distance_traveled+sum(sm_distance(x(i),x(i+1),y(i),y(i+1)));
        % DISTANCE TO TARGET
        % actual cumulative distance to target (used in distance analysis)
        dist_to_goal=dist_to_goal+sum(sm_distance(x(i),sm.sub{p}.session{s}.trial{k}.goal_x,y(i),sm.sub{p}.session{s}.trial{k}.goal_y)); 
    end
    
    % Distance-analysis
    % FINAL DISTANCE TO TARGET
    sm.sub{p}.session{s}.trial{k}.result.final_distance=sm_distance(sm.sub{p}.session{s}.trial{k}.goal_x,sm.sub{p}.session{s}.trial{k}.x_end,sm.sub{p}.session{s}.trial{k}.goal_y,sm.sub{p}.session{s}.trial{k}.y_end);
    
    % AVERAGE DISTANCE TO TARGET
    sm.sub{p}.session{s}.trial{k}.result.avg_distance=dist_to_goal/sdata_length(1);
    
    % Cumulative IDEAL DISTANCE TO TARGET
    sm.sub{p}.session{s}.trial{k}.ideal_distance_interpol=0;id_dist_to_goal=0; % start-initiation
    xi_length=length(xi_al)-1;
    for i=1:xi_length
        % ideal cumulative distance traveled (based on interpolated values)
        sm.sub{p}.session{s}.trial{k}.ideal_distance_interpol=sm.sub{p}.session{s}.trial{k}.ideal_distance_interpol+sum(sm_distance(xi_al(i),xi_al(i+1),yi_al(i),yi_al(i+1)));
        % ideal cumulative distance to target
        id_dist_to_goal=id_dist_to_goal+sum(sm_distance(xi_al(i),sm.sub{p}.session{s}.trial{k}.goal_x,yi_al(i),sm.sub{p}.session{s}.trial{k}.goal_y));
    end
    
    % IDEAL AVERAGE DISTANCE TO TARGET
    sm.sub{p}.session{s}.trial{k}.ideal_avg_distance=id_dist_to_goal/xi_length;
    
    % PATH ACCURACY
    sm.sub{p}.session{s}.trial{k}.result.path_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.distance_traveled,sm.sub{p}.session{s}.trial{k}.ideal_distance_interpol);
    % sm.sub{p}.session{s}.trial{k}.result.path_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.distance_traveled,sm.sub{p}.session{s}.trial{k}.ideal_distance); 
    
    % DISTANCE ACCURACY TO TARGET 
    sm.sub{p}.session{s}.trial{k}.result.distance_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.avg_distance,sm.sub{p}.session{s}.trial{k}.ideal_avg_distance);
     
    % VELOCITY 
    sm.sub{p}.session{s}.trial{k}.result.velocity=sm.sub{p}.session{s}.trial{k}.result.distance_traveled/sm.sub{p}.session{s}.trial{k}.result.time;

    fprintf('Path, distance and velocity analysis done for %d, session %d, file no %d.\n', subject, session, k);
    %% Egocentric variables    
    % Calculating PATH-ACCURACY to egocentric target
    sm.sub{p}.session{s}.trial{k}.result.path_accuracy_ego= sm_ac(sm.sub{p}.session{s}.trial{k}.result.distance_traveled,sm.sub{p}.session{s}.trial{k}.ideal_path_ego);
    dist_to_goal_ego=0;
    for i=1:sdata_length
        dist_to_goal_ego=dist_to_goal_ego+sum(sm_distance(x(i),sm.sub{p}.session{s}.trial{k}.goal_x_ego,y(i),sm.sub{p}.session{s}.trial{k}.goal_y_ego)); % cumulative distance to egocentric target
    end
    % FINAL DISTANCE TO EGOCENTRIC TARGET
    sm.sub{p}.session{s}.trial{k}.result.final_distance_ego=sm_distance(sm.sub{p}.session{s}.trial{k}.goal_x_ego,x(end,:),sm.sub{p}.session{s}.trial{k}.goal_y_ego,y(end,:));
    
    % % AVERAGE DISTANCE TO EGOCENTRIC TARGET
    %     dtet=dist_to_goal_ego./sdata_length;
    %     sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego=dtet(1,1);
    % % Cumulative ideal distance to egocentric target
    %     sm.sub{p}.session{s}.trial{k}.ideal_dist_to_goal_ego=0; xi_le=length(xi_eg);
    %     if sm.sub{p}.session{s}.trial{k}.goal_y_ego <= yi_eg(1,1)
    %         reference_y=yi_eg(1,1);
    %         reference_x=xi_eg(1,1);
    %     else
    %         reference_y=sm.sub{p}.session{s}.trial{k}.goal_y_ego;
    %         reference_x=sm.sub{p}.session{s}.trial{k}.goal_x_ego;
    %     end
    %
    %     for i=1:xi_le
    %         sm.sub{p}.session{s}.trial{k}.ideal_dist_to_goal_ego=sm.sub{p}.session{s}.trial{k}.ideal_dist_to_goal_ego+sum(sm_distance(xi_eg(i),reference_x,yi_eg(i),reference_y));
    %     end
    
    % % Average ideal distance to egoocentric target
    %     idtet=sm.sub{p}.session{s}.trial{k}.ideal_dist_to_goal_ego./xi_le;
    %     sm.sub{p}.session{s}.trial{k}.ideal_avg_dist_to_goal_ego=idtet(1,1);
    % % DISTANCE-ACCURACY regarding egocentric target
    %     sm.sub{p}.session{s}.trial{k}.result.distance_accuracy_ego=sm_ac(sm.sub{p}.session{s}.trial{k}.result.avg_distance_ego,sm.sub{p}.session{s}.trial{k}.ideal_avg_dist_to_goal_ego);
    % % Ideal velocity ego target
    %     sm.sub{p}.session{s}.trial{k}.ideal_velocity_ego=sm.sub{p}.session{s}.trial{k}.ideal_path_ego/sm.sub{p}.session{s}.trial{k}.ideal_time_ego;
    % % Velocity-accuracy egocentric target
    %     sm.sub{p}.session{s}.trial{k}.result.velocity_accuracy_ego=sm_ac(sm.sub{p}.session{s}.trial{k}.result.velocity, sm.sub{p}.session{s}.trial{k}.ideal_velocity_ego);
    
    fprintf('Egocentric variable analysis done for %d, session %d, file no %d.\n', subject, session, k);
    %% Turn & Rotation- Analysis using xy-coordinates
    % Body-Rotation-Analysis
    sm.sub{p}.session{s}.trial{k}.result.body_rotation=0; br=zeros(1,data_length);
    for i=2:(data_length-2)
        br(i)=sm_b_rot(y(i-1),x(i-1),y(i),x(i));
        sm.sub{p}.session{s}.trial{k}.result.body_rotation=sm.sub{p}.session{s}.trial{k}.result.body_rotation+br(i);
    end
    % Ideal sum of body-roatations
    l_xi_al=length(xi_al);
    sm.sub{p}.session{s}.trial{k}.ideal_body_rotation=0; br_i=zeros(1,l_xi_al);
    for i=2:(l_xi_al-2)
        br_i(i)=sm_b_rot(yi_al(i-1),xi_al(i-1),yi_al(i),xi_al(i));
        sm.sub{p}.session{s}.trial{k}.ideal_body_rotation=sm.sub{p}.session{s}.trial{k}.ideal_body_rotation+br_i(i);
    end
    % Body-rotation-accuracy
    sm.sub{p}.session{s}.trial{k}.result.body_rotation_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.body_rotation,sm.sub{p}.session{s}.trial{k}.ideal_body_rotation);
    
    fprintf('Body rotation analysis done for %d, session %d, file no %d.\n', subject, session, k);
    
    %% Body-Turn-Analysis
    % Cumulative body turns
    body_turn=zeros(1,data_length);
    sm.sub{p}.session{s}.trial{k}.result.body_turn_left=0;sm.sub{p}.session{s}.trial{k}.result.body_turn_right=0;body_walk_straight=0;
    for j=2:(length(br)-1)
        body_turn(j)=heaviside((br(j+1)-br(j)));
        if body_turn(j)==1 && (body_turn(j-1)==0 || body_turn(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.result.body_turn_right=sm.sub{p}.session{s}.trial{k}.result.body_turn_right+1;
        elseif body_turn(j) ==0 &&(body_turn(j-1)==1 || body_turn(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.result.body_turn_left=sm.sub{p}.session{s}.trial{k}.result.body_turn_left+1;
        else
            body_walk_straight=body_walk_straight+1;
        end
    end
    sm.sub{p}.session{s}.trial{k}.result.body_turn_total= sm.sub{p}.session{s}.trial{k}.result.body_turn_right+sm.sub{p}.session{s}.trial{k}.result.body_turn_left;
    % Cumulative ideal body turns
    body_turn_i=zeros(1,l_xi_al);
    sm.sub{p}.session{s}.trial{k}.ideal_body_turn_left=0;sm.sub{p}.session{s}.trial{k}.ideal_body_turn_right=0;ideal_body_walk_straight=0;
    for j=2:(length(br_i)-1)
        body_turn_i(j)=heaviside((br_i(j+1)-br_i(j)));
        if body_turn_i(j)==1 && (body_turn_i(j-1)==0 || body_turn_i(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.ideal_body_turn_right=sm.sub{p}.session{s}.trial{k}.ideal_body_turn_right+1;
        elseif body_turn_i(j) ==0 &&(body_turn_i(j-1)==1 || body_turn_i(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.ideal_body_turn_left=sm.sub{p}.session{s}.trial{k}.ideal_body_turn_left+1;
        else
            ideal_body_walk_straight=ideal_body_walk_straight+1;
        end
    end
    sm.sub{p}.session{s}.trial{k}.ideal_body_turn_total= sm.sub{p}.session{s}.trial{k}.ideal_body_turn_right+sm.sub{p}.session{s}.trial{k}.ideal_body_turn_left;
    % Body-turn-accuracy
    sm.sub{p}.session{s}.trial{k}.result.body_turn_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.body_turn_total,sm.sub{p}.session{s}.trial{k}.ideal_body_turn_total);
    
    fprintf('Body turn analysis done for %d, session %d, file no %d.\n', subject, session, k);
    %% Head-Rotation-Analysis using Z-coordinates
    r=M(:,4); % rotations in coloumn 6
    % Final deviation from start to target angle
    ro=length(r); sm.sub{p}.session{s}.trial{k}.result.final_deviation=0;
    % Cumulative rotation, sum of head rotations
    sm.sub{p}.session{s}.trial{k}.result.head_rotation=0;
    for j=1:(ro-1)
        sm.sub{p}.session{s}.trial{k}.result.final_deviation=sm.sub{p}.session{s}.trial{k}.result.final_deviation+((r(j+1)-r(j))); % deviation
        sm.sub{p}.session{s}.trial{k}.result.head_rotation=sm.sub{p}.session{s}.trial{k}.result.head_rotation+(abs((r(j+1)-r(j)))); % sum of head roations
    end
    sm.sub{p}.session{s}.trial{k}.result.full_head_rotation= sm.sub{p}.session{s}.trial{k}.result.head_rotation/360;
    % Cumulative amount of completed head-turns
    head_turn= zeros(1, ro);
    sm.sub{p}.session{s}.trial{k}.result.head_turn_left=0;sm.sub{p}.session{s}.trial{k}.result.head_turn_right=0;head_walk_straight=0;head_left=0; head_right=0;
    for j=2:(ro-1)
        head_turn(j)=heaviside((r(j+1)-r(j)));
        if head_turn(j)==1 && (head_turn(j-1)==0 || head_turn(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.result.head_turn_right=sm.sub{p}.session{s}.trial{k}.result.head_turn_right+1;
        elseif head_turn(j) ==0 && (head_turn(j-1)==1 || head_turn(j-1)==0.5)
            sm.sub{p}.session{s}.trial{k}.result.head_turn_left=sm.sub{p}.session{s}.trial{k}.result.head_turn_left+1;
        elseif (head_turn(j) ==0 && head_turn(j-1)==0)
            head_straight=head_walk_straight+1;
        elseif head_turn(j) ==1 && head_turn(j-1)==1
            head_right=head_right+1;
        elseif head_turn(j) ==0.5 && head_turn(j-1)==0.5
            head_left=head_left+1;
        end
    end
    % Head-turn total
    sm.sub{p}.session{s}.trial{k}.result.head_turn_total= sm.sub{p}.session{s}.trial{k}.result.head_turn_right+sm.sub{p}.session{s}.trial{k}.result.head_turn_left;
    % Head-turn-accuracy
    sm.sub{p}.session{s}.trial{k}.result.head_turn_accuracy=sm_ac(sm.sub{p}.session{s}.trial{k}.result.head_turn_total,sm.sub{p}.session{s}.trial{k}.ideal_headturnNo);
    
    fprintf('Head turn analysis done for %d, session %d, file no %d.\n', subject, session, k);
    %% Zone-Analysis
    [sm.sub{p}.session{s}.trial{k}.zone.alley_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_entry]=sm_wp10_coordinatesAlleys(x,...
        y,alley_full_x,alley_full_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.zone.alley_zone_out,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone_out,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_entry_out]=sm_wp10_coordinatesAlleys(x,...
        y,alley_half_out_x,alley_half_out_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.zone.alley_zone_in,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_alley_zone_in,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_entry_in]=sm_wp10_coordinatesAlleys(x,...
        y,alley_half_in_x,alley_half_in_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.zone.pentagon_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_pentagon_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.pentagon_entry]=sm_wp10_coordinatesPentagon(x,...
        y,cP_x,cP_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.zone.triangle_zone,....
        sm.sub{p}.session{s}.trial{k}.zone.rel_triangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.triangle_entry]=sm_wp10_coordinatesAlleys(x,...
        y,tri_x,tri_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.zone.rectangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_rectangle_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry]= sm_wp10_coordinatesAlleys(x,...
        y,rec_x,rec_y,data_length);
    
    [sm.sub{p}.session{s}.trial{k}.time.alley_time,...
        sm.sub{p}.session{s}.trial{k}.time.pentagon_time,...
        sm.sub{p}.session{s}.trial{k}.time.triangle_time,...
        sm.sub{p}.session{s}.trial{k}.time.rectangle_time]=sm_wp10_time(sm.sub{p}.session{s}.trial{k}.result.time,...
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
        sm.sub{p}.session{s}.trial{k}.result.correct_final_alley]=sm_wp10_success(success_criterium,...
        sm.sub{p}.session{s}.trial{k}.result.final_distance,...
        sm.sub{p}.session{s}.trial{k}.result.final_distance_ego,...
        sm.sub{p}.session{s}.trial{k}.trial_goal,...
        sm.sub{p}.session{s}.trial{k}.chosen_goal_str);
    
    % Direct path to target
    sm.sub{p}.session{s}.trial{k}.result.direct_path=sm_wp10_directPath(sm.sub{p}.session{s}.trial{k}.start,...
        sm.sub{p}.session{s}.trial{k}.goal,...
        sm.sub{p}.session{s}.trial{k}.result.success,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_entry,...
        sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry,...
        sm.sub{p}.session{s}.trial{k}.zone.triangle_entry);
    
    % Search strategies
    [sm.sub{p}.session{s}.trial{k}.searchStrategy.direct,...
        sm.sub{p}.session{s}.trial{k}.searchStrategy.egocentric,...
        sm.sub{p}.session{s}.trial{k}.searchStrategy.allocentric,...
        sm.sub{p}.session{s}.trial{k}.searchStrategy.failed,...
        sm.sub{p}.session{s}.trial{k}.searchStrategy.reoriented,...
        sm.sub{p}.session{s}.trial{k}.searchStrategy.serial,...
        sm.sub{p}.session{s}.trial{k}.searchStrategy.central_focus,...
        sm.sub{p}.session{s}.trial{k}.searchStrategy.random_search,...
        sm.sub{p}.session{s}.trial{k}.searchStrategy.unclassified,...
        sm.sub{p}.session{s}.trial{k}.result.search_strategy_no]=sm_wp10_searchStrategy(sm.sub{p}.session{s}.trial{k}.trial_condition,...
        sm.sub{p}.session{s}.trial{k}.result.direct_path,...
        sm.sub{p}.session{s}.trial{k}.result.success,...
        sm.sub{p}.session{s}.trial{k}.result.success_ego,...
        sm.sub{p}.session{s}.trial{k}.result.arm_explored,...
        sm.sub{p}.session{s}.trial{k}.result.path_explored,...
        sm.sub{p}.session{s}.trial{k}.result.path_score,...
        sm.sub{p}.session{s}.trial{k}.zone.rel_pentagon_zone,...
        sm.sub{p}.session{s}.trial{k}.zone.alley_entry,...
        sm.sub{p}.session{s}.trial{k}.zone.rectangle_entry,...
        sm.sub{p}.session{s}.trial{k}.zone.triangle_entry);
    
    fprintf('Exploration analysis done for %d, session %d, file no %d.\n', subject, session, k);
    
end

% %% Marker for excluding trials % TBD 
% % Criteria: timeout, any start-goal combination where direct path is ambiguous (S2-G11, S3-G14, S6-G3, S7-G6)
% % ... or no movement/very short trial time (i.e. path_abs=0, body_rot_abs=0, or time < 3)
% sm.sub{p}.session{s}.trial{k}.exclude=0;
% if sm.sub{p}.session{s}.trial{k}.chosen_alley_int==999
%     sm.sub{p}.session{s}.trial{k}.exclude=1;
%     fprintf('Trial %d marked for exclusion due to timeout.\n',k);
% elseif (sm.sub{p}.session{s}.trial{k}.start==2 && sm.sub{p}.session{s}.trial{k}.goal==11) ...
%         || (sm.sub{p}.session{s}.trial{k}.start==3 && sm.sub{p}.session{s}.trial{k}.goal==14) ...
%         || (sm.sub{p}.session{s}.trial{k}.start==6 && sm.sub{p}.session{s}.trial{k}.goal==3) ...
%         || (sm.sub{p}.session{s}.trial{k}.start==7 && sm.sub{p}.session{s}.trial{k}.goal==6)
%     sm.sub{p}.session{s}.trial{k}.exclude=1;
%     fprintf('Trial %d marked for exclusion due to ambiguous path between start %d and goal %d.\n',...
%         k,sm.sub{p}.session{s}.trial{k}.start,sm.sub{p}.session{s}.trial{k}.goal);
% elseif sm.sub{p}.session{s}.trial{k}.trial_condition ~=4 % only for navigational trials
%     if (sm.sub{p}.session{s}.trial{k}.result.distance_traveled<=0.1 ...
%             || sm.sub{p}.session{s}.trial{k}.result.body_rotation==0 ...
%             || sm.sub{p}.session{s}.trial{k}.result.time < 3)
%         sm.sub{p}.session{s}.trial{k}.exclude=1;
%         disp('Trial %d marked for exclusion due no movement/short trial time.\n');
%     end
% end

% save data
save(fullfile(folderOut, targetFileName_Subject),'sm', '-append'); 

%% Block 4: Writing data ---> result sheet XLSX for single trials %%
% header
col_header={'wp','date_analysis','id','sex','group','session',...
    'trial','trial_condition','feedback','start_position',...
    'goal_position','goal_identity','chosen_alley'};

% main variables
col_header_2={'time_abs','path_abs','path_accuracy','final_distance_to_goal_abs',...
    'av_distance_to_goal_abs','distance_accuracy',...
    'path_accuracy_ego','final_distance_to_ego_abs','velocity',...
    'success','success_ego','correct_final_alley','direct_path',...
    'arm_explored','arm_score','path_explored','path_score',...
    'search_strategy_no','direct','reoriented','serial','central_focus','random_search',...
    'unclassified','failed_strategy','allocentric','egocentric',...
    'head_rotation_abs', 'full_head_rotation','head_turn_abs', 'head_turn_accuracy','head_turn_left', 'head_turn_right',...
    'body_rotation_abs','body_rotation_accuracy', 'body_turn_abs','body_turn_accuracy', 'body_turn_left', 'body_turn_right'};

% support analysis variables 
col_header_3={'goal_x','goal_y','goal_x_ego','goal_y_ego','ideal_path','ideal_path_ego',...
    'ideal_time','ideal_time_ego','ideal_headturnNo',...
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
    'time_rec_1', 'time_rec_2', 'time_rec_3', 'time_rec_4', 'time_rec_5'};

% name of excel-file
Trial=num2str(sm.sub{p}.session{s}.trial{k}.trial_num);
Session=num2str(sm.sub{p}.session{s}.session);
group_var=[sm.sub{p}.wp string(yyyymmdd(datetime)) sm.sub{p}.id sm.sub{p}.sex sm.sub{p}.group ...  
    sm.sub{p}.session{s}.session sm.sub{p}.session{s}.trial{k}.trial_num ...
    sm.sub{p}.session{s}.trial{k}.trial_condition sm.sub{p}.session{s}.trial{k}.fb ...
    sm.sub{p}.session{s}.trial{k}.start ...
    sm.sub{p}.session{s}.trial{k}.goal sm.sub{p}.session{s}.trial{k}.trial_goal_identity ...
    sm.sub{p}.session{s}.trial{k}.chosen_goal_int];
file_name = ['results_' num2str(wp) '_' sm.sub{p}.Group '_' ID '_' Session '_' Trial '.xls'];
new_file = fullfile(folderOut, file_name);

% write data
xlswrite(new_file,strrep([group_var ...
    sm.sub{p}.session{s}.trial{k}.result.time ...
    sm.sub{p}.session{s}.trial{k}.result.distance_traveled sm.sub{p}.session{s}.trial{k}.result.path_accuracy sm.sub{p}.session{s}.trial{k}.result.final_distance ...
    sm.sub{p}.session{s}.trial{k}.result.avg_distance sm.sub{p}.session{s}.trial{k}.result.distance_accuracy ...
    sm.sub{p}.session{s}.trial{k}.result.path_accuracy_ego sm.sub{p}.session{s}.trial{k}.result.final_distance_ego ...
    sm.sub{p}.session{s}.trial{k}.result.velocity ...
    sm.sub{p}.session{s}.trial{k}.result.success sm.sub{p}.session{s}.trial{k}.result.success_ego ...
    sm.sub{p}.session{s}.trial{k}.result.correct_final_alley sm.sub{p}.session{s}.trial{k}.result.direct_path ...
    sm.sub{p}.session{s}.trial{k}.result.arm_explored sm.sub{p}.session{s}.trial{k}.result.arm_score ...
    sm.sub{p}.session{s}.trial{k}.result.path_explored sm.sub{p}.session{s}.trial{k}.result.path_score ...
    sm.sub{p}.session{s}.trial{k}.result.search_strategy_no sm.sub{p}.session{s}.trial{k}.searchStrategy.direct ...
    sm.sub{p}.session{s}.trial{k}.searchStrategy.reoriented sm.sub{p}.session{s}.trial{k}.searchStrategy.serial ...
    sm.sub{p}.session{s}.trial{k}.searchStrategy.central_focus sm.sub{p}.session{s}.trial{k}.searchStrategy.random_search ...
    sm.sub{p}.session{s}.trial{k}.searchStrategy.unclassified sm.sub{p}.session{s}.trial{k}.searchStrategy.failed ...
    sm.sub{p}.session{s}.trial{k}.searchStrategy.allocentric sm.sub{p}.session{s}.trial{k}.searchStrategy.egocentric ...
    sm.sub{p}.session{s}.trial{k}.result.head_rotation sm.sub{p}.session{s}.trial{k}.result.full_head_rotation ...
    sm.sub{p}.session{s}.trial{k}.result.head_turn_total sm.sub{p}.session{s}.trial{k}.result.head_turn_accuracy ...
    sm.sub{p}.session{s}.trial{k}.result.head_turn_left sm.sub{p}.session{s}.trial{k}.result.head_turn_right...
    sm.sub{p}.session{s}.trial{k}.result.body_rotation sm.sub{p}.session{s}.trial{k}.result.body_rotation_accuracy ...
    sm.sub{p}.session{s}.trial{k}.result.body_turn_total sm.sub{p}.session{s}.trial{k}.result.body_turn_accuracy ...
    sm.sub{p}.session{s}.trial{k}.result.body_turn_left sm.sub{p}.session{s}.trial{k}.result.body_turn_right],'.', ','),'data_vars','A2');

xlswrite(new_file,strrep([group_var ...
    sm.sub{p}.session{s}.trial{k}.goal_x sm.sub{p}.session{s}.trial{k}.goal_y ...
    sm.sub{p}.session{s}.trial{k}.goal_x_ego sm.sub{p}.session{s}.trial{k}.goal_y_ego ...
    sm.sub{p}.session{s}.trial{k}.ideal_path sm.sub{p}.session{s}.trial{k}.ideal_path_ego ...
    sm.sub{p}.session{s}.trial{k}.ideal_time sm.sub{p}.session{s}.trial{k}.ideal_time_ego sm.sub{p}.session{s}.trial{k}.ideal_headturnNo ...
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
    sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,1) sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,2) sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,3) sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,4) sm.sub{p}.session{s}.trial{k}.time.rectangle_time(1,5)],'.', ','),'support_vars','A2');

xlswrite(new_file,[col_header col_header_2],'data_vars','A1');
xlswrite(new_file,[col_header col_header_3],'support_vars','A1');

%% Create plots    
sm_wp10_plot_track(num2str(wp), sm.sub{p}.session{s}.trial{k}.trial_num,sm.sub{p}.session{s}.trial{k}.feedback,sm.sub{p}.session{s}.session,sm.sub{p}.session{s}.trial{k}.trial_condition,sm.sub{p}.id,sm.sub{p}.Group,name,...
    alley_polyshape_1, alley_polyshape_2, tri, rec,x,y,x_line_ego,y_line_ego,x_line,y_line,folderOut,sm.sub{p}.session{s}.trial{k}.goal_x,sm.sub{p}.session{s}.trial{k}.goal_y)

end

end

%% Write summaries for a selection of variables
new_name2 = [sm.sub{p}.Group '_' num2str(sm.sub{p}.id)  '_results'];
new_file = fullfile(folderOut2, new_name2);
sm_wp10_table_allTrials(folderOut,new_file,col_header,col_header_2,col_header_3);

% % Write table & summaries
% new_file = fullfile(folderOut2, [new_name2 '.xls']); % TBD %
% sm_wp10_summary(new_file);

p=p+1;

end 

%% Save data
targetFilePath         = [folderOut2, targetFileName];  
save(targetFilePath, 'sm', '-append')

clear