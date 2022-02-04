clear; close all; clc; format long;
addpath(genpath(pwd)) % add subfolder functions to path

%% Starmaze Data Processing
% @ date 2019-10-01 @ author Deetje Iggena
% @ date 2020-11-06 update by @ Patrizia Maier & now tracked by git
% for Starmaze version WP10 Frankfurt
% requires Matlab 2021a or later
% requires Signal Processing Toolbox for downsample function

% The script requires .csv files as input.
% One tracker file per trial with timestamp, x- and y-coordinates for movememt
% and z-coordinates for rotation.
% One trial_results file with general information (id, session, condition).

% The script starts with input:
%   1. Provide the span of participants you would like to analyse
%   2. Provide the total number of recorded sessions.

% Block 1: Set up input/output, Starmaze and Practise environment
% Block 2: Data preprocessing
% Block 3: Analysis

%% Block 1: Set up input and output folders, starmaze and practise environment 
%% Data input folder and participant information
[data_folder]  = sm_inputPath(); % provide folder with all raw data
[participant_start,participant_end] = sm_inputSubjectsNo(); % provide participant range
n_sessions      = 3; % default 3

%% Result folder
result_folder=[data_folder '\WP10_results'];
if ~exist(result_folder, 'dir')
    mkdir(result_folder);
    disp('Your output folder did not exist, a new folder was created.')
end

%% Load data table or create new one 
% load existing data
file_name         = '\wp10_results_table.mat';
file_path         = fullfile(result_folder, file_name);
if isfile(file_path)
    load(file_path)
end

% initialize if non-existing
if ~exist('sm','var')
    sm = []; 

    %% Set up Starmaze environment 
    % coordinates of min/max values
    values=table2array(readtable('wp10_values.csv'));
    [sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax]=sm_wp10_minMaxValues(values);

    % coordinates of start positions (normalized!)
    start=table2array(readtable('wp10_start.csv'));
    [sm.coord.start_x,sm.coord.start_y]=sm_wp10_start(start,sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax);

    % coordinates of goal positions (normalized!)
    goal=table2array(readtable('wp10_goal.csv'));
    [sm.coord.goal_x,sm.coord.goal_y]=sm_wp10_goal(goal,sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax);

    % coordinates of alley corners (normalized!)
    alley_x=table2array(readtable('wp10_alley_x.csv'));
    [n_corners,n_alleys] = size(alley_x);
    for i_alley=1:n_alleys
        for i_corner=1:n_corners
            alley_x(i_corner,i_alley)=datanorm(alley_x(i_corner,i_alley),sm.coord.xmin,sm.coord.xmax);
        end
    end
    alley_y=table2array(readtable('wp10_alley_y.csv'));
    for i_alley=1:n_alleys
        for i_corner=1:n_corners
            alley_y(i_corner,i_alley)=datanorm(alley_y(i_corner,i_alley),sm.coord.ymin,sm.coord.ymax);
        end
    end

    % coordinates of combined pentagon (normalized!) and central polyshape
    pentagon_x=table2array(readtable('wp10_pentagon_x.csv'));
    pentagon_y=table2array(readtable('wp10_pentagon_y.csv'));
    [sm.coord.central_x,sm.coord.central_y,sm.coord.central_poly,pentagon_x,pentagon_y]=sm_wp10_pentagon(alley_x,alley_y,pentagon_x,pentagon_y,...
        sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax);

    % create other polyshapes
    % alley polyshape
    [sm.coord.alley_full_x,sm.coord.alley_full_y,sm.coord.alley_poly,...
        sm.coord.alley_half_out_x,sm.coord.alley_half_out_y,sm.coord.alley_poly_out,...
        sm.coord.alley_half_in_x,sm.coord.alley_half_in_y,sm.coord.alley_poly_in]=sm_wp10_alleyPolyshape(alley_x,alley_y);
    % rectangle polyshape
    [sm.coord.rec_x,sm.coord.rec_y,sm.coord.rec_poly]=sm_wp10_rectPolyshape(n_alleys,alley_x,alley_y,pentagon_x,pentagon_y);
    % triangle polyshape
    [sm.coord.tri_x,sm.coord.tri_y,sm.coord.tri_poly]=sm_wp10_trianglePolyshape(n_alleys,alley_x,alley_y,pentagon_x,pentagon_y);
    % joint polyshape
    sm.coord.full_poly=[sm.coord.alley_poly_out{1,1} sm.coord.alley_poly_in{1,1}...
        sm.coord.alley_poly_out{1,2} sm.coord.alley_poly_in{1,2}...
        sm.coord.alley_poly_out{1,3} sm.coord.alley_poly_in{1,3}...
        sm.coord.alley_poly_out{1,4} sm.coord.alley_poly_in{1,4}...
        sm.coord.alley_poly_out{1,5} sm.coord.alley_poly_in{1,5} sm.coord.central_poly];

    % create graph
    % for automated shortest path calculation (requires Matlab 2021a)
    [sm.coord.graph,sm.coord.graph_x,sm.coord.graph_y]=sm_wp10_createGraph(sm.coord.start_x,sm.coord.start_y,...
        sm.coord.tri_x,sm.coord.tri_y,sm.coord.goal_x,sm.coord.goal_y);

    % add (ordered) information
    sm.coord.goal_names=["MA", "MC", "MI"];
    % sm.coord.start_names=["Player_MA" "Player_MB" "Player_MC" "Player_MD" "Player_ME" "Player_MF" "Player_MG" ...
    %     "Player_MH" "Player_MI", "Player_MJ", "Player_MX"];
    sm.coord.start_names=["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "X"];
    sm.coord.alley_names=["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"];

    % create test figure plot
    % sm_wp10_testfig("s_maze",sm.coord.full_poly,sm.coord.goal_x,sm.coord.goal_y,sm.coord.start_x,sm.coord.start_y,sm.coord.goal_names,sm.coord.graph,sm.coord.graph_x,sm.coord.graph_y);

    clear alley_x alley_y pentagon_x pentagon_y i_alley n_alleys i_corner n_corners start goal values; 
    
    %% Set up Practise environment (for motor control task)
    % coordinates of min/max values
    practise_values=table2array(readtable('wp10_practise_values.csv'));
    [sm.coord.practise.xmin,sm.coord.practise.xmax,sm.coord.practise.ymin,sm.coord.practise.ymax]=sm_wp10_minMaxValues(practise_values);

    % coordinates of start position (normalized!)
    practise_start=table2array(readtable('wp10_practise_start.csv'));
    [sm.coord.practise.start_x,sm.coord.practise.start_y]=sm_wp10_start(practise_start,sm.coord.practise.xmin,sm.coord.practise.xmax,sm.coord.practise.ymin,sm.coord.practise.ymax);

    % coordinates of goal positions (normalized!)
    practise_goal=table2array(readtable('wp10_practise_goal.csv'));
    [sm.coord.practise.goal_x,sm.coord.practise.goal_y]=sm_wp10_goal(practise_goal,sm.coord.practise.xmin,sm.coord.practise.xmax,sm.coord.practise.ymin,sm.coord.practise.ymax);

    % coordinates of alley corners (normalized!)
    practise_alley_x=table2array(readtable('wp10_practise_x.csv'));
    [n_corners,~] = size(practise_alley_x);
    for i_corner=1:n_corners
        practise_alley_x(i_corner,1)=datanorm(practise_alley_x(i_corner,1),sm.coord.practise.xmin,sm.coord.practise.xmax);
    end
    practise_alley_y=table2array(readtable('wp10_practise_y.csv'));
    for i_corner=1:n_corners
        practise_alley_y(i_corner,1)=datanorm(practise_alley_y(i_corner,1),sm.coord.practise.ymin,sm.coord.practise.ymax);
    end

    % create polyshape
    sm.coord.practise.practise_poly=polyshape(practise_alley_x(:),practise_alley_y(:));

    % add (ordered) information
    sm.coord.practise.practise_goal_names=["1", "2", "3", "4", "5", "6", "7", "8" , "9", "10" ];
    sm.coord.practise.practise_start_names="Player_P0";

    % create test figure plot
    % sm_wp10_testfig("p_maze",sm.coord.practise.practise_poly,sm.coord.practise.goal_x,sm.coord.practise.goal_y,sm.coord.practise.start_x,sm.coord.practise.start_y,sm.coord.practise.practise_goal_names,sm.coord.graph,sm.coord.graph_x,sm.coord.graph_y);

    clear practise_alley_x practise_alley_y i_corner n_corners practise_start practise_goal practise_values; 
    
    %% Save sm data 
    save(file_path, 'sm');
end

% get data index % TBD check this
[~,n_participants]=size(sm);
p=n_participants+1;

%% Block 2: Data preprocessing
for i_participant=participant_start:participant_end
    
    for s=1:n_sessions
        session_string=convertStringsToChars("S00" + s);
        
        %% Individual input and output folder
        % input folder
        input_folder=[data_folder '\' num2str(i_participant) '\' session_string];
        if ~exist(input_folder, 'dir')
            fprintf('Folder for participant %d, session %d does not exist. Continue with next iteration.\n', i_participant, session);
            continue;
        end
        
        % output folder (only for trial plots)
        output_folder=[data_folder '\' num2str(i_participant) '\results'];
        if ~exist(output_folder, 'dir')
            mkdir(output_folder);
            fprintf('Your output folder for participant %d did not exist, a new folder was created.\n', i_participant);
        end
        
%         % save individual matlab file % TBD remove
%         file_name_Subject = ['\' num2str(i_participant) '_results_table.mat'];
%         save(fullfile(output_folder, file_name_Subject), 'sm');
        
        %% Read-in trial file
        opts=detectImportOptions([input_folder, '\trial_results.csv']);
        opts=setvaropts(opts,'timestamp','InputFormat','MM/dd/uuuu hh:mm:ss aa');
        trial_data=readtable([input_folder, '\trial_results.csv'], opts);
        
        % add participant information to sm
        sm.participant(p).id=i_participant;
        [sm.participant(p).group,sm.participant(p).group_s, ...
            sm.participant(p).sex,sm.participant(p).sex_s]=sm_wp10_callGroup(sm.participant(p).id);
        sm.participant(p).session(s).session_number=trial_data.session_num(1,1);
        
        %% Read-in log file
        opts=detectImportOptions([input_folder, '\log.csv']);
        opts.SelectedVariableNames = {'message'};
        log_data=table2cell(readtable([input_folder, '\log.csv'], opts)); % read in log-file-info
        log_data=log_data(contains(log_data,'ID is'));
        [sm.participant(p).session(s).rand_dict]=sm_wp10_preprocLogData(log_data, i_participant);
        
        %% Read-in individual tracker trial files
        d=dir(fullfile(input_folder, '*.xlsx')); % every .xlsx is detected --> change if different file-format
        
        % first time: clean and convert csv to xlsx files
        if isempty(d)
            
            % preprocess individual tracker trial csv files
            sm_wp10_dataPrep(input_folder,i_participant,sm.participant(p).group_s,s);
            
            % update d
            d=dir(fullfile(input_folder, '*.xlsx'));
        end
        
        files={d.name};
        
        for k=1:numel(files)
            name=files{k};
            
            % for practise only process trial 2 (motor control task)
            if s==3
                pattern=("_T001"|"_T003"|"_T004"|"_T005"|"_T006"|"_T007");
                if contains(name, pattern)
                    continue
                end
            end
            
            % read-in data
            data=readtable(fullfile(input_folder, name));
            
            % original sampling rate
            sampling_rate_original=zeros(length(data.time)-1,1);
            for i=1:length(data.time)-1
                sampling_rate_original(i)=data.time(i+1)-data.time(i);
            end
            sm.participant(p).session(s).trial(k).support.sampling_rate=sum(sampling_rate_original)/length(sampling_rate_original);
                       
%             %%% ACTIVATE IF SAMPLING RATE DIFFERS %%% 
%             %%% needs to be checked if appropriate (alternative function 'decimate', 'resample' or activate only for higher sampling rates) %%% 
%             % temporal normalization (downsampling) 
%             data = downsample(data,round(sm.participant(p).session(s).trial(k).support.sampling_rate*1000));
%             
%             % new sampling rate %%% TBC %%%
%             sampling_rate_new=zeros(length(data.time)-1,1);
%             for i=1:length(data.time)-1
%                 sampling_rate_new(i)=data.time(i+1)-data.time(i);
%             end
%             sm.participant(p).session(s).trial(k).support.new_sampling_rate=sum(sampling_rate_new)/length(sampling_rate_new);
            
            % extract data 
            t=data.time; % time
            x=data.pos_x; % coordinates
            y=data.pos_z; % coordinates
            r=data.rot_y; % head rotations
            data_length=length(x);
            sdata_length=(size(x))-1;
            
            % spatial normalization
            if s==3 % practise maze
                x=datanorm(x,sm.coord.practise.xmin,sm.coord.practise.xmax); y=datanorm(y,sm.coord.practise.ymin,sm.coord.practise.ymax);
            else % star maze
                x=datanorm(x,sm.coord.xmin,sm.coord.xmax); y=datanorm(y,sm.coord.ymin,sm.coord.ymax);
            end
            
            sm.participant(p).session(s).trial(k).result.x_start=x(1); sm.participant(p).session(s).trial(k).result.y_start=y(1);
            sm.participant(p).session(s).trial(k).result.x_end=x(end); sm.participant(p).session(s).trial(k).result.y_end=y(end);
            
            % unique x-/y values,excluding periods without movement
            xy_unique = unique([x y],'rows','stable'); % excluding row duplicates
            x_unique = xy_unique(:,1);
            y_unique = xy_unique(:,2);
                       
            %% Get single trial info from trial_results
            sm.participant(p).session(s).session_duration=round(minutes(trial_data.timestamp(numel(files),1) - trial_data.timestamp(1,1))); 
            
            sm.participant(p).session(s).trial(k).block=trial_data.block_num(k,1);
            sm.participant(p).session(s).trial(k).trial_num=trial_data.trial_num(k,1);
            sm.participant(p).session(s).trial(k).trial_in_block=trial_data.trial_num_in_block(k,1);
            
            sm.participant(p).session(s).trial(k).feedback_s=string(trial_data.trial_feedback(k,1));
            sm.participant(p).session(s).trial(k).feedback=int8(strcmp(sm.participant(p).session(s).trial(k).feedback_s,'true'));
            
            sm.participant(p).session(s).trial(k).condition_s=string(trial_data.trial_type(k,1));
            sm.participant(p).session(s).trial(k).condition=sm_wp10_trialCondition(sm.participant(p).session(s).trial(k).condition_s,sm.participant(p).session(s).trial(k).feedback);
            
            n_goals=4;
            sm.participant(p).session(s).trial(k).goal_identity_s=string(trial_data.trial_goal_identity(k,1));
            sm.participant(p).session(s).trial(k).goal_identity=sm_wp10_trialGoalIdentity(n_goals, char(trial_data.trial_goal_identity(k,1)));
            
            [sm.participant(p).session(s).trial(k).goal_x,sm.participant(p).session(s).trial(k).goal_y,...
                sm.participant(p).session(s).trial(k).goal_i,sm.participant(p).session(s).trial(k).goal_s,...
                sm.participant(p).session(s).trial(k).goal_alley]=sm_wp10_trialGoal(char(string(trial_data.trial_goal(k,1))),...
                sm.coord.goal_x,sm.coord.goal_y,sm.coord.goal_names,sm.coord.alley_names);
            
            start_name=char(trial_data.trial_player(k,1)); sm.participant(p).session(s).trial(k).start_s=start_name(end);
            [sm.participant(p).session(s).trial(k).start_i]=sm_wp10_trialStart(sm.participant(p).session(s).trial(k).start_s,sm.coord.start_names);
            
            %% Time analysis
            sm.participant(p).session(s).trial(k).result.time=sm_time(t(1),t(end)); % total amount of time
            
            fprintf('Time analysis done for %d, session %d, file no %d.\n', i_participant, s, k);
            
            %% For motor control navigation trial
            % analysis of path and time
            if sm.participant(p).session(s).trial(k).condition==4
                %% Calculate variables depending on single trial settings
                % ideal path coordinates & length
                x_line_motor=[sm.coord.practise.start_x; sm.coord.practise.goal_x]; y_line_motor=[sm.coord.practise.start_y; sm.coord.practise.goal_y];
                sm.participant(p).session(s).trial(k).support.ideal_path_length=sm_wp10_idealPathLength(x_line_motor,y_line_motor);
                
                % interpolate data for further analysis
                % using 'interparc' function by John D'Errico (Matlab File Exchanger)
                [xi_al,yi_al]=sm_wp10_dataInterpolation(x_line_motor,y_line_motor,...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length);
                
                %% Time analysis: already done
                %% Coordinate analysis using x-/y-coordinates
                % Path analysis
                sm.participant(p).session(s).trial(k).result.path_length=0; % reset/initiate variables
                for i=1:sdata_length
                    % PATH to all TARGETS
                    % cumulative distance traveled (used in path accuracy)
                    sm.participant(p).session(s).trial(k).result.path_length=sm.participant(p).session(s).trial(k).result.path_length+sum(sm_distance(x(i),x(i+1),y(i),y(i+1)));
                end
                              
                % PATH ACCURACY to all TARGETS
                sm.participant(p).session(s).trial(k).result.path_accuracy=sm_ac(sm.participant(p).session(s).trial(k).result.path_length,sm.participant(p).session(s).trial(k).support.ideal_path_length);
                 
                % VELOCITY
                sm.participant(p).session(s).trial(k).result.velocity=sm.participant(p).session(s).trial(k).result.path_length/sm.participant(p).session(s).trial(k).result.time;
                
                fprintf('Motor control analysis done for %d, session %d, file no %d.\n', i_participant, s, k);
                
            else
                %% For all other navigation trials
                %% Calculate variables depending on single trial settings
                % ideal path coordinates & length, ideal egocentric path coordinates & length
                % Caution: dummy values for egocentric for inner starts (because no clear ideal egocentric path)
                [sm.participant(p).session(s).trial(k).support.goal_x_ego, ...
                    sm.participant(p).session(s).trial(k).support.goal_y_ego, ...
                    x_line, y_line, x_line_chosen, y_line_chosen, x_line_ego, y_line_ego, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length_chosen, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length_ego, ...
                    sm.participant(p).session(s).trial(k).support.ego_alley, ...
                    sm.participant(p).session(s).trial(k).support.ideal_headturnNo]=sm_wp10_depStartVariables(...
                    sm.coord.graph, sm.coord.graph_x, sm.coord.graph_y, ...
                    sm.participant(p).session(s).trial(k).start_i, ...
                    sm.participant(p).session(s).trial(k).goal_i,...
                    sm.participant(p).session(s).trial(k).result.x_end, ...
                    sm.participant(p).session(s).trial(k).result.y_end, ...
                    sm.coord.alley_full_x, sm.coord.alley_full_y, ...
                    sm.coord.rec_x, sm.coord.rec_y, sm.coord.central_poly, sm.coord.full_poly);
                
                % interpolate data for further analysis
                % using 'interparc' function by John D'Errico (Matlab File Exchanger)
                [xi_al,yi_al]=sm_wp10_dataInterpolation(x_line, y_line, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length);
                
                [xi_ch,yi_ch]=sm_wp10_dataInterpolation(x_line_chosen, y_line_chosen, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length_chosen);
                
                [xi_eg,yi_eg]=sm_wp10_dataInterpolation(x_line_ego, y_line_ego, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length_ego);
                
%                     % test plot
%                     figure;
%                     plot(sm.coord.full_poly);
%                     hold on
%                     plot(x_line, y_line, 'k+', xi_al, yi_al, 'k-',...
%                         x_line_ego, y_line_ego, 'rx', xi_eg, yi_eg, 'r-',...
%                         x_line_chosen, y_line_chosen, 'bx', xi_ch, yi_ch, 'b-');
%                     xlim([0 1]);
%                     ylim([0 1]);
%                     hold off
               
                % zone analysis for ideal paths
                [~, ~, sm.participant(p).session(s).trial(k).support.zone.ideal_alley_entry, ...
                    ~]=sm_wp10_coordinatesZonesStatic(xi_al,...
                    yi_al, zeros(length(xi_al),1), sm.coord.alley_full_x, sm.coord.alley_full_y);
                
                [~, ~, sm.participant(p).session(s).trial(k).support.zone.ideal_rectangle_entry, ...
                    ~]= sm_wp10_coordinatesZonesStatic(xi_al,...
                    yi_al, zeros(length(xi_al),1), sm.coord.rec_x, sm.coord.rec_y);
                
%                 [ideal_alley_entry_mat]=sm_wp10_coordinatesZonesDynamic(xi_al,...
%                     yi_al, sm.coord.alley_full_x, sm.coord.alley_full_y);
%                 [uniq_alley]=unique(ideal_alley_entry_mat,'rows');
%                 
%                 [ideal_rectangle_entry_mat]=sm_wp10_coordinatesZonesDynamic(xi_al,...
%                     yi_al, sm.coord.rec_x, sm.coord.rec_y);
%                 [uniq_rect]=unique(ideal_rectangle_entry_mat,'rows');
%                 if mod(sm.participant(p).session(s).trial(k).start_i,2) % even number = outer start
%                     uniq_rect=uniq_rect(2:end,:); % remove first row (start) for outer starts, as it's always zero
%                 end
                
                %% Block 3: Data analysis, i.e. calculcation of variables
                %% Time analysis: already done
                %% Chosen goal location
                [sm.participant(p).session(s).trial(k).result.chosen_goal_i, ...
                    sm.participant(p).session(s).trial(k).result.chosen_alley_str, ...
                    sm.participant(p).session(s).trial(k).result.chosen_alley_int, ...
                    sm.participant(p).session(s).trial(k).result.obj_at_chosen_loc]=sm_wp10_chosenGoal(...
                    sm.participant(p).session(s).rand_dict, ...
                    char(trial_data.chosen_goal(k,1)), sm.coord, ...
                    sm.participant(p).session(s).trial(k).result.x_end, ...
                    sm.participant(p).session(s).trial(k).result.y_end);
                               
                %% Standard coordinate analysis using x-/y-coordinates
                % Path analysis
                sm.participant(p).session(s).trial(k).result.path_length=0; total_dist_to_goal=0; total_dist_to_goal_ego=0; total_dist_to_chosen_goal=0; % reset/initiate variables
                for i=1:sdata_length
                    % PATH to target
                    % cumulative distance traveled (used in path accuracy)
                    sm.participant(p).session(s).trial(k).result.path_length=sm.participant(p).session(s).trial(k).result.path_length+sm_distance(x(i),x(i+1),y(i),y(i+1));
%                     % DISTANCE to CORRECT target
%                     % cumulative distance to target (used in distance analysis)
%                     total_dist_to_goal=total_dist_to_goal+sm_distance(x(i),sm.participant(p).session(s).trial(k).goal_x,y(i),sm.participant(p).session(s).trial(k).goal_y);
%                     % DISTANCE to EGOCENTRIC target
%                     % cumulative distance to egocentric target (used in distance analysis)
%                     total_dist_to_goal_ego=total_dist_to_goal_ego+sm_distance(x(i),sm.participant(p).session(s).trial(k).support.goal_x_ego,y(i),sm.participant(p).session(s).trial(k).support.goal_y_ego);
%                     % DISTANCE to CHOSEN target
%                     % cumulative distance to chosen target (used in distance analysis)
%                     total_dist_to_chosen_goal=total_dist_to_chosen_goal+sm_distance(x(i),x(end),y(i),y(end));
                end
                
                % Distance analysis
                % FINAL DISTANCE to CORRECT target
                sm.participant(p).session(s).trial(k).result.final_distance=0;
                if sm.participant(p).session(s).trial(k).feedback==0
                    sm.participant(p).session(s).trial(k).result.final_distance=sm_distance(...
                        sm.participant(p).session(s).trial(k).goal_x, ...
                        sm.participant(p).session(s).trial(k).result.x_end, ...
                        sm.participant(p).session(s).trial(k).goal_y, ...
                        sm.participant(p).session(s).trial(k).result.y_end);
                end
                
                % AVERAGE DISTANCE to CORRECT path (full trajectory including waiting/rotation)
                [~,distance_to_path] = dsearchn([xi_al, yi_al],[x, y]); % returns euclidian distance to nearest neighbour on interpolated ideal path
                distance_to_path = [distance_to_path; sm.participant(p).session(s).trial(k).result.final_distance]; % add final distance as last data point
                sm.participant(p).session(s).trial(k).result.avg_distance_path=mean(distance_to_path);
                sm.participant(p).session(s).trial(k).result.total_distance_path=sum(distance_to_path);
                
                % AVERAGE DISTANCE to CORRECT path (only path, remove duplicates due to waiting/rotation)
                [~,distance_to_path_unique] = dsearchn([xi_al, yi_al], xy_unique); % returns euclidian distance to nearest neighbour on interpolated ideal path
                distance_to_path_unique = [distance_to_path_unique; sm.participant(p).session(s).trial(k).result.final_distance]; % add final distance as last data point
                sm.participant(p).session(s).trial(k).result.avg_distance_path_pure=mean(distance_to_path_unique);
                sm.participant(p).session(s).trial(k).result.total_distance_path_pure=sum(distance_to_path_unique);
                
               
%                 %%% EXPLORATORY
%                 dtw([xi_al, yi_al]',xy_unique');
%                 dtw([xi_al, yi_al]',[x, y]');
% 
%                 [sdtw,~,~] = dtw([xi_al, yi_al]', xy_unique'); 
%                 ncdtw = exp(-(sdtw/length(xi_al)*10)); %*0.1 * 100
%                 ndtw = exp(-(sdtw/length(xi_al))); 
%                 
%                 %%%
%                 [dtw_trajectory,i1,i2] = dtw([xi_al, yi_al]',xy_unique');
%                 ndtw = exp(-(dtw_trajectory/length(xy_unique)*0.1)); 
%                 X = [xi_al(i1),xi_al(i1)]; Y = [x_unique(i2),y_unique(i2)];
%                 [dissimimlarityRatio,Z,~] = procrustes(X,Y,'Scaling',false,'reflection',false);
%                 figure; 
%                 plot(X(:,1),X(:,2),'r.',Y(:,1),Y(:,2),'y.',...
%                     Z(:,1),Z(:,2),'b.'); 
%                 xlim([0 1]);
%                 ylim([0 1]);
%                 
%                 % test plot
%                 figure;
%                 plot(sm.coord.full_poly);
%                 hold on
%                 plot(xi_al, yi_al, 'k-', x, y, 'r-',...
%                     x_unique, y_unique, 'g--');
%                 xlim([0 1]);
%                 ylim([0 1]);
%                 hold off
%                 
%                 %%% EXPLORATORY
                
                
%                 % AVERAGE DISTANCE to CORRECT target
%                 sm.participant(p).session(s).trial(k).result.avg_distance_target=total_dist_to_goal/sdata_length(1);
%                 sm.participant(p).session(s).trial(k).result.total_distance_target=total_dist_to_goal;
%                 sm.participant(p).session(s).trial(k).result.sum_data_points=sdata_length(1);
                
                % Cumulative IDEAL DISTANCE to CORRECT target
                id_total_dist_to_goal=0; % start-initiation
                xi_length=length(xi_al)-1;
                for i=1:xi_length
                    % ideal cumulative distance to allocentric target
                    id_total_dist_to_goal=id_total_dist_to_goal+sm_distance(xi_al(i),sm.participant(p).session(s).trial(k).goal_x,yi_al(i),sm.participant(p).session(s).trial(k).goal_y);
                end
                
%                 % IDEAL AVERAGE DISTANCE to CORRECT target
%                 sm.participant(p).session(s).trial(k).support.ideal_avg_distance_target=id_total_dist_to_goal/xi_length;
%                 sm.participant(p).session(s).trial(k).support.ideal_total_distance_target=id_total_dist_to_goal;
%                 sm.participant(p).session(s).trial(k).support.ideal_sum_data_points=xi_length;
                
                % PATH ACCURACY to CORRECT target
                sm.participant(p).session(s).trial(k).result.path_accuracy=sm_ac(...
                    sm.participant(p).session(s).trial(k).result.path_length, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length);
                
%                 % DISTANCE ACCURACY to CORRECT target
%                 sm.participant(p).session(s).trial(k).result.distance_accuracy=sm_ac(...
%                     sm.participant(p).session(s).trial(k).result.avg_distance_target, ...
%                     sm.participant(p).session(s).trial(k).support.ideal_avg_distance_target);
                
                % VELOCITY
                sm.participant(p).session(s).trial(k).result.velocity=sm.participant(p).session(s).trial(k).result.path_length / ...
                    sm.participant(p).session(s).trial(k).result.time;
                
                fprintf('Path, distance and velocity analysis done for %d, session %d, file no %d.\n', i_participant, s, k);
                
                %% Additional analysis for all probe trials: Distance in relation to chosen target
                
                if sm.participant(p).session(s).trial(k).feedback==0
                    % Path analysis to CHOSEN target: same as above
                    % PATH to CHOSEN target: same as above
                    % DISTANCE to CHOSEN target: see as above
                    
                    % AVERAGE DISTANCE to ideal PATH to CHOSEN target (full trajectory including waiting/rotation)
                    [~,distance_to_chosen_path] = dsearchn([xi_ch, yi_ch],[x,y]); % returns euclidian distance to nearest neighbour on interpolated ideal path
                    sm.participant(p).session(s).trial(k).result.avg_distance_chosen_path=mean(distance_to_chosen_path);
                    sm.participant(p).session(s).trial(k).result.total_distance_chosen_path=sum(distance_to_chosen_path);
                    
                    % AVERAGE DISTANCE to ideal PATH to CHOSEN target (only path, remove duplicates due to waiting/rotation)
                    [~,distance_to_chosen_path_unique] = dsearchn([xi_ch, yi_ch],xy_unique); % returns euclidian distance to nearest neighbour on interpolated ideal path
                    sm.participant(p).session(s).trial(k).result.avg_distance_chosen_path_pure=mean(distance_to_chosen_path_unique);
                    sm.participant(p).session(s).trial(k).result.total_distance_chosen_path_pure=sum(distance_to_chosen_path_unique);
                    
%                     % AVERAGE DISTANCE to CHOSEN target
%                     sm.participant(p).session(s).trial(k).result.avg_distance_chosen_target=total_dist_to_chosen_goal/sdata_length(1);
%                     sm.participant(p).session(s).trial(k).result.total_distance_chosen_target=total_dist_to_chosen_goal;
%                     % sum data points: same as in analyis above
                    
%                     % Cumulative IDEAL DISTANCE to CHOSEN target
%                     id_total_dist_to_goal_chosen=0; % start-initiation
%                     xi_ch_length=length(xi_ch)-1;
%                     for i=1:xi_ch_length
%                         % ideal cumulative distance to chosen target
%                         id_total_dist_to_goal_chosen=id_total_dist_to_goal_chosen+sm_distance(xi_ch(i),x(end),yi_ch(i),y(end));
%                     end
                    
%                     % IDEAL AVERAGE DISTANCE to CHOSEN target
%                     sm.participant(p).session(s).trial(k).support.ideal_avg_distance_chosen_target=id_total_dist_to_goal_chosen/xi_ch_length;
%                     sm.participant(p).session(s).trial(k).support.ideal_total_distance_chosen_target=id_total_dist_to_goal_chosen;
%                     sm.participant(p).session(s).trial(k).support.ideal_sum_data_points_chosen=xi_ch_length;
                     
                    % PATH ACCURACY to CHOSEN target
                    sm.participant(p).session(s).trial(k).result.path_accuracy_chosen=sm_ac(sm.participant(p).session(s).trial(k).result.path_length,sm.participant(p).session(s).trial(k).support.ideal_path_length_chosen);
                    
%                     % DISTANCE ACCURACY to CHOSEN target
%                     sm.participant(p).session(s).trial(k).result.distance_accuracy_chosen=sm_ac(sm.participant(p).session(s).trial(k).result.avg_distance_chosen_target,sm.participant(p).session(s).trial(k).support.ideal_avg_distance_chosen_target);
                else
                    % set dummy variables
                    sm.participant(p).session(s).trial(k).result.avg_distance_chosen_path=999;
                    sm.participant(p).session(s).trial(k).result.total_distance_chosen_path=999;
                    sm.participant(p).session(s).trial(k).result.avg_distance_chosen_path_pure=999;
                    sm.participant(p).session(s).trial(k).result.total_distance_chosen_path_pure=999;
                    sm.participant(p).session(s).trial(k).result.avg_distance_chosen_target=999;
                    sm.participant(p).session(s).trial(k).result.total_distance_chosen_target=999;
                    sm.participant(p).session(s).trial(k).result.path_accuracy_chosen=999;
                    sm.participant(p).session(s).trial(k).result.distance_accuracy_chosen=999;
                    
                    sm.participant(p).session(s).trial(k).support.ideal_path_length_chosen=999;
                    sm.participant(p).session(s).trial(k).support.ideal_avg_distance_chosen_target=999;
                    sm.participant(p).session(s).trial(k).support.ideal_total_distance_chosen_target=999;
                    sm.participant(p).session(s).trial(k).support.ideal_sum_data_points_chosen=999;
                end
                
                fprintf('Additional: Distance in relation to chosen target done for %d, session %d, file no %d.\n', i_participant, s, k);
          
                %% Additional analysis for allocentric probe trials with potential egocentric response
                % excludes allocentric trials with inner starts (even integer) as there
                % is no clearly identifiable egocentric path/goal location from these starts
                
                if sm.participant(p).session(s).trial(k).condition==1 && ...
                        mod(sm.participant(p).session(s).trial(k).start_i,2)
                    % Path analysis to EGOCENTRIC target: same as above
                    % PATH to CHOSEN target: same as above
                    % DISTANCE to EGO target: see as above
                    
                    % Distance analysis
                    % FINAL DISTANCE to EGOCENTRIC target
                    sm.participant(p).session(s).trial(k).result.final_distance_ego=sm_distance(...
                        sm.participant(p).session(s).trial(k).support.goal_x_ego,...
                        sm.participant(p).session(s).trial(k).result.x_end,...
                        sm.participant(p).session(s).trial(k).support.goal_y_ego,...
                        sm.participant(p).session(s).trial(k).result.y_end);
                    
                    % AVERAGE DISTANCE to EGOCENTRIC PATH (full trajectory including waiting/rotation)
                    [~,distance_to_ego_path] = dsearchn([xi_eg, yi_eg],[x,y]); % returns euclidian distance to nearest neighbour on interpolated ideal path
                    distance_to_ego_path = [distance_to_ego_path; sm.participant(p).session(s).trial(k).result.final_distance_ego]; % add final distance as last data point
                    sm.participant(p).session(s).trial(k).result.avg_distance_ego_path=mean(distance_to_ego_path);
                    sm.participant(p).session(s).trial(k).result.total_distance_ego_path=sum(distance_to_ego_path);
                    
                    % AVERAGE DISTANCE to EGOCENTRIC PATH (only path, remove duplicates due to waiting/rotation)
                    [~,distance_to_ego_path_unique] = dsearchn([xi_eg, yi_eg],xy_unique); % returns euclidian distance to nearest neighbour on interpolated ideal path
                    distance_to_ego_path_unique = [distance_to_ego_path_unique; sm.participant(p).session(s).trial(k).result.final_distance_ego]; % add final distance as last data point
                    sm.participant(p).session(s).trial(k).result.avg_distance_ego_path_pure=mean(distance_to_ego_path_unique);
                    sm.participant(p).session(s).trial(k).result.total_distance_ego_path_pure=sum(distance_to_ego_path_unique);
                    
%                     % AVERAGE DISTANCE to EGOCENTRIC target
%                     sm.participant(p).session(s).trial(k).result.avg_distance_ego_target=total_dist_to_goal_ego/sdata_length(1);
%                     sm.participant(p).session(s).trial(k).result.total_distance_ego_target=total_dist_to_goal_ego;
%                     % sum data points: same as in analyis above
                    
%                     % Cumulative IDEAL DISTANCE to EGOCENTRIC target
%                     id_total_dist_to_goal_ego=0; % start-initiation
%                     xi_eg_length=length(xi_eg)-1;
%                     for i=1:xi_eg_length
%                         % ideal cumulative distance to egocentric target
%                         id_total_dist_to_goal_ego=id_total_dist_to_goal_ego+sm_distance(xi_eg(i),sm.participant(p).session(s).trial(k).support.goal_x_ego,yi_eg(i),sm.participant(p).session(s).trial(k).support.goal_y_ego);
%                     end
                    
%                     % IDEAL AVERAGE DISTANCE to EGOCENTRIC target
%                     sm.participant(p).session(s).trial(k).support.ideal_avg_distance_ego_target=id_total_dist_to_goal_ego/xi_eg_length;
%                     sm.participant(p).session(s).trial(k).support.ideal_total_distance_ego_target=id_total_dist_to_goal_ego;
%                     sm.participant(p).session(s).trial(k).support.ideal_sum_data_points_ego=xi_eg_length;
                    
                    % PATH ACCURACY to EGOCENTRIC target
                    sm.participant(p).session(s).trial(k).result.path_accuracy_ego=sm_ac(...
                        sm.participant(p).session(s).trial(k).result.path_length, ...
                        sm.participant(p).session(s).trial(k).support.ideal_path_length_ego);
                    
%                     % DISTANCE ACCURACY to EGOCENTRIC target
%                     sm.participant(p).session(s).trial(k).result.distance_accuracy_ego=sm_ac(...
%                         sm.participant(p).session(s).trial(k).result.avg_distance_ego_target, ...
%                         sm.participant(p).session(s).trial(k).support.ideal_avg_distance_ego_target);
                else
                    % set dummy values
                    sm.participant(p).session(s).trial(k).result.final_distance_ego=999;
                    sm.participant(p).session(s).trial(k).result.avg_distance_ego_path=999;
                    sm.participant(p).session(s).trial(k).result.total_distance_ego_path=999;
                    sm.participant(p).session(s).trial(k).result.avg_distance_ego_path_pure=999;
                    sm.participant(p).session(s).trial(k).result.total_distance_ego_path_pure=999;
                    sm.participant(p).session(s).trial(k).result.avg_distance_ego_target=999;
                    sm.participant(p).session(s).trial(k).result.total_distance_ego_target=999;
                    sm.participant(p).session(s).trial(k).result.path_accuracy_ego=999;
                    sm.participant(p).session(s).trial(k).result.distance_accuracy_ego=999;
                    
                    sm.participant(p).session(s).trial(k).support.ideal_path_length_ego=999;
                    sm.participant(p).session(s).trial(k).support.ideal_avg_distance_ego_target=999;
                    sm.participant(p).session(s).trial(k).support.ideal_total_distance_ego_target=999;
                    sm.participant(p).session(s).trial(k).support.ideal_sum_data_points_ego=999;
                end
                
                fprintf('Additional: Distance in relation to egocentric target done for %d, session %d, file no %d.\n', i_participant, s, k);
                      
                 %% Body rotation analysis
%                 % Body rotation analysis
%                 sm.participant(p).session(s).trial(k).result.sum_body_rotation=0; br=zeros(1,data_length);
%                 for i=2:(data_length-2)
%                     br(i)=sm_b_rot(y(i-1),x(i-1),y(i),x(i));
%                 end
%                 sm.participant(p).session(s).trial(k).result.sum_body_rotation=sum(br);
%                 
%                 % Ideal sum of body roatations
%                 l_xi_al=length(xi_al);
%                 sm.participant(p).session(s).trial(k).support.ideal_sum_body_rotation=0; br_i=zeros(1,l_xi_al);
%                 for i=2:(l_xi_al-2)
%                     br_i(i)=sm_b_rot(yi_al(i-1),xi_al(i-1),yi_al(i),xi_al(i));
%                 end
%                 sm.participant(p).session(s).trial(k).support.ideal_sum_body_rotation=sum(br_i);
%                 
%                 % Body-rotation-accuracy
%                 sm.participant(p).session(s).trial(k).result.body_rotation_accuracy=sm_ac(sm.participant(p).session(s).trial(k).result.sum_body_rotation,sm.participant(p).session(s).trial(k).support.ideal_sum_body_rotation);
%                 
%                 fprintf('Body rotation analysis done for %d, session %d, file no %d.\n', i_participant, s, k);
                
                 %% Body turn analysis
%                 % Cumulative body turns
%                 body_turn=zeros(1,data_length);
%                 sm.participant(p).session(s).trial(k).result.body_turn_left=0;sm.participant(p).session(s).trial(k).result.body_turn_right=0;body_walk_straight=0;
%                 for j=2:(length(br)-1)
%                     body_turn(j)=heaviside((br(j+1)-br(j)));
%                     if body_turn(j)==1 && (body_turn(j-1)==0 || body_turn(j-1)==0.5)
%                         sm.participant(p).session(s).trial(k).result.body_turn_right=sm.participant(p).session(s).trial(k).result.body_turn_right+1;
%                     elseif body_turn(j)==0 &&(body_turn(j-1)==1 || body_turn(j-1)==0.5)
%                         sm.participant(p).session(s).trial(k).result.body_turn_left=sm.participant(p).session(s).trial(k).result.body_turn_left+1;
%                     else
%                         body_walk_straight=body_walk_straight+1;
%                     end
%                 end
%                 sm.participant(p).session(s).trial(k).result.no_body_turn=sm.participant(p).session(s).trial(k).result.body_turn_right+sm.participant(p).session(s).trial(k).result.body_turn_left;
%                 
%                 % Cumulative ideal body turns
%                 body_turn_i=zeros(1,l_xi_al);
%                 sm.participant(p).session(s).trial(k).support.ideal_body_turn_left=0;sm.participant(p).session(s).trial(k).support.ideal_body_turn_right=0;ideal_body_walk_straight=0;
%                 for j=2:(length(br_i)-1)
%                     body_turn_i(j)=heaviside((br_i(j+1)-br_i(j)));
%                     if body_turn_i(j)==1 && (body_turn_i(j-1)==0 || body_turn_i(j-1)==0.5)
%                         sm.participant(p).session(s).trial(k).support.ideal_body_turn_right=sm.participant(p).session(s).trial(k).support.ideal_body_turn_right+1;
%                     elseif body_turn_i(j)==0 &&(body_turn_i(j-1)==1 || body_turn_i(j-1)==0.5)
%                         sm.participant(p).session(s).trial(k).support.ideal_body_turn_left=sm.participant(p).session(s).trial(k).support.ideal_body_turn_left+1;
%                     else
%                         ideal_body_walk_straight=ideal_body_walk_straight+1;
%                     end
%                 end
%                 sm.participant(p).session(s).trial(k).support.ideal_no_body_turn=sm.participant(p).session(s).trial(k).support.ideal_body_turn_right+sm.participant(p).session(s).trial(k).support.ideal_body_turn_left;
%                 
%                 % Body-turn-accuracy
%                 sm.participant(p).session(s).trial(k).result.body_turn_accuracy=sm_ac(sm.participant(p).session(s).trial(k).result.no_body_turn,sm.participant(p).session(s).trial(k).support.ideal_no_body_turn);
%                 
%                 fprintf('Body turn analysis done for %d, session %d, file no %d.\n', i_participant, s, k);
%                 
                 %% Head rotation analysis
%                 % Head rotation information from column 4 of M stored as r
%                 
%                 ro=length(r); sm.participant(p).session(s).trial(k).result.final_deviation=0; % final deviation from start to target angle
%                 sm.participant(p).session(s).trial(k).result.sum_head_rotation=0; % cumulative rotation, sum of head rotations
%                 for j=1:(ro-1)
%                     sm.participant(p).session(s).trial(k).result.final_deviation=sm.participant(p).session(s).trial(k).result.final_deviation+(r(j+1)-r(j)); % deviation
%                     sm.participant(p).session(s).trial(k).result.sum_head_rotation=sm.participant(p).session(s).trial(k).result.sum_head_rotation+abs(r(j+1)-r(j)); % sum of head roations
%                 end
%                 sm.participant(p).session(s).trial(k).result.no_full_head_rotation=sm.participant(p).session(s).trial(k).result.sum_head_rotation/360;
%                 
%                 fprintf('Head rotation analysis done for %d, session %d, file no %d.\n', i_participant, s, k);
                
                %% Head turn analysis
%                 % Cumulative amount of completed head-turns
%                 head_turn=zeros(1, ro);
%                 sm.participant(p).session(s).trial(k).result.head_turn_left=0;sm.participant(p).session(s).trial(k).result.head_turn_right=0;head_walk_straight=0;head_left=0; head_right=0;
%                 for j=2:(ro-1)
%                     head_turn(j)=heaviside((r(j+1)-r(j)));
%                     if head_turn(j)==1 && (head_turn(j-1)==0 || head_turn(j-1)==0.5)
%                         sm.participant(p).session(s).trial(k).result.head_turn_right=sm.participant(p).session(s).trial(k).result.head_turn_right+1;
%                     elseif head_turn(j)==0 && (head_turn(j-1)==1 || head_turn(j-1)==0.5)
%                         sm.participant(p).session(s).trial(k).result.head_turn_left=sm.participant(p).session(s).trial(k).result.head_turn_left+1;
%                     elseif (head_turn(j)==0 && head_turn(j-1)==0)
%                         head_straight=head_walk_straight+1;
%                     elseif head_turn(j)==1 && head_turn(j-1)==1
%                         head_right=head_right+1;
%                     elseif head_turn(j)==0.5 && head_turn(j-1)==0.5
%                         head_left=head_left+1;
%                     end
%                 end
%                 
%                 % Head-turn total
%                 sm.participant(p).session(s).trial(k).result.no_head_turn=sm.participant(p).session(s).trial(k).result.head_turn_right+sm.participant(p).session(s).trial(k).result.head_turn_left;
%                 
%                 % Head-turn-accuracy
%                 sm.participant(p).session(s).trial(k).result.head_turn_accuracy=sm_ac(sm.participant(p).session(s).trial(k).result.no_head_turn,sm.participant(p).session(s).trial(k).support.ideal_headturnNo);
%                 
%                 fprintf('Head turn analysis done for %d, session %d, file no %d.\n', i_participant, s, k);
                
                %% Zone analysis
                [sm.participant(p).session(s).trial(k).result.zone.alley_zone,...
                    sm.participant(p).session(s).trial(k).result.zone.rel_alley_zone,...
                    sm.participant(p).session(s).trial(k).result.zone.alley_entry,...
                    sm.participant(p).session(s).trial(k).result.zone.alley_rotations]=sm_wp10_coordinatesZonesStatic(x,...
                    y,r,sm.coord.alley_full_x,sm.coord.alley_full_y);
                
                [alley_entry_mat]=sm_wp10_coordinatesZonesDynamic(x,...
                    y,sm.coord.alley_full_x,sm.coord.alley_full_y);
                
                [sm.participant(p).session(s).trial(k).result.zone.alley_zone_out,...
                    sm.participant(p).session(s).trial(k).result.zone.rel_alley_zone_out,...
                    sm.participant(p).session(s).trial(k).result.zone.alley_entry_out,...
                    sm.participant(p).session(s).trial(k).result.zone.alley_rotations_out]=sm_wp10_coordinatesZonesStatic(x,...
                    y,r,sm.coord.alley_half_out_x,sm.coord.alley_half_out_y);
                
                [sm.participant(p).session(s).trial(k).result.zone.alley_zone_in,...
                    sm.participant(p).session(s).trial(k).result.zone.rel_alley_zone_in,...
                    sm.participant(p).session(s).trial(k).result.zone.alley_entry_in,...
                    sm.participant(p).session(s).trial(k).result.zone.alley_rotations_in]=sm_wp10_coordinatesZonesStatic(x,...
                    y,r,sm.coord.alley_half_in_x,sm.coord.alley_half_in_y);
                
                [sm.participant(p).session(s).trial(k).result.zone.pentagon_zone,...
                    sm.participant(p).session(s).trial(k).result.zone.rel_pentagon_zone,...
                    sm.participant(p).session(s).trial(k).result.zone.pentagon_entry,...
                    sm.participant(p).session(s).trial(k).result.zone.pentagon_rotations]=sm_wp10_coordinatesPentagon(x,...
                    y,r,sm.coord.central_x,sm.coord.central_y);
                
                [sm.participant(p).session(s).trial(k).result.zone.triangle_zone,....
                    sm.participant(p).session(s).trial(k).result.zone.rel_triangle_zone,...
                    sm.participant(p).session(s).trial(k).result.zone.triangle_entry,...
                    sm.participant(p).session(s).trial(k).result.zone.triangle_rotations]=sm_wp10_coordinatesZonesStatic(x,...
                    y,r,sm.coord.tri_x,sm.coord.tri_y);
                
                [sm.participant(p).session(s).trial(k).result.zone.rectangle_zone,...
                    sm.participant(p).session(s).trial(k).result.zone.rel_rectangle_zone,...
                    sm.participant(p).session(s).trial(k).result.zone.rectangle_entry,...
                    sm.participant(p).session(s).trial(k).result.zone.rectangle_rotations]=sm_wp10_coordinatesZonesStatic(x,...
                    y,r,sm.coord.rec_x,sm.coord.rec_y);
                
                [rectangle_entry_mat]=sm_wp10_coordinatesZonesDynamic(x,...
                    y,sm.coord.rec_x,sm.coord.rec_y);
                
                [sm.participant(p).session(s).trial(k).result.zone.alley_time, ...
                    sm.participant(p).session(s).trial(k).result.zone.pentagon_time, ...
                    sm.participant(p).session(s).trial(k).result.zone.triangle_time, ...
                    sm.participant(p).session(s).trial(k).result.zone.rectangle_time]=sm_wp10_timeInZone(...
                    sm.participant(p).session(s).trial(k).result.time, ...
                    sm.participant(p).session(s).trial(k).result.zone.rel_alley_zone, ...
                    sm.participant(p).session(s).trial(k).result.zone.rel_pentagon_zone, ...
                    sm.participant(p).session(s).trial(k).result.zone.rel_triangle_zone, ...
                    sm.participant(p).session(s).trial(k).result.zone.rel_rectangle_zone);
                
                fprintf('Zone analysis done for %d, session %d, file no %d.\n', i_participant, s, k);
                
                %% Exploration-Analysis
                % Arm score and Path score as indicators of alley-exploration
                sm.participant(p).session(s).trial(k).result.arm_explored=sm_wp10_armExplored(...
                    sm.participant(p).session(s).trial(k).result.zone.alley_zone);
                sm.participant(p).session(s).trial(k).result.arm_score=sm_wp10_armScore(...
                    sm.participant(p).session(s).trial(k).result.zone.alley_entry);
                sm.participant(p).session(s).trial(k).result.path_explored=sm_wp10_pathExplored(...
                    sm.participant(p).session(s).trial(k).result.zone.alley_zone_out,...
                    sm.participant(p).session(s).trial(k).result.zone.alley_zone_in,...
                    sm.participant(p).session(s).trial(k).result.zone.rectangle_zone,...
                    sm.participant(p).session(s).trial(k).result.zone.triangle_zone);
                sm.participant(p).session(s).trial(k).result.path_score=sm_wp10_pathScore(...
                    sm.participant(p).session(s).trial(k).result.zone.alley_entry_out,...
                    sm.participant(p).session(s).trial(k).result.zone.alley_entry_in,...
                    sm.participant(p).session(s).trial(k).result.zone.rectangle_entry,...
                    sm.participant(p).session(s).trial(k).result.zone.triangle_entry);
                
                % Success
                success_criterium = 0.1; % cut-off proximity to to goal; change value if necessary
                [sm.participant(p).session(s).trial(k).result.success, ...
                    sm.participant(p).session(s).trial(k).result.success_ego,...
                    sm.participant(p).session(s).trial(k).result.correct_final_alley,...
                    sm.participant(p).session(s).trial(k).result.correct_final_alley_ego]=sm_wp10_success(...
                    success_criterium,...
                    sm.participant(p).session(s).trial(k).result.final_distance,...
                    sm.participant(p).session(s).trial(k).result.final_distance_ego,...
                    sm.participant(p).session(s).trial(k).goal_alley,...
                    sm.participant(p).session(s).trial(k).result.chosen_alley_int,...
                    sm.participant(p).session(s).trial(k).support.ego_alley);
                
%                 % Direct path to target
%                 [ideal_no, match_abs, ...
%                     sm.participant(p).session(s).trial(k).result.direct_path_percent, ...
%                     sm.participant(p).session(s).trial(k).result.direct_path]=sm_wp10_directPathCalc(...
%                     uniq_alley, uniq_rect, alley_entry_mat, rectangle_entry_mat);
                
%                 % Search strategies
%                 [sm.participant(p).session(s).trial(k).result.search_strategy_direct,...
%                     sm.participant(p).session(s).trial(k).result.search_strategy_detour,...
%                     sm.participant(p).session(s).trial(k).result.search_strategy_reoriented,...
%                     sm.participant(p).session(s).trial(k).result.search_strategy_no]=sm_wp10_searchStrategy(...
%                     sm.participant(p).session(s).trial(k).result.direct_path,...
%                     sm.participant(p).session(s).trial(k).result.path_explored,...
%                     sm.participant(p).session(s).trial(k).result.path_score);
                
                fprintf('Exploration analysis done for %d, session %d, file no %d.\n', i_participant, s, k);
                
                %% Marker for excluding trials
                % Criteria: timeout, or no movement/very short trial time (i.e. path_length=0, body_rot_abs=0, or time < 3)
                sm.participant(p).session(s).trial(k).exclude_trial_matlab=0;
                if sm.participant(p).session(s).trial(k).result.chosen_alley_int==999
                    sm.participant(p).session(s).trial(k).exclude_trial_matlab=1;
                    fprintf('Trial %d marked for exclusion due to timeout.\n',k);
                elseif sm.participant(p).session(s).trial(k).condition ~=4 % not for motor control task
                    if (sm.participant(p).session(s).trial(k).result.path_length<=0.1 ...
                            || sm.participant(p).session(s).trial(k).result.sum_body_rotation==0 ...
                            || sm.participant(p).session(s).trial(k).result.time < 3)
                        sm.participant(p).session(s).trial(k).exclude_trial_matlab=1;
                        fprintf('Trial %d marked for exclusion due lack of movement/short trial time.\n',k);
                    end
                end
            end
            
%             % save data % TBD remove
%             save(fullfile(output_folder, file_name_Subject),'sm', '-append');
            
            %% Create plots
            if s==3
                sm_wp10_plotMotorControl(sm.participant(p).session(s).trial(k).trial_num,...
                    sm.participant(p).session(s).session_number,...
                    sm.participant(p).id,sm.participant(p).group_s,...
                    sm.coord.practise.practise_poly, sm.coord.practise.goal_x, sm.coord.practise.goal_y, ...
                    sm.coord.practise.start_x, sm.coord.practise.start_y,...
                    sm.coord.practise.practise_goal_names, x, y ,xi_al, yi_al, output_folder)
            else
                sm_wp10_plotTrack(sm.participant(p).session(s).trial(k).trial_num, ...
                    sm.participant(p).session(s).session_number, ...
                    sm.participant(p).session(s).trial(k).condition, ...
                    sm.participant(p).session(s).trial(k).start_i, ...
                    sm.participant(p).id,sm.participant(p).group_s, ...
                    sm.participant(p).session(s).trial(k).result.success, ...
                    sm.participant(p).session(s).trial(k).result.direct_path, ...
                    sm.participant(p).session(s).trial(k).result.search_strategy_no, ...
                    sm.coord.alley_poly_out, sm.coord.alley_poly_in, sm.coord.tri_poly, sm.coord.rec_poly, ...
                    x, y, x_line_ego, y_line_ego, x_line, y_line, ...
                    sm.participant(p).session(s).trial(k).goal_x, ...
                    sm.participant(p).session(s).trial(k).goal_y,...
                    output_folder)
            end
        end
        
    end
    
    % update index
    p=p+1;
    
end

%% Save data
file_path         = [result_folder, file_name];
save(file_path, 'sm', '-append')

clear