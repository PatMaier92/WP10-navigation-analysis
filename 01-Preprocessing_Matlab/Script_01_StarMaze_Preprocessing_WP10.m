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

% Block 1: Set up input/output folders, Starmaze and Practise environment
% Block 2: Data preparation 
% Block 3: Data analysis

%% Block 1: Set up input and output folders, starmaze and practise environment 
%% data input folder and participant information
[data_folder]  = setInputPath(); % provide folder with all raw data
[participant_start,participant_end] = setParticipants(); % provide participant range
n_sessions      = 3; % default 3

%% result folder
result_folder=[data_folder '\WP10_results'];
if ~exist(result_folder, 'dir')
    mkdir(result_folder);
    disp('Your output folder did not exist, a new folder was created.')
end

%% load data table or create new one 
% load existing data
file_name         = '\wp10_results_table.mat';
file_path         = fullfile(result_folder, file_name);
if isfile(file_path)
    load(file_path)
end

% initialize if non-existing
if ~exist('sm','var')
    sm = []; 

    %% set up Starmaze environment 
    % coordinates of min/max values
    values=table2array(readtable('wp10_values.csv'));
    [sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax]=setMinMaxValues(values);

    % coordinates of start positions (normalized!)
    start=table2array(readtable('wp10_start.csv'));
    [sm.coord.start_x,sm.coord.start_y]=setStartValues(start,sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax);

    % coordinates of goal positions (normalized!)
    goal=table2array(readtable('wp10_goal.csv'));
    [sm.coord.goal_x,sm.coord.goal_y]=setGoalValues(goal,sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax);

    % coordinates of alley corners (normalized!)
    alley_x=table2array(readtable('wp10_alley_x.csv'));
    [n_corners,n_alleys] = size(alley_x);
    for i_alley=1:n_alleys
        for i_corner=1:n_corners
            alley_x(i_corner,i_alley)=setNormalizedValues(alley_x(i_corner,i_alley),sm.coord.xmin,sm.coord.xmax);
        end
    end
    alley_y=table2array(readtable('wp10_alley_y.csv'));
    for i_alley=1:n_alleys
        for i_corner=1:n_corners
            alley_y(i_corner,i_alley)=setNormalizedValues(alley_y(i_corner,i_alley),sm.coord.ymin,sm.coord.ymax);
        end
    end

    % coordinates of combined pentagon (normalized!) and central polyshape
    pentagon_x=table2array(readtable('wp10_pentagon_x.csv'));
    pentagon_y=table2array(readtable('wp10_pentagon_y.csv'));
    [sm.coord.central_x,sm.coord.central_y,sm.coord.central_poly,pentagon_x,pentagon_y]=setPentagonValues(alley_x,alley_y,pentagon_x,pentagon_y,...
        sm.coord.xmin,sm.coord.xmax,sm.coord.ymin,sm.coord.ymax);

    % create other polyshapes
    % alley polyshape
    [sm.coord.alley_full_x,sm.coord.alley_full_y,sm.coord.alley_poly,...
        sm.coord.alley_half_out_x,sm.coord.alley_half_out_y,sm.coord.alley_poly_out,...
        sm.coord.alley_half_in_x,sm.coord.alley_half_in_y,sm.coord.alley_poly_in]=setAlleyPolyshape(alley_x,alley_y);
    % rectangle polyshape
    [sm.coord.rec_x,sm.coord.rec_y,sm.coord.rec_poly]=setRectPolyshape(n_alleys,alley_x,alley_y,pentagon_x,pentagon_y);
    % triangle polyshape
    [sm.coord.tri_x,sm.coord.tri_y,sm.coord.tri_poly]=setTriPolyshape(n_alleys,alley_x,alley_y,pentagon_x,pentagon_y);
    % joint polyshape
    sm.coord.full_poly=[sm.coord.alley_poly_out{1,1} sm.coord.alley_poly_in{1,1}...
        sm.coord.alley_poly_out{1,2} sm.coord.alley_poly_in{1,2}...
        sm.coord.alley_poly_out{1,3} sm.coord.alley_poly_in{1,3}...
        sm.coord.alley_poly_out{1,4} sm.coord.alley_poly_in{1,4}...
        sm.coord.alley_poly_out{1,5} sm.coord.alley_poly_in{1,5} sm.coord.central_poly];

    % create graph
    % for automated shortest path calculation (requires Matlab 2021a)
    [sm.coord.graph,sm.coord.graph_x,sm.coord.graph_y]=setGraph(sm.coord.start_x,sm.coord.start_y,...
        sm.coord.tri_x,sm.coord.tri_y,sm.coord.goal_x,sm.coord.goal_y);

    % add (ordered) information
    sm.coord.goal_names=["MA", "MC", "MI"];
    sm.coord.start_names=["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "X"];
    sm.coord.alley_names=["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"];

    % create test figure plot
    % plotTestfigure("s_maze",sm.coord.full_poly,sm.coord.goal_x,sm.coord.goal_y,sm.coord.start_x,sm.coord.start_y,sm.coord.goal_names,sm.coord.graph,sm.coord.graph_x,sm.coord.graph_y);

    clear alley_x alley_y pentagon_x pentagon_y i_alley n_alleys i_corner n_corners start goal values; 
    
    %% set up Practise environment (for motor control task)
    % coordinates of min/max values
    practise_values=table2array(readtable('wp10_practise_values.csv'));
    [sm.coord.practise.xmin,sm.coord.practise.xmax,sm.coord.practise.ymin,sm.coord.practise.ymax]=setMinMaxValues(practise_values);

    % coordinates of start position (normalized!)
    practise_start=table2array(readtable('wp10_practise_start.csv'));
    [sm.coord.practise.start_x,sm.coord.practise.start_y]=setStartValues(practise_start,sm.coord.practise.xmin,sm.coord.practise.xmax,sm.coord.practise.ymin,sm.coord.practise.ymax);

    % coordinates of goal positions (normalized!)
    practise_goal=table2array(readtable('wp10_practise_goal.csv'));
    [sm.coord.practise.goal_x,sm.coord.practise.goal_y]=setGoalValues(practise_goal,sm.coord.practise.xmin,sm.coord.practise.xmax,sm.coord.practise.ymin,sm.coord.practise.ymax);

    % coordinates of alley corners (normalized!)
    practise_alley_x=table2array(readtable('wp10_practise_x.csv'));
    [n_corners,~] = size(practise_alley_x);
    for i_corner=1:n_corners
        practise_alley_x(i_corner,1)=setNormalizedValues(practise_alley_x(i_corner,1),sm.coord.practise.xmin,sm.coord.practise.xmax);
    end
    practise_alley_y=table2array(readtable('wp10_practise_y.csv'));
    for i_corner=1:n_corners
        practise_alley_y(i_corner,1)=setNormalizedValues(practise_alley_y(i_corner,1),sm.coord.practise.ymin,sm.coord.practise.ymax);
    end

    % create polyshape
    sm.coord.practise.practise_poly=polyshape(practise_alley_x(:),practise_alley_y(:));

    % add (ordered) information
    sm.coord.practise.practise_goal_names=["1", "2", "3", "4", "5", "6", "7", "8" , "9", "10" ];
    sm.coord.practise.practise_start_names="Player_P0";

    % create test figure plot
    % plotTestfigure("p_maze",sm.coord.practise.practise_poly,sm.coord.practise.goal_x,sm.coord.practise.goal_y,sm.coord.practise.start_x,sm.coord.practise.start_y,sm.coord.practise.practise_goal_names,sm.coord.graph,sm.coord.graph_x,sm.coord.graph_y);

    clear practise_alley_x practise_alley_y i_corner n_corners practise_start practise_goal practise_values; 
    
    %% save sm data 
    save(file_path, 'sm');
    
    %% initialize participant index
    p=1; 
    
else
    %% get participant index %% TBD CHECK THIS (and make so recognizes participant) %%
    [~,n]=size(sm);
    p=n+1;
    clear n; 
    
end

%% Block 2: Data preprocessing
for id=participant_start:participant_end
    
    for s=1:n_sessions        
        %% individual input and output folder
        % input folder
        input_folder=[data_folder '\' num2str(id) '\S00' num2str(s)]; 
        if ~exist(input_folder, 'dir')
            fprintf('Folder for participant %d, session %d does not exist. Continue with next iteration.\n', id, session);
            continue;
        end
        
        % output folder (only for trial plots)
        output_folder=[data_folder '\' num2str(id) '\plots'];
        if ~exist(output_folder, 'dir')
            mkdir(output_folder);
            fprintf('Your output folder for participant %d did not exist, a new folder was created.\n', id);
        end
        
        %% read-in trial file
        opts=detectImportOptions([input_folder, '\trial_results.csv']);
        opts=setvaropts(opts,'timestamp','InputFormat','MM/dd/uuuu hh:mm:ss aa');
        trial_data=readtable([input_folder, '\trial_results.csv'], opts); clear opts; 
        
        % add participant information to sm
        sm.participant(p).id=id;
        [sm.participant(p).group,sm.participant(p).group_s, ...
            sm.participant(p).sex,sm.participant(p).sex_s]=setGroupSexInfo(sm.participant(p).id);
        sm.participant(p).session(s).session_number=trial_data.session_num(1,1);
        
        %% read-in log file %% TBD: only read-in once in session 1 (once checked for correctness)
        opts=detectImportOptions([input_folder, '\log.csv']);
        opts.SelectedVariableNames = {'message'};
        log_data=table2cell(readtable([input_folder, '\log.csv'], opts)); % read in log-file-info
        log_data=log_data(contains(log_data,'ID is'));
        [sm.participant(p).session(s).rand_dict]=setRandomizationDict(log_data, id);
        clear log_data opts; 
        
        %% read-in individual tracker trial files
        d=dir(fullfile(input_folder, '*.xlsx')); % every .xlsx is detected --> change if different file-format
        
        % first time: clean and convert csv to xlsx files %% TBD: do not
        % save this but do it as part of data load 
        if isempty(d)
            % preprocess individual tracker trial csv files
            convertTrackerToXLSX(input_folder,id,sm.participant(p).group_s,s);
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
            
            % get sampling rate
            sampling_rate_original=zeros(length(data.time)-1,1);
            for i=1:length(data.time)-1
                sampling_rate_original(i)=data.time(i+1)-data.time(i);
            end
            sm.participant(p).session(s).trial(k).support.sampling_rate=sum(sampling_rate_original)/length(sampling_rate_original);
            clear sampling_rate_original i; 
            
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
            r=data.rot_y; % yaw rotations
            
            % spatial normalization
            if s==3 % practise maze
                x=setNormalizedValues(x,sm.coord.practise.xmin,sm.coord.practise.xmax); y=setNormalizedValues(y,sm.coord.practise.ymin,sm.coord.practise.ymax);
            else % star maze
                x=setNormalizedValues(x,sm.coord.xmin,sm.coord.xmax); y=setNormalizedValues(y,sm.coord.ymin,sm.coord.ymax);
            end
            
            % save start and end points 
            sm.participant(p).session(s).trial(k).x_1=x(1); sm.participant(p).session(s).trial(k).y_1=y(1);
            sm.participant(p).session(s).trial(k).x_n=x(end); sm.participant(p).session(s).trial(k).y_n=y(end);
            
            % unique x-/y values, excluding periods without movement
            xy_unique = unique([x y],'rows','stable'); % excluding row duplicates
            x_unique = xy_unique(:,1);
            y_unique = xy_unique(:,2); 
            clear xy_unique; 
                       
            %% get single trial info from trial_results
            sm.participant(p).session(s).session_duration=round(minutes(trial_data.timestamp(numel(files),1) - trial_data.timestamp(1,1))); 
            
            sm.participant(p).session(s).trial(k).block=trial_data.block_num(k,1);
            sm.participant(p).session(s).trial(k).trial_num=trial_data.trial_num(k,1);
            sm.participant(p).session(s).trial(k).trial_in_block=trial_data.trial_num_in_block(k,1);
            
            sm.participant(p).session(s).trial(k).feedback_s=string(trial_data.trial_feedback(k,1));
            sm.participant(p).session(s).trial(k).feedback=int8(strcmp(sm.participant(p).session(s).trial(k).feedback_s,'true'));
            
            sm.participant(p).session(s).trial(k).condition_s=string(trial_data.trial_type(k,1));
            sm.participant(p).session(s).trial(k).condition=setTrialCondition(sm.participant(p).session(s).trial(k).condition_s,sm.participant(p).session(s).trial(k).feedback);
            
            n_goals=4;
            sm.participant(p).session(s).trial(k).goal_identity_s=string(trial_data.trial_goal_identity(k,1));
            sm.participant(p).session(s).trial(k).goal_identity=setTrialGoalIdentity(n_goals, char(trial_data.trial_goal_identity(k,1)));
            clear n_goals; 
            
            [sm.participant(p).session(s).trial(k).goal_x,sm.participant(p).session(s).trial(k).goal_y,...
                sm.participant(p).session(s).trial(k).goal_i,sm.participant(p).session(s).trial(k).goal_s,...
                sm.participant(p).session(s).trial(k).goal_alley]=setTrialGoalLocation(char(string(trial_data.trial_goal(k,1))),...
                sm.coord.goal_x,sm.coord.goal_y,sm.coord.goal_names,sm.coord.alley_names);
            
            start_name=char(trial_data.trial_player(k,1)); sm.participant(p).session(s).trial(k).start_s=start_name(end); clear start_name; 
            [sm.participant(p).session(s).trial(k).start_i]=setTrialStartLocation(sm.participant(p).session(s).trial(k).start_s,sm.coord.start_names);
            
            %% For all normal navigation trials (i.e., not motor control task)
            if sm.participant(p).session(s).trial(k).condition~=4
                %% compute support variables depending on this trial's settings
                % ideal path coordinates & length, ideal egocentric path coordinates & length
                % Caveat: dummy values for egocentric for inner starts (because no clear ideal egocentric path)
                [sm.participant(p).session(s).trial(k).support.goal_x_ego,...
                    sm.participant(p).session(s).trial(k).support.goal_y_ego,...
                    x_line, y_line, x_line_chosen, y_line_chosen, x_line_ego, y_line_ego,...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length,...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length_chosen,...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length_ego,...
                    sm.participant(p).session(s).trial(k).support.ego_alley]=computeStartDependentIdealVariables(...
                    sm.coord.graph, sm.coord.graph_x, sm.coord.graph_y,...
                    sm.participant(p).session(s).trial(k).start_i,...
                    sm.participant(p).session(s).trial(k).goal_i,...
                    sm.participant(p).session(s).trial(k).x_n,...
                    sm.participant(p).session(s).trial(k).y_n,...
                    sm.coord.alley_full_x, sm.coord.alley_full_y,...
                    sm.coord.rec_x, sm.coord.rec_y, sm.coord.central_poly, ...
                    sm.coord.full_poly);
                
                % interpolate data for further analysis
                % using 'interparc' function by John D'Errico (Matlab File Exchanger)
                [xi_al,yi_al]=interpolateData(x_line, y_line, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length);
                
                [xi_ch,yi_ch]=interpolateData(x_line_chosen, y_line_chosen, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length_chosen);
                
                [xi_eg,yi_eg]=interpolateData(x_line_ego, y_line_ego, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length_ego);
                
%                 % test plot
%                 figure;
%                 plot(sm.coord.full_poly);
%                 hold on
%                 plot(x_line, y_line, 'k+', xi_al, yi_al, 'k-',...
%                 x_line_ego, y_line_ego, 'rx', xi_eg, yi_eg, 'r-',...
%                 x_line_chosen, y_line_chosen, 'bx', xi_ch, yi_ch, 'b-');
%                 xlim([0 1]);
%                 ylim([0 1]);
%                 hold off
               
                % zone analysis for ideal paths
                [~, ~, sm.participant(p).session(s).trial(k).support.zone.ideal_alley_entry, ...
                    ~]=computeStaticZoneValues(xi_al, yi_al,...
                    zeros(length(xi_al),1), sm.coord.alley_full_x, sm.coord.alley_full_y);
                
                [~, ~, sm.participant(p).session(s).trial(k).support.zone.ideal_rectangle_entry, ...
                    ~]=computeStaticZoneValues(xi_al, yi_al,...
                    zeros(length(xi_al),1), sm.coord.rec_x, sm.coord.rec_y);
                
%                 [ideal_alley_entry_mat]=computeDynamicZoneValues(xi_al,...
%                     yi_al, sm.coord.alley_full_x, sm.coord.alley_full_y);
%                 [uniq_alley]=unique(ideal_alley_entry_mat,'rows');
%                 
%                 [ideal_rectangle_entry_mat]=computeDynamicZoneValues(xi_al,...
%                     yi_al, sm.coord.rec_x, sm.coord.rec_y);
%                 [uniq_rect]=unique(ideal_rectangle_entry_mat,'rows');
%                 if mod(sm.participant(p).session(s).trial(k).start_i,2) % even number = outer start
%                     uniq_rect=uniq_rect(2:end,:); % remove first row (start) for outer starts, as it's always zero
%                 end
                
                %% Block 3: Data analysis, i.e. calculcation of variables
                %% time analysis
                % TIME
                sm.participant(p).session(s).trial(k).time=computeTime(t(1),t(end));
                % fprintf('Time analysis done for %d, session %d, file no %d.\n', id, s, k);           
                               
                %% standard coordinate analysis using x-/y-coordinates
                % PATH LENGTH 
                sm.participant(p).session(s).trial(k).path_length=computePathLength(x,y); 
                
                % PATH LENGTH ERROR 
                sm.participant(p).session(s).trial(k).path_length_error=computeDeviationToIdealValue(...
                    sm.participant(p).session(s).trial(k).path_length, ...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length);
                
                % VELOCITY 
                sm.participant(p).session(s).trial(k).velocity=sm.participant(p).session(s).trial(k).path_length / ...
                    sm.participant(p).session(s).trial(k).time;
                
                % FINAL DISTANCE to CORRECT target
                sm.participant(p).session(s).trial(k).final_distance=0;
                if sm.participant(p).session(s).trial(k).feedback==0
                    sm.participant(p).session(s).trial(k).final_distance=computeDistance(...
                        sm.participant(p).session(s).trial(k).goal_x, ...
                        sm.participant(p).session(s).trial(k).x_n, ...
                        sm.participant(p).session(s).trial(k).goal_y, ...
                        sm.participant(p).session(s).trial(k).y_n);
                end
                
                % AVERAGE DISTANCE to PATH
                % with full x-/y-trajectory
                [sm.participant(p).session(s).trial(k).path_distance, ~] = computePathDistance(...
                    xi_al, yi_al, x, y, sm.participant(p).session(s).trial(k).final_distance, true);  
                % with unique x-/y-trajectory (duplicates due to waiting and rotation are removed)              
                [sm.participant(p).session(s).trial(k).adj_path_distance, ~] = computePathDistance(...
                    xi_al, yi_al, x_unique, y_unique, sm.participant(p).session(s).trial(k).final_distance, true);        
                
                % AVERAGE DISTANCE to TARGET 
                % target distance 
                [sm.participant(p).session(s).trial(k).target_distance, ~]=computeTargetDistance(x,y,...
                    sm.participant(p).session(s).trial(k).goal_x,sm.participant(p).session(s).trial(k).goal_y); 
                % ideal target distance 
                [ideal_distance_target, ~]=computeTargetDistance(xi_al,yi_al,...
                    sm.participant(p).session(s).trial(k).goal_x,sm.participant(p).session(s).trial(k).goal_y); 
                % target distance error 
                sm.participant(p).session(s).trial(k).target_distance_error=computeDeviationToIdealValue(...
                    sm.participant(p).session(s).trial(k).target_distance, ideal_distance_target);
                clear ideal_distance_target; 
                
                % fprintf('Standard path and distance analysis done for %d, session %d, file no %d.\n', id, s, k);
                
                %% additional distance analysis for probe trials  
                if sm.participant(p).session(s).trial(k).feedback==0   
                    % AVERAGE DISTANCE to PATH to CHOSEN target 
                    % with full x-/y-trajectory
                    [sm.participant(p).session(s).trial(k).chosen_path_distance, ~] = computePathDistance(...
                        xi_ch, yi_ch, x, y, 0, false); 
                    % with unique x-/y-trajectory (duplicates due to waiting/rotation are removed)              
                    [sm.participant(p).session(s).trial(k).adj_chosen_path_distance, ~] = computePathDistance(...
                        xi_ch, yi_ch, x_unique, y_unique, 0, false);     
                    
                    % AVERAGE DISTANCE to CHOSEN TARGET
                    % target distance 
                    [sm.participant(p).session(s).trial(k).chosen_target_distance, ~]=computeTargetDistance(x,y,x(end),y(end)); 
                    % ideal target distance 
                    [ideal_chosen_target_distance, ~]=computeTargetDistance(xi_ch,yi_ch,x(end),y(end)); 
                    % target distance error 
                    sm.participant(p).session(s).trial(k).chosen_target_distance_error=computeDeviationToIdealValue(...
                        sm.participant(p).session(s).trial(k).chosen_target_distance, ideal_chosen_target_distance);  
                    clear ideal_chosen_target_distance; 
                else
                    % dummy values
                    sm.participant(p).session(s).trial(k).chosen_path_distance=999;
                    sm.participant(p).session(s).trial(k).adj_chosen_path_distance=999;
                    sm.participant(p).session(s).trial(k).chosen_target_distance=999;
                    sm.participant(p).session(s).trial(k).chosen_target_distance_error=999;
                end
                
                % fprintf('Additional: Distance analysis to chosen target done for %d, session %d, file no %d.\n', id, s, k);
          
                %% additional distance analysis for allocentric probe trials with potential egocentric response
                % excludes allocentric trials with inner starts (even integer) as there
                % is no clear egocentric path/goal location from these starts 
                if sm.participant(p).session(s).trial(k).condition==1 && mod(sm.participant(p).session(s).trial(k).start_i,2)
                    % FINAL DISTANCE to EGOCENTRIC target
                    sm.participant(p).session(s).trial(k).final_distance_ego=computeDistance(...
                        sm.participant(p).session(s).trial(k).support.goal_x_ego,...
                        sm.participant(p).session(s).trial(k).x_n,...
                        sm.participant(p).session(s).trial(k).support.goal_y_ego,...
                        sm.participant(p).session(s).trial(k).y_n);
                    
                    % AVERAGE DISTANCE to EGOCENTRIC PATH
                    % with full x-/y-trajectory
                    [sm.participant(p).session(s).trial(k).ego_path_distance, ~] = computePathDistance(...
                        xi_eg, yi_eg, x, y, sm.participant(p).session(s).trial(k).final_distance_ego, true); 

                    % with unique x-/y-trajectory (duplicates due to waiting/rotation are removed)              
                    [sm.participant(p).session(s).trial(k).adj_ego_path_distance, ~] = computePathDistance(...
                        xi_eg, yi_eg, x_unique, y_unique, sm.participant(p).session(s).trial(k).final_distance_ego, true);        
 
                    % AVERAGE DISTANCE to EGOCENTRIC TARGET
                    % target distance 
                    [sm.participant(p).session(s).trial(k).ego_target_distance, ~]=computeTargetDistance(x,y,...
                        sm.participant(p).session(s).trial(k).support.goal_x_ego,...
                        sm.participant(p).session(s).trial(k).support.goal_y_ego); 
                    % ideal target distance 
                    [ideal_ego_target_distance, ~]=computeTargetDistance(xi_eg,yi_eg,...
                        sm.participant(p).session(s).trial(k).support.goal_x_ego,...
                        sm.participant(p).session(s).trial(k).support.goal_y_ego); 
                    % target distance error 
                    sm.participant(p).session(s).trial(k).ego_target_distance_error=computeDeviationToIdealValue(...
                        sm.participant(p).session(s).trial(k).ego_target_distance, ideal_ego_target_distance); 
                    clear ideal_ego_target_distance;
                else
                    % dummy values
                    sm.participant(p).session(s).trial(k).final_distance_ego=999;
                    sm.participant(p).session(s).trial(k).ego_path_distance=999;
                    sm.participant(p).session(s).trial(k).adj_ego_path_distance=999;
                    sm.participant(p).session(s).trial(k).ego_target_distance=999;
                    sm.participant(p).session(s).trial(k).ego_target_distance_error=999;
                end
                
                % fprintf('Additional: Distance analysis to egocentric target done for %d, session %d, file no %d.\n', id, s, k);
                      
                %% rotation analysis
                % TOTAL XYZ ROTATION
                % calculate total rotation as change in yaw rotation (r)
                % this value includes rotation due to x-/y-trajectory (i.e. left-forward movement)
                % and additional rotation on the spot (i.e. left movement) 
                rot=0;
                for j=2:length(r)
                    rot=rot+abs(r(j)-r(j-1));
                end
                sm.participant(p).session(s).trial(k).rotation_xyz=rot; clear rot; 
                
                % XY ROTATION
                % calculate rotation due to unqiue x-/y-trajectory (i.e. left-forward movement)
                sm.participant(p).session(s).trial(k).rotation_xy = computeXYRotation(x_unique, y_unique);
                
                % Z ROTATION
                % calculate 'pure' z rotation (i.e. left movement on the spot) 
                sm.participant(p).session(s).trial(k).rotation_z = sm.participant(p).session(s).trial(k).rotation_xyz - sm.participant(p).session(s).trial(k).rotation_xy;
                sm.participant(p).session(s).trial(k).rotation_z_by_xy = computeDeviationToIdealValue(...
                    sm.participant(p).session(s).trial(k).rotation_xyz, ...
                    sm.participant(p).session(s).trial(k).rotation_xy); 
                               
                % fprintf('Rotation analysis done for %d, session %d, file no %d.\n', id, s, k);
                        
                %% analysis of chosen goal location (SUCCESS)
                % compute chosen goal location
                [sm.participant(p).session(s).trial(k).chosen_goal_i, ...
                    sm.participant(p).session(s).trial(k).chosen_alley_s, ...
                    sm.participant(p).session(s).trial(k).chosen_alley_i, ...
                    sm.participant(p).session(s).trial(k).obj_at_chosen_loc]=computeChosenGoals(...
                    sm.participant(p).session(s).rand_dict, ...
                    char(trial_data.chosen_goal(k,1)), ...
                    sm.coord.alley_poly, sm.coord.rec_poly, sm.coord.tri_poly,...
                    sm.coord.alley_names, sm.coord.goal_names,...
                    sm.participant(p).session(s).trial(k).x_n, ...
                    sm.participant(p).session(s).trial(k).y_n);
               
                % evaluate correctness of chosen goal location
                criterion = 0.1; % cut-off proximity to to goal (change if necessary)
                [sm.participant(p).session(s).trial(k).correct_goal, ...
                    sm.participant(p).session(s).trial(k).correct_goal_ego,...
                    sm.participant(p).session(s).trial(k).correct_final_alley,...
                    sm.participant(p).session(s).trial(k).correct_final_alley_ego]=computeSuccess(...
                    criterion, sm.participant(p).session(s).trial(k).final_distance,...
                    sm.participant(p).session(s).trial(k).final_distance_ego,...
                    sm.participant(p).session(s).trial(k).goal_alley,...
                    sm.participant(p).session(s).trial(k).support.ego_alley,...
                    sm.participant(p).session(s).trial(k).chosen_alley_i);
                
                % fprintf('Success performance analysis done for %d, session %d, file no %d.\n', id, s, k);
                
                %% TBD check if needed: zone analysis
                [sm.participant(p).session(s).trial(k).zone.alley_zone,...
                    sm.participant(p).session(s).trial(k).zone.rel_alley_zone,...
                    sm.participant(p).session(s).trial(k).zone.alley_entry,...
                    sm.participant(p).session(s).trial(k).zone.alley_rotations]=computeStaticZoneValues(x,...
                    y,r,sm.coord.alley_full_x,sm.coord.alley_full_y);
                
                [alley_entry_mat]=computeDynamicZoneValues(x,...
                    y,sm.coord.alley_full_x,sm.coord.alley_full_y);
                
                [sm.participant(p).session(s).trial(k).zone.alley_zone_out,...
                    sm.participant(p).session(s).trial(k).zone.rel_alley_zone_out,...
                    sm.participant(p).session(s).trial(k).zone.alley_entry_out,...
                    sm.participant(p).session(s).trial(k).zone.alley_rotations_out]=computeStaticZoneValues(x,...
                    y,r,sm.coord.alley_half_out_x,sm.coord.alley_half_out_y);
                
                [sm.participant(p).session(s).trial(k).zone.alley_zone_in,...
                    sm.participant(p).session(s).trial(k).zone.rel_alley_zone_in,...
                    sm.participant(p).session(s).trial(k).zone.alley_entry_in,...
                    sm.participant(p).session(s).trial(k).zone.alley_rotations_in]=computeStaticZoneValues(x,...
                    y,r,sm.coord.alley_half_in_x,sm.coord.alley_half_in_y);
                
                [sm.participant(p).session(s).trial(k).zone.triangle_zone,....
                    sm.participant(p).session(s).trial(k).zone.rel_triangle_zone,...
                    sm.participant(p).session(s).trial(k).zone.triangle_entry,...
                    sm.participant(p).session(s).trial(k).zone.triangle_rotations]=computeStaticZoneValues(x,...
                    y,r,sm.coord.tri_x,sm.coord.tri_y);
                
                [sm.participant(p).session(s).trial(k).zone.rectangle_zone,...
                    sm.participant(p).session(s).trial(k).zone.rel_rectangle_zone,...
                    sm.participant(p).session(s).trial(k).zone.rectangle_entry,...
                    sm.participant(p).session(s).trial(k).zone.rectangle_rotations]=computeStaticZoneValues(x,...
                    y,r,sm.coord.rec_x,sm.coord.rec_y);
                
                [rectangle_entry_mat]=computeDynamicZoneValues(x,...
                    y,sm.coord.rec_x,sm.coord.rec_y);
                
                [sm.participant(p).session(s).trial(k).zone.pentagon_zone,...
                    sm.participant(p).session(s).trial(k).zone.rel_pentagon_zone,...
                    sm.participant(p).session(s).trial(k).zone.pentagon_entry,...
                    sm.participant(p).session(s).trial(k).zone.pentagon_rotations]=computeStaticPentagonValues(x,...
                    y,r,sm.coord.central_x,sm.coord.central_y);
                
                [sm.participant(p).session(s).trial(k).zone.alley_time, ...
                    sm.participant(p).session(s).trial(k).zone.pentagon_time, ...
                    sm.participant(p).session(s).trial(k).zone.triangle_time, ...
                    sm.participant(p).session(s).trial(k).zone.rectangle_time]=computeTimeInZone(...
                    sm.participant(p).session(s).trial(k).time, ...
                    sm.participant(p).session(s).trial(k).zone.rel_alley_zone, ...
                    sm.participant(p).session(s).trial(k).zone.rel_pentagon_zone, ...
                    sm.participant(p).session(s).trial(k).zone.rel_triangle_zone, ...
                    sm.participant(p).session(s).trial(k).zone.rel_rectangle_zone);
                
                % fprintf('Zone analysis done for %d, session %d, file no %d.\n', id, s, k);
                
                %% exploration analysis
                % Path score as indicators of exploration
                sm.participant(p).session(s).trial(k).path_explored=sm_wp10_pathExplored(...
                    sm.participant(p).session(s).trial(k).zone.alley_zone_out,...
                    sm.participant(p).session(s).trial(k).zone.alley_zone_in,...
                    sm.participant(p).session(s).trial(k).zone.rectangle_zone,...
                    sm.participant(p).session(s).trial(k).zone.triangle_zone);
                sm.participant(p).session(s).trial(k).path_score=sm_wp10_pathScore(...
                    sm.participant(p).session(s).trial(k).zone.alley_entry_out,...
                    sm.participant(p).session(s).trial(k).zone.alley_entry_in,...
                    sm.participant(p).session(s).trial(k).zone.rectangle_entry,...
                    sm.participant(p).session(s).trial(k).zone.triangle_entry);
                

                
%                 % Direct path to target
%                 [ideal_no, match_abs, ...
%                     sm.participant(p).session(s).trial(k).direct_path_percent, ...
%                     sm.participant(p).session(s).trial(k).direct_path]=sm_wp10_directPathCalc(...
%                     uniq_alley, uniq_rect, alley_entry_mat, rectangle_entry_mat);
                
%                 % Search strategies
%                 [sm.participant(p).session(s).trial(k).search_strategy_direct,...
%                     sm.participant(p).session(s).trial(k).search_strategy_detour,...
%                     sm.participant(p).session(s).trial(k).search_strategy_reoriented,...
%                     sm.participant(p).session(s).trial(k).search_strategy_no]=sm_wp10_searchStrategy(...
%                     sm.participant(p).session(s).trial(k).direct_path,...
%                     sm.participant(p).session(s).trial(k).path_explored,...
%                     sm.participant(p).session(s).trial(k).path_score);
                
                % fprintf('Exploration analysis done for %d, session %d, file no %d.\n', id, s, k);
                
                %% set marker for excluded trials
                % criteria: timeout, or no movement/very short trial time (i.e. path_length=0, body_rot_abs=0, or time < 3)
                sm.participant(p).session(s).trial(k).exclude_trial_matlab=0;
                if sm.participant(p).session(s).trial(k).chosen_alley_i==999
                    sm.participant(p).session(s).trial(k).exclude_trial_matlab=1;
                    fprintf('Trial %d marked for exclusion due to timeout.\n',k);
                elseif sm.participant(p).session(s).trial(k).condition ~=4 % not for motor control task
                    if (sm.participant(p).session(s).trial(k).path_length<=0.1 ...
                            || sm.participant(p).session(s).trial(k).rotation_xyz==0 ...
                            || sm.participant(p).session(s).trial(k).time < 3)
                        sm.participant(p).session(s).trial(k).exclude_trial_matlab=1;
                        fprintf('Trial %d marked for exclusion due lack of movement or trial time < 3 sec.\n',k);
                    end
                end
                
                %% create plots
                sm_wp10_plotTrack(sm.participant(p).session(s).trial(k).trial_num, ...
                    sm.participant(p).session(s).session_number, ...
                    sm.participant(p).session(s).trial(k).condition, ...
                    sm.participant(p).session(s).trial(k).start_i, ...
                    sm.participant(p).id,sm.participant(p).group_s, ...
                    sm.participant(p).session(s).trial(k).correct_goal, ...
                    sm.participant(p).session(s).trial(k).direct_path, ...
                    sm.participant(p).session(s).trial(k).search_strategy_no, ...
                    sm.coord.alley_poly_out, sm.coord.alley_poly_in, sm.coord.tri_poly, sm.coord.rec_poly, ...
                    x, y, x_line_ego, y_line_ego, x_line, y_line, ...
                    sm.participant(p).session(s).trial(k).goal_x, ...
                    sm.participant(p).session(s).trial(k).goal_y,...
                    output_folder)
                
            else 
                %% For motor control navigation trial
                %% compute variables depending on the trial's settings
                % ideal path coordinates & length
                x_line_motor=[sm.coord.practise.start_x; sm.coord.practise.goal_x]; 
                y_line_motor=[sm.coord.practise.start_y; sm.coord.practise.goal_y];
                ideal_motor_path_length=computePathLength(x_line_motor,y_line_motor);
                
                % interpolate data for further analysis
                % using 'interparc' function by John D'Errico (Matlab File Exchanger)
                [xi_al,yi_al]=interpolateData(x_line_motor,y_line_motor,...
                    sm.participant(p).session(s).trial(k).support.ideal_path_length);
                clear x_line_motor y_line_motor; 
                
                %% time analysis
                % TIME
                sm.participant(p).session(s).trial(k).time=computeTime(t(1),t(end));

                %% standard coordinate analysis using x-/y-coordinates
                % PATH LENGTH to all targets
                sm.participant(p).session(s).trial(k).path_length=computePathLength(x,y); 
                              
                % PATH LENGTH ERROR to all target
                sm.participant(p).session(s).trial(k).path_length_error=computeDeviationToIdealValue(...
                    sm.participant(p).session(s).trial(k).path_length, ideal_motor_path_length);
                clear ideal_motor_path_length;
                 
                % VELOCITY
                sm.participant(p).session(s).trial(k).velocity=sm.participant(p).session(s).trial(k).path_length/sm.participant(p).session(s).trial(k).time;
                
                % fprintf('Motor control analysis done for %d, session %d, file no %d.\n', id, s, k);
                
                %% create plot 
                plotMotorControlTrial(sm.participant(p).session(s).trial(k).trial_num,...
                    sm.participant(p).session(s).session_number,...
                    sm.participant(p).id,sm.participant(p).group_s,...
                    sm.coord.practise.practise_poly, sm.coord.practise.goal_x, sm.coord.practise.goal_y,...
                    sm.coord.practise.start_x, sm.coord.practise.start_y,...
                    sm.coord.practise.practise_goal_names, x, y ,xi_al, yi_al, output_folder)
                
            end
        end   
    end
    
    % save .mat data %% TBD check %%
    save(file_path, 'sm', '-append')

    % update participant index
    p=p+1;
    
end

%% save .mat data %% TBD check %%
save(file_path, 'sm', '-append')

clear all; 