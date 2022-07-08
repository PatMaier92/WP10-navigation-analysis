clear; close all; clc; format long;
addpath(genpath(pwd)) % add subfolder functions to path

%% Starmaze Data Processing
% @ date 2019-10-01 @ author Deetje Iggena
% @ date 2020-11-06 update by @ Patrizia Maier & now tracked by git
% for Starmaze version WP10 Frankfurt
% requires Matlab 2021a or later (for shortestpath())
% requires Signal Processing Toolbox for downsample() and dtw()
% requires Text Analytics Toolbox for editDistance()

% The script requires .csv files as input.
% One tracker file per trial with timestamp, x- and y-coordinates for movememt
% and z-coordinates for rotation.
% One trial_results file with general information (id, session, condition).

% Block 1: Set up input/output folders, Starmaze and Practise environment
% Block 2: Data preparation 
% Block 3: Data analysis
% Block 4: Write data to xlsx file

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
file_name         = '\wp10_results_navigation.mat';
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
    [sm.coord.goal_x_in_alleys,sm.coord.goal_y_in_alleys]=setGoalMatrix(sm.coord.goal_x,sm.coord.goal_y);
    
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
    
    % compute random x-/y-coordinate distribution in Starmaze
    [sm.coord.random_x, sm.coord.random_y]=computeRandomLocations(sm.coord.full_poly, 1000); 
    
    % compute final distance between locations and random x-/y-coordinates 
    sm.coord.final_distance_distribution={}; 
    for i=1:size(sm.coord.goal_x_in_alleys,1)
        for j=1:size(sm.coord.goal_x_in_alleys,2)
            distribution=zeros(length(sm.coord.random_x),1); 
            for m=1:length(sm.coord.random_x)
                distribution(m)=computeDistance(sm.coord.goal_x_in_alleys(i,j),...
                    sm.coord.random_x(m), sm.coord.goal_y_in_alleys(i,j), sm.coord.random_y(m));
            end
            sm.coord.final_distance_distribution{i,j}=sort(distribution, 'ascend'); 
        end 
    end
    
    % create graph
    % for automated shortest path calculation (requires Matlab 2021a)
    [sm.coord.graph,sm.coord.graph_x,sm.coord.graph_y]=setGraph(sm.coord.start_x,sm.coord.start_y,...
        sm.coord.tri_x,sm.coord.tri_y,sm.coord.goal_x,sm.coord.goal_y);

    % add (ordered) information
    sm.coord.goal_names=["MA", "MC", "MI"];
    sm.coord.start_names=["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "X"];
    sm.coord.alley_names=["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"];

    % create test figure plot
    % plotTestFigure("s_maze",sm.coord.full_poly,sm.coord.goal_x,sm.coord.goal_y,sm.coord.start_x,sm.coord.start_y,sm.coord.goal_names,sm.coord.graph,sm.coord.graph_x,sm.coord.graph_y);

    clear i j distribution alley_x alley_y pentagon_x pentagon_y i_alley n_alleys i_corner n_corners start goal values; 
    
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
    % plotTestFigure("p_maze",sm.coord.practise.practise_poly,sm.coord.practise.goal_x,sm.coord.practise.goal_y,sm.coord.practise.start_x,sm.coord.practise.start_y,sm.coord.practise.practise_goal_names,sm.coord.graph,sm.coord.graph_x,sm.coord.graph_y);

    clear practise_alley_x practise_alley_y i_corner n_corners practise_start practise_goal practise_values; 
    
    %% initialize participant index
    p=1; 
else
    p=0; % dummy value 
end

%% Block 2: Data preprocessing
for id=participant_start:participant_end
tic;     
    % set participant index
    if p~=1 
        % check if ID exists in data 
        p_ind = find([sm.participant.id]==id); 
        if isempty(p_ind) % if not: append participant data    
            [~,n]=size(sm.participant);
            p=n+1; clear n p_ind; 
        else % if yes: overwrite participant data
            p=p_ind; clear p_ind;
            fprintf('Data for participant %d already existed and is overwritten now.\n', id);
        end
    end 
    
    % loop over sessions 
    for s=1:n_sessions        
        %% set individual input and output folder
        % input folder
        input_folder=[data_folder '\' num2str(id) '\S00' num2str(s)]; 
        if ~exist(input_folder, 'dir')
            fprintf('Folder for participant %d, session %d does not exist. Continue with next iteration.\n', id, s);
            continue;
        end
        
        % output folder (only for trial plots)
        output_folder=[data_folder '\' num2str(id) '\plots'];
        if ~exist(output_folder, 'dir')
            mkdir(output_folder);
            fprintf('Your output folder for participant %d did not exist, a new folder was created.\n', id);
        end
        
        %% read-in trial results file
        opts=detectImportOptions([input_folder, '\trial_results.csv']);
        opts=setvaropts(opts,'timestamp','InputFormat','MM/dd/uuuu hh:mm:ss aa');
        trial_data=readtable([input_folder, '\trial_results.csv'], opts); clear opts; 
        
        % store participant information
        sm.participant(p).id=id;
        [sm.participant(p).group, sm.participant(p).sex]=setGroupSexInfo(sm.participant(p).id);
        sm.participant(p).session(s).session_num=trial_data.session_num(1,1);
        
        %% read-in log file (only once in session 1)
        if s==1
            opts=detectImportOptions([input_folder, '\log.csv'], 'VariableNamesLine', 1, 'Delimiter', ',');
            opts.DataLines=[2,Inf]; 
            opts.SelectedVariableNames = {'message'};
            log_data=table2cell(readtable([input_folder, '\log.csv'], opts));
            log_data=log_data(contains(log_data,'ID is'));
            [sm.participant(p).rand_dict]=setRandomizationDict(log_data, id);
            clear log_data opts; 
        end 
        
        %% get individual trial tracker file names 
        d = dir(fullfile(input_folder, 'trackedpoint_*.csv'));
        files = {d.name}; clear d;
        
        % loop over trials 
        for k=1:numel(files)
            name=files{k};
            
            % for session 3 only process trial 2 (motor control task)
            if s==3
                pattern=("_T001"|"_T003"|"_T004"|"_T005"|"_T006"|"_T007");
                if contains(name, pattern)
                    continue
                end
            end
            
            % read-in and clean trial tracker data
            data=readtable(fullfile(input_folder, name));
            data=cleanTrialTrackerFile(data,s); 
        
            % get sampling rate 
            % (if sampling rate differs, data needs to be corrected and 
            % interpolated before further analysis, however,
            % sampling rate seems pretty consistent in WP10 data)
            sampling_rate_original=zeros(length(data.time)-1,1);
            for i=1:length(data.time)-1
                sampling_rate_original(i)=data.time(i+1)-data.time(i);
            end
            sm.participant(p).session(s).trial(k).sampling_rate=sum(sampling_rate_original)/length(sampling_rate_original);
            clear sampling_rate_original i; 
            
            % extract data 
            t=data.time; % time
            x=data.pos_x; % coordinates
            y=data.pos_z; % coordinates
            r=data.rot_y; % yaw rotations
            clear data; 
            
            % spatial normalization
            if s==3 % practise maze
                x=setNormalizedValues(x,sm.coord.practise.xmin,sm.coord.practise.xmax); y=setNormalizedValues(y,sm.coord.practise.ymin,sm.coord.practise.ymax);
            else % star maze
                x=setNormalizedValues(x,sm.coord.xmin,sm.coord.xmax); y=setNormalizedValues(y,sm.coord.ymin,sm.coord.ymax);
            end
            
            % save start and end points 
            sm.participant(p).session(s).trial(k).x_1=x(1); sm.participant(p).session(s).trial(k).y_1=y(1);
            sm.participant(p).session(s).trial(k).x_n=x(end); sm.participant(p).session(s).trial(k).y_n=y(end);
                                  
            %% get single trial info from trial_results
            sm.participant(p).session(s).session_duration=round(minutes(trial_data.timestamp(numel(files),1) - trial_data.timestamp(1,1))); 
            
            sm.participant(p).session(s).trial(k).block=trial_data.block_num(k,1);
            sm.participant(p).session(s).trial(k).trial=trial_data.trial_num(k,1);
            sm.participant(p).session(s).trial(k).trial_in_block=trial_data.trial_num_in_block(k,1);
            
            sm.participant(p).session(s).trial(k).feedback=string(trial_data.trial_feedback(k,1));
               
            condition=string(trial_data.trial_type(k,1));
            sm.participant(p).session(s).trial(k).condition=setTrialCondition(condition,sm.participant(p).session(s).trial(k).feedback);
            clear condition; 
            
            sm.participant(p).session(s).trial(k).goal_identity=string(trial_data.trial_goal_identity(k,1));
            
            [sm.participant(p).session(s).trial(k).goal_x,sm.participant(p).session(s).trial(k).goal_y,...
                sm.participant(p).session(s).trial(k).goal_i,sm.participant(p).session(s).trial(k).goal_s,...
                sm.participant(p).session(s).trial(k).goal_alley]=setTrialGoalLocation(char(string(trial_data.trial_goal(k,1))),...
                sm.coord.goal_x,sm.coord.goal_y,sm.coord.goal_names,sm.coord.alley_names);
            
            start_name=char(trial_data.trial_player(k,1)); sm.participant(p).session(s).trial(k).start_s=start_name(end); clear start_name; 
            [sm.participant(p).session(s).trial(k).start_i]=setTrialStartLocation(sm.participant(p).session(s).trial(k).start_s,sm.coord.start_names);
            
            %% For all normal navigation trials (i.e., not motor control task)
            if sm.participant(p).session(s).trial(k).condition~="practise"
                %% compute support variables depending on this trial's settings
                % ideal path coordinates & ideal path length
                % Caveat: dummy values for egocentric from inner starts
                % Requires Matlab 2021a for 'shortestpath' function and
                % 'interparc' function by John D'Errico (Matlab File Exchanger)
                [x_line, y_line, origin_x_line, origin_y_line, x_line_chosen, y_line_chosen, x_line_ego, y_line_ego,...
                    x_line_A, y_line_A, x_line_C, y_line_C, x_line_E, y_line_E, x_line_G, y_line_G, x_line_I, y_line_I,...
                    sm.participant(p).session(s).trial(k).goal_x_ego, sm.participant(p).session(s).trial(k).goal_y_ego,...
                    sm.participant(p).session(s).trial(k).ego_alley,...
                    sm.participant(p).session(s).trial(k).ideal_path_length, sm.participant(p).session(s).trial(k).ideal_chosen_path_length,...
                    sm.participant(p).session(s).trial(k).ideal_ego_path_length,...
                    ideal_path_A, ideal_path_C, ideal_path_E, ideal_path_G, ideal_path_I]=computeStartDependentIdealVariables(...
                    sm.coord.graph, sm.coord.graph_x, sm.coord.graph_y,...
                    sm.participant(p).session(s).trial(k).start_i, sm.participant(p).session(s).trial(k).goal_i,...
                    sm.participant(p).session(s).trial(k).x_n, sm.participant(p).session(s).trial(k).y_n,...
                    sm.coord.goal_x_in_alleys, sm.coord.goal_y_in_alleys,...
                    sm.coord.alley_full_x, sm.coord.alley_full_y, sm.coord.rec_x, sm.coord.rec_y, ...
                    sm.coord.central_poly, sm.coord.full_poly);
                               
                % interpolate ideal path data for further analysis
                % using 'interparc' function by John D'Errico (Matlab File Exchanger)
                [xi_al,yi_al]=interpolateData(x_line, y_line, sm.participant(p).session(s).trial(k).ideal_path_length);
                
                [xi_ch,yi_ch]=interpolateData(x_line_chosen, y_line_chosen, sm.participant(p).session(s).trial(k).ideal_chosen_path_length);
                
                [xi_eg,yi_eg]=interpolateData(x_line_ego, y_line_ego, sm.participant(p).session(s).trial(k).ideal_ego_path_length);
                
                [xi_st,yi_st]=interpolateData(x_line_G, y_line_G, ideal_path_G);
                
%                 % test plot
%                 figure; plot(sm.coord.full_poly); hold on; 
%                 plot(x, y, 'k-', x_line, y_line, 'k+', xi_al, yi_al, 'k-',...
%                     x_line_ego, y_line_ego, 'rx', xi_eg, yi_eg, 'r-',...
%                     x_line_chosen, y_line_chosen, 'bx', xi_ch, yi_ch, 'b-',...
%                     origin_x_line, origin_y_line, 'm--');
%                 xlim([0 1]); ylim([0 1]); hold off; 

                % compute ideal path zone sequences
                [ideal_seq_10]=computeZoneSequence(x_line, y_line, sm.coord.alley_full_x, sm.coord.alley_full_y,...
                    sm.coord.alley_half_out_x, sm.coord.alley_half_out_y, sm.coord.alley_half_in_x, sm.coord.alley_half_in_y,...
                    sm.coord.rec_x, sm.coord.rec_y, sm.coord.tri_x, sm.coord.tri_y, 10);
                
                [ideal_seq_10_chosen]=computeZoneSequence(x_line_chosen, y_line_chosen, sm.coord.alley_full_x, sm.coord.alley_full_y,...
                    sm.coord.alley_half_out_x, sm.coord.alley_half_out_y, sm.coord.alley_half_in_x, sm.coord.alley_half_in_y,...
                    sm.coord.rec_x, sm.coord.rec_y, sm.coord.tri_x, sm.coord.tri_y, 10);
                 
                [ideal_seq_10_ego]=computeZoneSequence(x_line_ego, y_line_ego, sm.coord.alley_full_x, sm.coord.alley_full_y,...
                    sm.coord.alley_half_out_x, sm.coord.alley_half_out_y, sm.coord.alley_half_in_x, sm.coord.alley_half_in_y,...
                    sm.coord.rec_x, sm.coord.rec_y, sm.coord.tri_x, sm.coord.tri_y, 10);
                
                [ideal_seq_10_origin]=computeZoneSequence(origin_x_line, origin_y_line, sm.coord.alley_full_x, sm.coord.alley_full_y,...
                    sm.coord.alley_half_out_x, sm.coord.alley_half_out_y, sm.coord.alley_half_in_x, sm.coord.alley_half_in_y,...
                    sm.coord.rec_x, sm.coord.rec_y, sm.coord.tri_x, sm.coord.tri_y, 10);
                               
                %% Block 3: Data analysis, i.e. calculcation of variables  
                %% accuracy analysis
                % compute chosen goal location
                [sm.participant(p).session(s).trial(k).chosen_goal_i, sm.participant(p).session(s).trial(k).chosen_alley_s, ...
                    sm.participant(p).session(s).trial(k).chosen_alley_i, sm.participant(p).session(s).trial(k).obj_at_chosen_loc]=computeChosenGoals(...
                    sm.participant(p).rand_dict, char(trial_data.chosen_goal(k,1)), ...
                    sm.coord.alley_poly, sm.coord.rec_poly, sm.coord.tri_poly, sm.coord.alley_names, sm.coord.goal_names,...
                    sm.participant(p).session(s).trial(k).x_n, sm.participant(p).session(s).trial(k).y_n);
                 
                % evaluate global accuracy of chosen location
                sm.participant(p).session(s).trial(k).correct_final_alley=sm.participant(p).session(s).trial(k).goal_alley==sm.participant(p).session(s).trial(k).chosen_alley_i; 
                sm.participant(p).session(s).trial(k).correct_final_alley_ego=sm.participant(p).session(s).trial(k).ego_alley==sm.participant(p).session(s).trial(k).chosen_alley_i ...
                    && sm.participant(p).session(s).trial(k).correct_final_alley==0;        

                % fprintf('Accuracy analysis done for %d, session %d, file no %d.\n', id, s, k);   
                
                %% time analysis
                % TIME
                sm.participant(p).session(s).trial(k).time=computeTime(t(1),t(end));
                % fprintf('Time analysis done for %d, session %d, file no %d.\n', id, s, k);           
                               
                %% standard coordinate analysis using x-/y-coordinates
                % PATH LENGTH 
                sm.participant(p).session(s).trial(k).path_length=computePathLength(x,y); 
                
                % PATH LENGTH ERROR to TARGET 
                sm.participant(p).session(s).trial(k).path_length_error=computeDeviationToIdealValue(...
                    sm.participant(p).session(s).trial(k).path_length, ...
                    sm.participant(p).session(s).trial(k).ideal_path_length);
                
                % VELOCITY 
                sm.participant(p).session(s).trial(k).velocity=sm.participant(p).session(s).trial(k).path_length / ...
                    sm.participant(p).session(s).trial(k).time;
                
%                 % AVERAGE DISTANCE to PATH
%                 [sm.participant(p).session(s).trial(k).path_distance, ~]=computePathDistance(...
%                     xi_al, yi_al, x, y, sm.participant(p).session(s).trial(k).final_distance, true);  
                
                % Exploratory: DYNAMIC TIME WARPING DISTANCE for PATH to TARGET
                sm.participant(p).session(s).trial(k).dtw_path_distance=dtw([xi_al,yi_al]',[x,y]');
                
                % AVERAGE DISTANCE to TARGET 
                % target distance 
                [sm.participant(p).session(s).trial(k).target_distance, ~]=computeTargetDistance(x,y,...
                    sm.participant(p).session(s).trial(k).goal_x,sm.participant(p).session(s).trial(k).goal_y); 
                 % ideal target distance 
                [sm.participant(p).session(s).trial(k).ideal_target_distance, ~]=computeTargetDistance(xi_al,yi_al,...
                    sm.participant(p).session(s).trial(k).goal_x,sm.participant(p).session(s).trial(k).goal_y);               
                % target distance deviation 
                sm.participant(p).session(s).trial(k).target_distance_deviation=sm.participant(p).session(s).trial(k).target_distance - sm.participant(p).session(s).trial(k).ideal_target_distance; 
                
                % normalized target distance score 
                sm.participant(p).session(s).trial(k).target_distance_score=computeDistanceScore(...
                    sm.participant(p).session(s).trial(k).target_distance,x,y,...
                    sm.coord.random_x,sm.coord.random_y); 
                % normalized ideal target distance score
                sm.participant(p).session(s).trial(k).ideal_target_distance_score=computeDistanceScore(...
                    sm.participant(p).session(s).trial(k).ideal_target_distance,xi_al,yi_al,...
                    sm.coord.random_x,sm.coord.random_y); 
                % normalized target distance score deviation
                sm.participant(p).session(s).trial(k).target_distance_score_deviation=sm.participant(p).session(s).trial(k).target_distance_score - sm.participant(p).session(s).trial(k).ideal_target_distance_score;            
                
                % fprintf('Standard path and distance analysis done for %d, session %d, file no %d.\n', id, s, k);
                
                %% additional distance analysis for probe trials  
                if sm.participant(p).session(s).trial(k).feedback=="false"  
                    % FINAL DISTANCE to TARGET 
                    sm.participant(p).session(s).trial(k).final_distance=computeDistance(...
                        sm.participant(p).session(s).trial(k).goal_x, sm.participant(p).session(s).trial(k).x_n, ...
                        sm.participant(p).session(s).trial(k).goal_y, sm.participant(p).session(s).trial(k).y_n);
                   
                    % MEMORY SCORE (final distance in relation to distribution of final distance for random points)
                    sm.participant(p).session(s).trial(k).memory_score=computeMemoryScore(sm.coord.final_distance_distribution,...
                        sm.participant(p).session(s).trial(k).final_distance, sm.participant(p).session(s).trial(k).goal_i,...
                        sm.participant(p).session(s).trial(k).goal_alley);

                    % PATH LENGTH ERROR to CHOSEN target 
                    sm.participant(p).session(s).trial(k).chosen_path_length_error=computeDeviationToIdealValue(...
                        sm.participant(p).session(s).trial(k).path_length, ...
                        sm.participant(p).session(s).trial(k).ideal_chosen_path_length);
                
%                     % AVERAGE DISTANCE to PATH to CHOSEN target 
%                     [sm.participant(p).session(s).trial(k).chosen_path_distance, ~]=computePathDistance(...
%                         xi_ch, yi_ch, x, y, 0, false);   

                    % Exploratory: DYNAMIC TIME WARPING DISTANCE for PATH to CHOSEN target 
                    sm.participant(p).session(s).trial(k).chosen_dtw_path_distance=dtw([xi_ch,yi_ch]',[x,y]');
                    
%                     % AVERAGE DISTANCE to CHOSEN TARGET
%                     % target distance 
%                     [sm.participant(p).session(s).trial(k).chosen_target_distance, ~]=computeTargetDistance(x,y,x(end),y(end)); 
%                     % ideal target distance 
%                     [ideal_chosen_target_distance, ~]=computeTargetDistance(xi_ch,yi_ch,x(end),y(end)); 
%                     % target distance error 
%                     sm.participant(p).session(s).trial(k).chosen_target_distance_error=computeDeviationToIdealValue(...
%                         sm.participant(p).session(s).trial(k).chosen_target_distance, ideal_chosen_target_distance);  
%                     clear ideal_chosen_target_distance; 
                else
                    % dummy values
                    sm.participant(p).session(s).trial(k).final_distance=999;
                    sm.participant(p).session(s).trial(k).memory_score=999; 
%                     sm.participant(p).session(s).trial(k).chosen_path_distance=999;
                    sm.participant(p).session(s).trial(k).chosen_path_length_error=999; 
                    sm.participant(p).session(s).trial(k).chosen_dtw_path_distance=999;
%                     sm.participant(p).session(s).trial(k).chosen_target_distance=999;
%                     sm.participant(p).session(s).trial(k).chosen_target_distance_error=999;
                end
                
                % fprintf('Additional: Distance analysis to chosen target done for %d, session %d, file no %d.\n', id, s, k);
          
                %% additional distance analysis for allocentric probe trials
                % homing behavior
                if sm.participant(p).session(s).trial(k).condition=="allo_ret"
                    % FINAL DISTANCE to target in START ALLEY
                    sm.participant(p).session(s).trial(k).final_distance_start=computeDistance(...
                        sm.coord.goal_x_in_alleys(sm.participant(p).session(s).trial(k).goal_i, 4), sm.participant(p).session(s).trial(k).x_n,...
                        sm.coord.goal_y_in_alleys(sm.participant(p).session(s).trial(k).goal_i, 4), sm.participant(p).session(s).trial(k).y_n);
                    
                    % MEMORY SCORE to target in START ALLEY
                    sm.participant(p).session(s).trial(k).memory_score_start=computeMemoryScore(sm.coord.final_distance_distribution,...
                        sm.participant(p).session(s).trial(k).final_distance_start, sm.participant(p).session(s).trial(k).goal_i,...
                        7);
                    
%                     % AVERAGE DISTANCE to target in START ALLEY
%                     % target distance 
%                     [sm.participant(p).session(s).trial(k).start_target_distance, ~]=computeTargetDistance(x,y,...
%                         sm.coord.goal_x_in_alleys(sm.participant(p).session(s).trial(k).goal_i, 4), sm.coord.goal_y_in_alleys(sm.participant(p).session(s).trial(k).goal_i, 4)); 
%                     % ideal target distance 
%                     [ideal_start_target_distance, ~]=computeTargetDistance(xi_st,yi_st,...
%                         sm.coord.goal_x_in_alleys(sm.participant(p).session(s).trial(k).goal_i, 4), sm.coord.goal_y_in_alleys(sm.participant(p).session(s).trial(k).goal_i, 4));
%                     % target distance error 
%                     sm.participant(p).session(s).trial(k).start_target_distance_error=computeDeviationToIdealValue(...
%                         sm.participant(p).session(s).trial(k).start_target_distance, ideal_start_target_distance); 
%                     clear ideal_start_target_distance;
                else 
                    % dummy values
                    sm.participant(p).session(s).trial(k).final_distance_start=999;
                    sm.participant(p).session(s).trial(k).memory_score_start=999; 
%                     sm.participant(p).session(s).trial(k).start_target_distance=999;
%                     sm.participant(p).session(s).trial(k).start_target_distance_error=999;
                end 
                
                % egocentric behavior
                % excludes trials with inner starts because no clear egocentric path/goal location
                if sm.participant(p).session(s).trial(k).condition=="allo_ret" && mod(sm.participant(p).session(s).trial(k).start_i,2)
                    % FINAL DISTANCE to EGOCENTRIC target
                    sm.participant(p).session(s).trial(k).final_distance_ego=computeDistance(...
                        sm.participant(p).session(s).trial(k).goal_x_ego, sm.participant(p).session(s).trial(k).x_n,...
                        sm.participant(p).session(s).trial(k).goal_y_ego, sm.participant(p).session(s).trial(k).y_n);
                    
                    % MEMORY SCORE to EGOCENTRIC target(final distance to ego in relation to distribution of final distance for random points)
                    sm.participant(p).session(s).trial(k).memory_score_ego=computeMemoryScore(sm.coord.final_distance_distribution,...
                        sm.participant(p).session(s).trial(k).final_distance_ego, sm.participant(p).session(s).trial(k).goal_i,...
                        sm.participant(p).session(s).trial(k).ego_alley);

%                     % AVERAGE DISTANCE to EGOCENTRIC PATH
%                     [sm.participant(p).session(s).trial(k).ego_path_distance, ~]=computePathDistance(...
%                         xi_eg, yi_eg, x, y, sm.participant(p).session(s).trial(k).final_distance_ego, true);     

                    % Exploratory: DYNAMIC TIME WARPING DISTANCE for PATH
                    sm.participant(p).session(s).trial(k).ego_dtw_path_distance=dtw([xi_eg,yi_eg]',[x,y]');
 
%                     % AVERAGE DISTANCE to EGOCENTRIC TARGET
%                     % target distance 
%                     [sm.participant(p).session(s).trial(k).ego_target_distance, ~]=computeTargetDistance(x,y,...
%                         sm.participant(p).session(s).trial(k).goal_x_ego, sm.participant(p).session(s).trial(k).goal_y_ego); 
%                     % ideal target distance 
%                     [ideal_ego_target_distance, ~]=computeTargetDistance(xi_eg,yi_eg,...
%                         sm.participant(p).session(s).trial(k).goal_x_ego, sm.participant(p).session(s).trial(k).goal_y_ego); 
%                     % target distance error 
%                     sm.participant(p).session(s).trial(k).ego_target_distance_error=computeDeviationToIdealValue(...
%                         sm.participant(p).session(s).trial(k).ego_target_distance, ideal_ego_target_distance); 
%                     clear ideal_ego_target_distance;
                else
                    % dummy values
                    sm.participant(p).session(s).trial(k).final_distance_ego=999;
                    sm.participant(p).session(s).trial(k).memory_score_ego=999;
%                     sm.participant(p).session(s).trial(k).ego_path_distance=999;
                    sm.participant(p).session(s).trial(k).ego_dtw_path_distance=999;
%                     sm.participant(p).session(s).trial(k).ego_target_distance=999;
%                     sm.participant(p).session(s).trial(k).ego_target_distance_error=999;
                end
                
                % fprintf('Additional: Distance analysis to egocentric target done for %d, session %d, file no %d.\n', id, s, k);
                      
                %% rotation analysis
                % TOTAL ROTATION
                % calculate total rotation in degrees as change in yaw rotation (r)
                % this value includes rotation due to x-/y-trajectory (i.e. left-forward movement)
                rot=zeros(1,length(r)-1); 
                for j=2:length(r)
                    temp=abs(r(j)-r(j-1));
                    if temp > 180 % correct errors due to switch at 0° to 360°
                        temp=360-temp; 
                    end 
                    rot(j-1)=temp;
                end
                sm.participant(p).session(s).trial(k).rotation_degrees=sum(rot);  
                sm.participant(p).session(s).trial(k).rotation_turns=sum(rot)/360;
                sm.participant(p).session(s).trial(k).rotation_turns_by_path_length=sum(rot)/360/sm.participant(p).session(s).trial(k).path_length; 
                clear rot j temp;
                     
                % fprintf('Rotation analysis done for %d, session %d, file no %d.\n', id, s, k);       
                
                %% zone analysis for exploration behavior             
                % compute path zone sequence (10 zones) for actual data 
                [seq_10]=computeZoneSequence(x, y, sm.coord.alley_full_x, sm.coord.alley_full_y,...
                    sm.coord.alley_half_out_x, sm.coord.alley_half_out_y, sm.coord.alley_half_in_x, sm.coord.alley_half_in_y,...
                    sm.coord.rec_x, sm.coord.rec_y, sm.coord.tri_x, sm.coord.tri_y, 10); 
                
                % compute path zone sequence (20 zones) for actual data 
                [seq_20]=computeZoneSequence(x, y, sm.coord.alley_full_x, sm.coord.alley_full_y,...
                    sm.coord.alley_half_out_x, sm.coord.alley_half_out_y, sm.coord.alley_half_in_x, sm.coord.alley_half_in_y,...
                    sm.coord.rec_x, sm.coord.rec_y, sm.coord.tri_x, sm.coord.tri_y, 20); 
  
                % number of explored zones (i.e. re-entries not counted)
                zones_explored=length(unique(seq_20));
                
                % number of entered zones (i.e. re-entries counted) 
                zones_entered=length(seq_20);
                
                % EDIT DISTANCE 
                % compute edit distance costs (i.e. deviation between ideal and actual path zone sequence) 
                % method: Levenshtein - lowest number of insertions, deletions, and substitutions
                sm.participant(p).session(s).trial(k).path_edit_distance=editDistance(seq_10, ideal_seq_10);
                sm.participant(p).session(s).trial(k).chosen_path_edit_distance=editDistance(seq_10, ideal_seq_10_chosen);
                                                                 
                % SEARCH STRATEGY category
                [sm.participant(p).session(s).trial(k).search_strategy]=computeSearchStrategy(...
                    zones_explored, zones_entered,...
                    sm.participant(p).session(s).trial(k).chosen_path_edit_distance); 
                
                % fprintf('Exploration analysis done for %d, session %d, file no %d.\n', id, s, k);
                
                %% set marker for excluded trials
                % criteria: timeout, or no movement/very short trial time (i.e. path_length=0, rotation=0, or time < 3)
                sm.participant(p).session(s).trial(k).exclude_trial_matlab=0;
                if sm.participant(p).session(s).trial(k).chosen_alley_i==999
                    sm.participant(p).session(s).trial(k).exclude_trial_matlab=1; 
                    fprintf('Trial %d marked for exclusion due to timeout.\n',k);
                elseif (sm.participant(p).session(s).trial(k).path_length<=0.1 ...
                        || sm.participant(p).session(s).trial(k).rotation_degrees==0 ...
                        || sm.participant(p).session(s).trial(k).time<3)
                        sm.participant(p).session(s).trial(k).exclude_trial_matlab=1;
                        fprintf('Trial %d marked for exclusion due lack of movement or trial time < 3 sec.\n',k);
                end
                
                %% create trial plot  
%                 plotTrialTrack(sm.participant(p).session(s).trial(k).trial, sm.participant(p).session(s).session_num,...
%                     sm.participant(p).session(s).trial(k).condition, sm.participant(p).session(s).trial(k).start_i,...
%                     sm.participant(p).id,sm.participant(p).group, sm.participant(p).session(s).trial(k).correct_final_alley,...
%                     sm.participant(p).session(s).trial(k).search_strategy,...
%                     sm.coord.full_poly, x, y, x_line, y_line, x_line_ego, y_line_ego, x_line_chosen, y_line_chosen,...
%                     sm.participant(p).session(s).trial(k).goal_x, sm.participant(p).session(s).trial(k).goal_y, output_folder);

            else 
                %% For motor control task               
                %% compute support variables depending on the trial's settings
                % ideal path coordinates & length
                x_line_motor=[sm.coord.practise.start_x; sm.coord.practise.goal_x]; y_line_motor=[sm.coord.practise.start_y; sm.coord.practise.goal_y];
                ideal_motor_path_length=computePathLength(x_line_motor,y_line_motor);
                               
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
                
                % TOTAL ROTATION
                rot=zeros(1,length(r)-1); 
                for j=2:length(r)
                    temp=abs(r(j)-r(j-1));
                    if temp > 180 % correct errors due to switch at 0° to 360°
                        temp=360-temp; 
                    end 
                    rot(j-1)=temp;
                end
                sm.participant(p).session(s).trial(k).rotation_degrees=sum(rot);  
                sm.participant(p).session(s).trial(k).rotation_turns=sum(rot)/360;
                sm.participant(p).session(s).trial(k).rotation_turns_by_path_length=sum(rot)/360/sm.participant(p).session(s).trial(k).path_length; 
                clear rot j temp;
                
                % fprintf('Motor control analysis done for %d, session %d, file no %d.\n', id, s, k);
                
                %% set marker for excluded trials to zero
                sm.participant(p).session(s).trial(k).exclude_trial_matlab=0;
                
                %% create trial plot  
%                 plotMotorControlTrial(sm.participant(p).session(s).trial(k).trial, sm.participant(p).session(s).session_num,...
%                     sm.participant(p).id,sm.participant(p).group, sm.coord.practise.practise_poly, ...
%                     sm.coord.practise.goal_x, sm.coord.practise.goal_y, sm.coord.practise.start_x, sm.coord.practise.start_y,...
%                     sm.coord.practise.practise_goal_names, x, y , x_line_motor, y_line_motor, output_folder);
%                 clear x_line_motor y_line_motor;     
            end
            
            clear x* y* t r ideal_path* origin* *seq* zones* name;             
        end
        
        clear files trial_data input_folder; 
        fprintf('Analysis done for %d, session %d.\n', id, s);
    end
    
    p=0; % dummy value
    save(file_path, 'sm'); 
    toc;
end

%% Block 4: Write data to xlsx file
% [data_folder]  = setInputPath();
% writeNavTableToXLSX(data_folder); 

clear; 