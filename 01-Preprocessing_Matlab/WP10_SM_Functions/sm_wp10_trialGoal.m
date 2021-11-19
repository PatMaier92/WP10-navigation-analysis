function [goal_x,goal_y,goal_int,goal_str,alley_int]=sm_wp10_trialGoal(trial_goal,...
    all_goal_x,all_goal_y,goal_locs,alley_locs)
% SM_WP10_TRIALGOAL Return goal information for this trial for Starmaze
% WP10.
%
% Input:
% trial_goal information (char).
% goal_x, goal_y are xy-coordinate vectors of all goals.
% goal_locs is ordered string array of goals.
% alley_locs is ordered string array of alley letters.
%
% Returns: goal information (integer, string), alley information (integer)
% and x-/y-coordinates.

% string
goal_str=trial_goal(end);

% integer
goal_int=find(contains(goal_locs, goal_str));
alley_int=find(contains(alley_locs, goal_str));

% coordinates
goal_x=all_goal_x(goal_int);
goal_y=all_goal_y(goal_int);

% correct for false input
if isempty(goal_int) || isempty(alley_int)
    disp('Unknown input information in trialGoal.m. Set to 999.');
    goal_int=999;
    alley_int=999;
    goal_x=999;
    goal_y=999;
end

end