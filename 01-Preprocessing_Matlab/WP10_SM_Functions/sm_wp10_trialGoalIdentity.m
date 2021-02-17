function [goal_identity_int]=sm_wp10_trialGoalIdentity(goal_num, goal_identity)
% SM_WP10_TRIALGOALIDENTITY Return goal identity for this trial for Starmaze
% WP10. 
%
% Input: 
% goal_num is number ob goal objects (integer). 
% trial_goal_identity information (string).
%
% Returns: goal_identity_int (integer). 

% get first two digits (identifier)
if goal_identity~="none";
    goal_identity_int=str2double(goal_identity(1:2));
else
    % check for trials without goal object
    goal_identity_int=999; 
end

% check for other errors, trials with implausible goal object number
if goal_identity_int > goal_num
    goal_identity_int=999;
    disp('Unknown input information in trialGoalIdentity.m. Set to 999.');
end

end