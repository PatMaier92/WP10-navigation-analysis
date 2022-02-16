function [goal_identity_int]=setTrialGoalIdentity(goal_num, goal_identity)
% setTrialGoalIdentity: Returns goal identity for this trial for Starmaze
% WP10. 
%
% Input: 
% goal_num (integer). 
% goal_identity information (string).
%
% Returns: goal_identity_int (integer). 

% get first two digits (identifier)
if goal_identity=="none"
    goal_identity_int=999; 
else
    goal_identity_int=str2double(goal_identity(1:2));
    % additional check for implausible values 
    if goal_identity_int > goal_num
        goal_identity_int=999;
        disp('Unknown input information in trialGoalIdentity.m. Set to 999.');
    end
end

end