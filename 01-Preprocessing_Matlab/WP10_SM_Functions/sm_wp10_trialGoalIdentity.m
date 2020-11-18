function [goal_identity_int]=sm_wp10_trialGoalIdentity(goal_identity)
% SM_WP10_TRIALGOALIDENTITY Return goal identity for this trial for Starmaze
% WP10. 
%
% Input: 
% trial_goal_identity information (string).
%
% Returns: goal_identity_int (integer). 

goal_1=contains(goal_identity,'01-Fahrrad');
goal_2=contains(goal_identity,'02-Fussball');
goal_3=contains(goal_identity,'03-Geige');
goal_4=contains(goal_identity,'04-Stuhl');

if goal_1==1
    goal_identity_int=1;
elseif goal_2==1
    goal_identity_int=2;
elseif goal_3==1
    goal_identity_int=3;
elseif goal_4==1
    goal_identity_int=4;
else
    goal_identity_int=999;
    disp('Unknown input information in trialGoalIdentity.m. The goal_identity_int is set to 999.');
end

end