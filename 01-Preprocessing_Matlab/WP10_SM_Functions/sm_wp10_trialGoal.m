function [goal_x,goal_y,goal]=sm_wp10_trialGoal(trial_goal,goal_x,goal_y)
% SM_WP10_TRIALGOAL Return goal information for this trial for Starmaze
% WP10. 
%
% Input: 
% trial_goal information (string).
% goal_x, goal_y are xy-coordinates.
%
% Returns: goal information (integer) and xy-coordinates.

Goal1='GoalMA';
Goal3='GoalMC';
Goal9='GoalMI';

goal_1=contains(trial_goal,Goal1);
goal_3=contains(trial_goal,Goal3);
goal_9=contains(trial_goal,Goal9);

if goal_1==1
    goal_x=goal_x(1); goal_y=goal_y(1);
    goal=1;
elseif goal_3==1
    goal_x=goal_x(2); goal_y=goal_y(2);
    goal=3;
elseif goal_9==1
    goal_x=goal_x(3); goal_y=goal_y(3);
    goal=9;
end

end