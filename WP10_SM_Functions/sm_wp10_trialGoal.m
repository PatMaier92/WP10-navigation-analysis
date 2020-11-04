function [goal_x, goal_y, goal]=sm_wp10_trialGoal(wp,trial_goal, goal_x, goal_y)
% SM_WP10_TRIALGOAL Return goal information for this trial for Starmaze
% WP10. 
%
% Input: 
% wp is Starmaze work package (integer).
% trial_goal information (string).
% goal_x, goal_y are xy-coordinates.
%
% Returns: goal information (integer). 

if wp==10
    Goal1='GoalMA';
    Goal2='GoalMC';
    Goal3='GoalMI';
end

goal_1=contains(trial_goal,Goal1);
goal_2=contains(trial_goal,Goal2);
goal_3=contains(trial_goal,Goal3);

if goal_1==1
    goal_x=goal_x(1); goal_y=goal_y(1);
    goal=1;
elseif goal_2==1
    goal_x=goal_x(2); goal_y=goal_y(2);
    goal=2;
elseif goal_3==1
    goal_x=goal_x(3); goal_y=goal_y(3);
    goal=3;
end

end