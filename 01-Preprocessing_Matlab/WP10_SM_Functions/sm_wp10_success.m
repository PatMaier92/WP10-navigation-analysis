function [success, success_ego, correct_final_alley]=sm_wp10_success(success_crit, final_distance,final_distance_ego, trial_goal, chosen_goal)
% SM_WP10_SUCCESS Determines success performance in Starmaze WP10.
%
% Input:
% final_distance, final_distance_ego are distance values (float). 
% trial_goal and chosen_goal indicate correct and chosen alley(string). 
%
% Returns: success, success_ego and correct_final_alley (boolean). 
 
if final_distance <= success_crit
    success=1;
else
    success=0;
end

if final_distance_ego <= 0.1
    success_ego=1;
else
    success_ego=0;
end

if contains(trial_goal, chosen_goal)
    correct_final_alley=1;
else
    correct_final_alley=0;
end
 
end