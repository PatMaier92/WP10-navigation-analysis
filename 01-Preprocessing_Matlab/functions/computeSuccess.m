function [correct_goal, success_ego, correct_final_alley, correct_final_alley_ego]=computeSuccess(success_crit,...
    final_distance, final_distance_ego, goal_alley, ego_alley, chosen_alley_i)
% computeSuccess Determines success performance in Starmaze WP10.
%
% Input:
% final_distance, final_distance_ego are distance values (float). 
% allo_alley, ego_alley, chosen_goal_i indicate alley integers. 
%
% Returns: success, success_ego, correct_final_alley, correct_final_alley_ego (integer). 
 
if final_distance <= success_crit
    correct_goal=1;
else
    correct_goal=0;
end

if final_distance_ego <= success_crit
    success_ego=1;
else
    success_ego=0;
end

if goal_alley==chosen_alley_i
    correct_final_alley=1;
else
    correct_final_alley=0;
end

if ego_alley==chosen_alley_i && goal_alley~=chosen_alley_i
    correct_final_alley_ego=1;
else
    correct_final_alley_ego=0;
end
 
end