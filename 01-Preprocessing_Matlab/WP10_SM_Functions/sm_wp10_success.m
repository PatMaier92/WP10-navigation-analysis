function [success_allo, success_ego, correct_final_alley_allo, correct_final_alley_ego]=sm_wp10_success(success_crit,...
    final_distance_allo, final_distance_ego, allo_alley, chosen_alley_int, ego_alley)
% SM_WP10_SUCCESS Determines success performance in Starmaze WP10.
%
% Input:
% final_distance_allo, final_distance_ego are distance values (float). 
% allo_alley, ego_alley and chosen_goal_int indicate correct and chosen
% alley integers. 
%
% Returns: success, success_ego, correct_final_alley, correct_final_alley_ego (boolean). 
 
if final_distance_allo <= success_crit
    success_allo=1;
else
    success_allo=0;
end

if final_distance_ego <= success_crit
    success_ego=1;
else
    success_ego=0;
end

if allo_alley==chosen_alley_int
    correct_final_alley_allo=1;
else
    correct_final_alley_allo=0;
end

if ego_alley==chosen_alley_int
    correct_final_alley_ego=1;
else
    correct_final_alley_ego=0;
end
 
end