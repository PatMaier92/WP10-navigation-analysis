function [success, success_ego, correct_final_alley]=sm_wp10_success(final_distance,final_distance_ego, trial_goal, chosen_goal)

if final_distance <= 0.1
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