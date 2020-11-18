function [chosen_alley_int,chosen_goal_string]=sm_wp10_chosenGoalAlley(chosen_goal)
% SM_WP10_CHOSENGOALALLEY Stores chosen alley in new variables.  
%
% Input: 
% chosen_goal indicates chosen goal alley (e.g. MC)
%
% Returns: chosen_alley_int (integer)
% 1 = MA, 3 = MC, 5 = ME (no goal), 7 = MG (no goal), 9 = MI, 0 = Others
% and copy of chosen_alley (string)

% string
chosen_goal_string=chosen_goal;

% integer
if chosen_goal=='MA'
    chosen_alley_int=1; 
elseif chosen_goal=='MC'
    chosen_alley_int=3;
elseif chosen_goal=='ME'
    chosen_alley_int=5;
elseif chosen_goal=='MG'
    chosen_alley_int=7;
elseif chosen_goal=='MI'
    chosen_alley_int=9;
else
    chosen_alley_int=0;
end

end
