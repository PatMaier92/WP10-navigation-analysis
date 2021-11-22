function [chosen_goal_int, chosen_alley_string, chosen_alley_int, ...
    obj_at_chosen_loc]=sm_wp10_chosenGoal(rand_dict, chosen_goal_string, ...
    goal_locs, alley_locs)
% SM_WP10_CHOSENGOALALLEY Stores chosen goal as int and string in new variables.  
%
% Input: 
% rand_dict with randomization info (structure/dictionary)
% chosen_goal_string (string, e.g. "MC")
% goal_locs is ordered array of goals (string)
% alley_locs is ordered array of alley letters (string)
%
% Returns: chosen_goal_int (integer), chosen_alley_string (string), chosen_alley_int (integer), 
% obj_at_chosen_loc correct object at chosen goal location (integer) 

% string
chosen_alley_string=chosen_goal_string(end);

% integer 
chosen_goal_int=find(contains(goal_locs, chosen_goal_string));
chosen_alley_int=find(contains(alley_locs, chosen_alley_string)); 

% correct for false input 
if isempty(chosen_goal_int)
    fprintf('Unknown goal int input %s in chosenGoal.m. Set to 999.\n', chosen_goal_string);
    chosen_goal_int=999;
end

% correct for false input 
if isempty(chosen_alley_int)
    fprintf('Unknown alley int input %s in chosenGoal.m. Set to 999.\n', chosen_alley_string);
    chosen_alley_int=999; 
end

% extract correct object at chosen location 
obj_at_chosen_loc=999; 
fields = fieldnames(rand_dict); 
for i=1:length(fields)
    if string(chosen_goal_string)==string(fields{i})
        key=fields{i};
        temp=char(rand_dict.(key).object);
        obj_at_chosen_loc=double(string(temp(1:2))); 
    end
end 

end
