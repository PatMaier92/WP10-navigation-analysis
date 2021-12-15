function [chosen_goal_int, chosen_alley_str, chosen_alley_int, ...
    obj_at_chosen_loc]=sm_wp10_chosenGoal(rand_dict, chosen_goal_str, ...
    sm_coord, xend, yend)
% SM_WP10_CHOSENGOALALLEY Returns chosen goal location as integer and string.  
%
% Input: 
% rand_dict with randomization info (structure/dictionary)
% chosen_goal_string (string), only relevant for "Timeout". The other chosen_goal_string values 
%   are error-prone for triangle/intersections and therefore recalculated by this function. 
% sm_coord contains polyshapes, string array of goal names, string array of alley names 
% xend, yend are chosen final x-/y-coordinates
%
% Returns: 
% chosen_goal_int (integer), chosen_alley_str (string), 
% chosen_alley_int (integer), obj_at_chosen_loc (integer) 


if chosen_goal_str == "Timeout"
    chosen_alley_int = 999; 
    chosen_alley_str = "Timeout"; 
    chosen_goal_int = 999; 
    obj_at_chosen_loc = 999;  
    disp('Timeout in chosenGoal.m. Set to 999.\n');
else 
    for c=1:5
        if inpolygon(xend, yend, sm_coord.alley_polyshape{c}.Vertices(:,1), sm_coord.alley_polyshape{c}.Vertices(:,2)) ...
                || inpolygon(xend, yend, sm_coord.tri{c}.Vertices(:,1), sm_coord.tri{c}.Vertices(:,2)) % chosen location in alley or triangle (outer arm)
            % chosen alley integer
            chosen_alley_int = c*2-1;
            break; 
        elseif inpolygon(xend, yend, sm_coord.rec{c}.Vertices(:,1), sm_coord.rec{c}.Vertices(:,2)) % chosen locaiton in rectangle (inner arm)
             % chosen alley integer
            chosen_alley_int = c*2;
            break;
        end 
    end 
    
    % chosen alley string
    chosen_alley_str = sm_coord.alley_locs(chosen_alley_int);
    
    % chosen goal string
    chosen_goal_int = find(contains(sm_coord.goal_locs, chosen_alley_str)); 
    if isempty(chosen_goal_int)
        chosen_goal_int = 999;
        fprintf('Unknown goal int input %s in chosenGoal.m. Set to 999.\n', chosen_goal_str);
    end 

    % correct object at chosen location 
    obj_at_chosen_loc=999; 
    fields = fieldnames(rand_dict); 
    for i=1:length(fields)
        if strcat("M",chosen_alley_str) == string(fields{i})
            key = fields{i};
            temp = char(rand_dict.(key).object);
            obj_at_chosen_loc = double(string(temp(1:2))); 
        end
    end 
end 

end
