function [search_strategy_no]=computeSearchStrategy(shortest_path_to_goal, ...
    zones_explored, zones_entered, sequence, ideal_sequence_chosen_goal)
% computeSearchStrategy Determine search strategy in Starmaze WP10. 
%
% Input: 
% shortest path to goal (boolean)
% number of zones explored and entered (integer)
% sequence and ideal sequece to chosen goal (string)
% 
% Returns: Search strategy integer: 
% 1 = direct 
% 2 = detour
% 3 = reoriented

% set default to NaN
search_strategy_no=999;

% evaluate exploration behavior 
if shortest_path_to_goal || editDistance(sequence,ideal_sequence_chosen_goal)==0 % direct = shortest path to correct goal or shortest path to chosen goal 
    search_strategy_no=1;
elseif zones_explored==zones_entered % detour = not shortest path, did not re-enter zones
    search_strategy_no=2; 
else % reorient = did re-enter zones
    search_strategy_no=3;
end 

end