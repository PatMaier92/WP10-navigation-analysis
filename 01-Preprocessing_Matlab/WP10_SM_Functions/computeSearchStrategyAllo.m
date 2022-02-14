function [search_strategy_no]=computeSearchStrategyAllo(shortest_path_to_goal, correct_goal, correct_ego_goal, ...
    zones_explored, zones_entered, sequence, ideal_sequence_egocentric, ideal_sequence_from_origin)
% computeSearchStrategyAllo Determine search strategy 
% in allocentric probe trials in Starmaze WP10. 
%
% Input: 
% shortest path to goal, correct goal, correct ego goal(boolean)
% number of zones explored and entered (integer)
% sequence, ideal egocentric sequence, ideal sequence from origin to goal (string)
% 
% Returns: Search strategy integer
% 1=direct allocentric 2=detour allocentric
% 3=direct egocentric 4=detour egocentric 
% 5=back to start alley and then to goal 
% 0=unclassified

% set default to unclassified 
search_strategy_no=0; 

% evaluate exploration behavior
if shortest_path_to_goal % 1=direct allocentric
    search_strategy_no=1;
elseif zones_explored==zones_entered && correct_goal % 2=detour allocentric 
    search_strategy_no=2;
elseif editDistance(sequence,ideal_sequence_egocentric)==0 % 3=direct egocentric 
    search_strategy_no=3;
elseif zones_explored==zones_entered && correct_ego_goal % 4=detour egocentric 
    search_strategy_no=4; 
elseif editDistance(sequence(strfind(seq_10, 'G'):end),ideal_sequence_from_origin)==0 % back to start alley and then to goal 
    search_strategy_no=5;
end

end