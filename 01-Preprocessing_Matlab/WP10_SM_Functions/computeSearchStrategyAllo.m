function [search_strategy_no]=computeSearchStrategyAllo()
% computeSearchStrategyAllo Determine search strategy 
% in allocentric probe trials in Starmaze WP10. 
%
% Input: 
% 
% Returns: Search strategy integer

% set default to NaN
search_strategy_no=999;

% % evaluate exploration behavior 
% if shortest_path_to_goal || editDistance(sequence,ideal_sequence_chosen_goal)==0 % direct = shortest path to correct goal or shortest path to chosen goal 
%     search_strategy_no=1;
% elseif zones_explored==zones_entered % detour = not shortest path, did not re-enter zones
%     search_strategy_no=2; 
% else % reorient = did re-enter zones
%     search_strategy_no=3;
% end 

end