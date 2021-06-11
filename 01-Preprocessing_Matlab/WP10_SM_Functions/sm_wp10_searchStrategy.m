function [direct, detour, reoriented, search_strategy_no]=sm_wp10_searchStrategy(direct_path, ...
    path_explored, path_score)
% SM_WP10_SEARCHSTRATEGY Determine search strategies in Starmaze WP10. 
%
% Input: Different variables indicating trial exploration behavior. 
% 
% Returns: Boolean values for different search strategy options and one
% search strategy integer. 
%
% % Search strategy integer: 
% 1 = direct 
% 2 = detour
% 3 = reoriented

% set default to 'false'
direct=0; detour=0; reoriented=0; search_strategy_no=0;

% evaluate exploration behavior 
if path_explored==path_score % did not re-enter same area
    if direct_path==1 % shortest path to correct goal
        search_strategy_no=1;
        direct=1; 
    elseif path_explored <=9 % not correct goal but still shortest path 
        search_strategy_no=1;
        direct=1; 
    else % took detour but did not re-enter same area
        search_strategy_no=2; 
        detour=1; 
    end
else % did re-enter same area
    search_strategy_no=3; 
    reoriented=1;  
end

end