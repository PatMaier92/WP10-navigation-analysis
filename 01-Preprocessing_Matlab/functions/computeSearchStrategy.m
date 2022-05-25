function [search_strategy]=computeSearchStrategy(zones_explored,...
    zones_entered, edit_chosen_path)
% computeSearchStrategy Determine search strategy in Starmaze WP10. 
%
% Input: 
% number of zones explored, excl. re-entries (integer)
% number of zones entered, incl. re-entries (integer)
% edit distance for path to chosen goal location (integer) 
% 
% Returns: Search strategy (string): 
% direct, detour, reoriented

% set default to NaN
search_strategy="999";

% evaluate exploration behavior 
if edit_chosen_path==0 % direct = direct path to chosen goal
    search_strategy="direct";
elseif zones_explored==zones_entered % detour = not direct but did not re-enter zones
    search_strategy="detour"; 
else % reorient = did re-enter zones
    search_strategy="reorient";
end 

end