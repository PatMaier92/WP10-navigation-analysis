function pathScore=sm_wp10_pathScore(alley_entry_out, alley_entry_in, rectangle_entry, triangle_entry)
% SM_WP10_PATHSCORE Counts how many zones (alleys, triangles, rectanges)
% were entered in Starmaze WP10. Counts re-entries in the same zones. 
%
% Input: 
% alley_entry_out, alley_entry_in, rectangle_entry, triangle_entry have zone
% information from wp10_coordinatesAlleys/wp10_coordinatesPentagon. 
%
% Returns: pathScore is number of explored zones (integer). 

pathScore = sum(alley_entry_out) + sum(alley_entry_in) + ...
    sum(rectangle_entry) + sum(triangle_entry); 

end