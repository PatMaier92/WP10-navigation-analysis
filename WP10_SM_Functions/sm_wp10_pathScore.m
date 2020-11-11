function pathScore=sm_wp10_pathScore(alley_zone_out, alley_zone_in, rectangle_zone, triangle_zone)
% SM_WP10_PATHSCORE Counts how many zones (alleys, triangles, rectanges)
% were entered in Starmaze WP10. Counts re-entries in the same zones. 
%
% Input: 
% alley_zone_out, alley_zone_in, rectangle_zone, triangle_zone have zone
% information from wp10_coordinatesAlleys/wp10_coordinatesPentagon. 
%
% Returns: pathScore is number of explored zones (integer). 

pathScore = sum(alley_zone_out) + sum(alley_zone_in) + ...
    sum(rectangle_zone) + sum(triangle_zone); 

end