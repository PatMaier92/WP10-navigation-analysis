function pathScore=sm_wp10_pathScore(alley_zone_out, alley_zone_in, rectangle_zone, triangle_zone)
% SM_WP10_PATHSCORE Counts how many zones (alleys, triangles, rectanges)
% were entered in Starmaze WP10. Does not count re-entries in the same zones. 
%
% Input: 
% alley_zone_out, alley_zone_in, rectangle_zone, triangle_zone have zone
% information from wp10_coordinatesAlleys/wp10_coordinatesPentagon. 
%
% Returns: pathScore is number of explored zones (integer). 

[row,col]=size(alley_zone_in);
pathScore=0; % exploration mark
for c=1:col
    if alley_zone_out(1,c) > 0
        pathScore= pathScore+1;
    end
    if alley_zone_in(1,c) > 0
        pathScore= pathScore+1;
    end
    if rectangle_zone(1,c) > 0
        pathScore= pathScore+1;
    end
    if triangle_zone(1,c) > 0
        pathScore= pathScore+1;
    end
end

end