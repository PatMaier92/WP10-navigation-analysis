function pathExplored=sm_wp10_pathExplored(alley_zone_out, alley_zone_in, rectangle_zone, triangle_zone)
% SM_WP10_PATHEXPLORED Counts how many zones (alleys, triangles, rectanges)
% were entered in Starmaze WP10. Does not count re-entries in the same area. 
%
% Input: 
% alley_zone_out, alley_zone_in, rectangle_zone, triangle_zone have zone
% information from wp10_coordinatesAlleys/wp10_coordinatesPentagon. 
%
% Returns: pathExplored is number of explored zones (integer). 

[row,col]=size(alley_zone_in);
pathExplored=0; % exploration mark
for c=1:col
    if alley_zone_out(1,c) > 0
        pathExplored= pathExplored+1;
    end
    if alley_zone_in(1,c) > 0
        pathExplored= pathExplored+1;
    end
    if rectangle_zone(1,c) > 0
        pathExplored= pathExplored+1;
    end
    if triangle_zone(1,c) > 0
        pathExplored= pathExplored+1;
    end
end

end