function pathScore=sm_wp10_pathScore(alley_zone_out, alley_zone_in, rectangle_zone, triangle_zone)
% SM_WP10_PATHSCORE Takes exploration information and returns a
% path score for Starmaze WP10, indicating how many zones have been crossed. 
%
% Input: 
% alley_zone_out, alley_zone_in, rectangle_zone, triangle_zone are zone
% information
%
% Returns: Path score, indicating how many zones have been crossed
% (integer).

[row,col]=size(alley_zone_in);
pathScore=0;
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