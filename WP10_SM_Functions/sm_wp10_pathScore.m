function pathScore=sm_wp10_pathScore(alley_zone_out,alley_zone_in, rectangle_zone, triangle_zone)

[row,col]=size(alley_zone_in);
% determine path score, determine how many zones have been crossed
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