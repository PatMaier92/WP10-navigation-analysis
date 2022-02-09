function n=computeZoneExploration(alley_out, alley_in, rectangle, triangle)
% computeZoneExploration Counts how many zones were entered in Starmaze WP10. 
% Does not count re-entries in the same area (exploration). 
%
% Input: 
% alley_out, alley_in, rectangle, triangle contain number of data points
% (i.e. coordinates, not entries) in this zone. 
%
% Returns: n is number of explored zones (integer). 

[~,col]=size(alley_out);
n=0; 
for c=1:col
    if alley_out(1,c) > 0
        n= n+1;
    end
    if alley_in(1,c) > 0
        n= n+1;
    end
    if rectangle(1,c) > 0
        n= n+1;
    end
    if triangle(1,c) > 0
        n= n+1;
    end
end

end