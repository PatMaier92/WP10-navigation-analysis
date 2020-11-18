function armsExplored=sm_wp10_armExplored(alley_zone)
% SM_WP10_ARMEXPLORED Counts how many arms were entered in Starmaze WP10. 
% Does not count re-entries in the same arms. 
% 
% Input:
% alley_zone is vector with value for each arm, resulting from
% wp10_coordinatesAlley. Values > 0 indicate arm was entered. 
%
% Returns: armsExplored is number of explored arms (integer). 

[row,col]=size(alley_zone);
armsExplored=0; % exploration mark
for c=1:col
    if alley_zone(:,c) > 0
        armsExplored=armsExplored+1;
    end
end

end