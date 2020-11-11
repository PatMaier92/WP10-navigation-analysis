function armScore=sm_wp10_armScore(alley_entry)
% SM_WP10_ARMSCORE Counts how many arms were entered in Starmaze WP10. 
% Counts re-entries in the same arms. 
% 
% Input:
% alley_entry is vector with value for each arm, resulting from
% wp10_coordinatesAlley. 
%
% Returns: armScore is number of entered arms (integer). 

armScore=sum(alley_entry);

end