% UNUSED % 

function exploration=sm_wp10_exploration(alley_zone)
% % SM_WP10_EXPLORATION Counts how many areas in Starmaze WP10
% % have been entered. Does not count re-entries in the same area. 
% % 
% % Input:
% % alley_zone is vector with one value for each arm, resulting from
% % wp10_coordinatesAlley. 
% %
% % Returns: exploration is number of explored areas (integer). 
% 
% [row,col]=size(alley_zone);
% exploration=0; % exploration mark
% for c=1:col
%     if alley_zone(:,c) > 0
%         exploration=exploration+1;
%     end
% end

end