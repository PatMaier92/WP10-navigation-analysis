% function [avg_distance, total_distance]=computePathDistance(xi_al, yi_al, ...
%     x, y, final_distance, add_fd)
% % computePathDistance Calculates distance between actual path and ideal path,
% % i.e. a path error.
% %
% % Input: 
% % xi_al, yi_al, x, y are vectors with x-/y-coordinate points (float). 
% % final_distance (float). 
% % add_fd (boolean) indicates if final_distance should be added.
% %
% % Returns:  
% % avg_distance (float) and total_distance (float).
% 
% % euclidian distance to nearest neighbour on interpolated ideal path
% [~,distance] = dsearchn([xi_al, yi_al],[x, y]); 
% % add final distance as last data point
% if add_fd==true 
%     distance = [distance; final_distance];
% end 
% 
% % average
% avg_distance=mean(distance);
% % total 
% total_distance=sum(distance);