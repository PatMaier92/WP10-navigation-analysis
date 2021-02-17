function path_length=sm_wp10_idealPathLength(x_line, y_line)
% SM_WP10_IDEALPATHLENGTH Calculates sum of distance traveled between several points.
%
% Input: 
% o_x_line, o_y_line are vectors with x-/y-coordinate points, i.e. a path. 
%
% Returns:  
% Path length as summation of distance between points (float).

path_length=0; 
for i=1:length(x_line)-1
    path_length=path_length+sm_distance(x_line(i),x_line(i+1), y_line(i),y_line(i+1)); 
end