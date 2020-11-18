function [xi_al,yi_al,xi_eg,yi_eg]=sm_wp10_dataInterpolation(start_x,start_y,x_line, y_line,...
    x_line_ego, y_line_ego, goal_x,goal_y,e_target_x, e_target_y)
% SM_WP10_DATAINTERPOLATION Interpolating data for Starmaze WP10 depending
% of starting position.
%
% Input: ?
%
% Returns: ? 

% interpolate data
xl_al = x_line; yl_al = y_line;

% interpolate data depending on start-positions 
if start_x < goal_x
    xi_al = start_x:0.0001:goal_x;
else
    xi_al = goal_x:0.0001:start_x;
end

if start_x < e_target_x
    xi_eg = start_x:0.0001:e_target_x;
else
    xi_eg = e_target_x:0.0001:start_x;
end
           
% xi_eg = start_x:0.0001:e_target_x;
xi_al = transpose(xi_al);
xi_eg = transpose(xi_eg);

% linear interpolation of ideal paths to allocentric and egocentric target
yi_al = interp1(xl_al,yl_al,xi_al,'linear');
yi_eg = interp1(x_line_ego,y_line_ego,xi_eg,'linear');
yi_al = transpose(yi_al);
% yi_eg = transpose(yi_eg);

end