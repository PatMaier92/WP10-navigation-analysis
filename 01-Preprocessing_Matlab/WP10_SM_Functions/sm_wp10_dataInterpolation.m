function [xi_al, yi_al, xi_eg, yi_eg]=sm_wp10_dataInterpolation(x_line,...
    y_line, ideal_distance, x_line_ego, y_line_ego, ideal_distance_ego)
% SM_WP10_DATAINTERPOLATION Interpolating data for Starmaze WP10 depending
% on path line vectors. This method takes into account distance values 
% to create equally spaced interpolated values. 
% Using the 'interparc' function by John D'Errico (Matlab File Exchanger). 
%
% Input:
% x_line,y_line,x_line_ego,y_line_ego are vectors with x-/y-coordinates
% ideal_distance, ideal_distance_ego are ideal path distance values
%
% Returns:
% xi_al,yi_al,xi_eg,yi_eg are vectors with interpolated x-/y-coordinates

% interpolate
crit=round(ideal_distance*1000);
al = interparc(crit,x_line,y_line,'linear');
xi_al=al(:,1); yi_al=al(:,2);

crit_ego=round(ideal_distance_ego*1000);
eg = interparc(crit_ego,x_line_ego,y_line_ego,'linear');
xi_eg=eg(:,1); yi_eg=eg(:,2);

end