function [xmin,xmax,ymin,ymax]=sm_wp10_MinMaxValues(values)
% SM_WP10_MINMAXVALUES Takes Starmaze WP10 minimum and maximum
% xy-coordinates and returns them as single coordinate variables. 
%
% Input: 
% values are an array which is read-in from a csv. file. 
%
% Returns: xmin,xmax,ymin,ymax are minimum and maximum coordinate
% variables.

xmin=values(1,1);
xmax=values(2,1);

ymin=values(1,2);
ymax=values(2,2);

end