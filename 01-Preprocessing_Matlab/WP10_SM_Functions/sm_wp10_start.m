function [start_x,start_y]=sm_wp10_start(start,xmin,xmax,ymin,ymax)
% SM_WP10_START Normalizes start positions in Starmaze WP10 based on min/max
% xy-coordinates. 
%
% Input: 
% start is xy-coordinates of start position.
% xmin,xmax,ymin,ymax are minimum, maximum xy-coordinates.
%
% Returns: start_x,start_y are normalized xy-coordinates. 

[row,col]=size(start);
for r=1:row
    start_x(r,1)=datanorm(start(r,1),xmin,xmax);
    start_y(r,1)= datanorm(start(r,2),ymin,ymax);
end

end