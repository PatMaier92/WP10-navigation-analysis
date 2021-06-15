function [cP_x,cP_y,cP,pent_x,pent_y]=sm_wp10_pentagon(alley_x,alley_y,pentagon_x,pentagon_y,xmin,xmax,ymin,ymax)
% SM_WP10_PENTAGON Normalizes xy-coordinates for corners of inner 
% pent of Starmaze WP10 and creates polyshape. 
%
% Input: 
% alley_x,alley_y are xy-coordinates of outer corners. 
% pentagon_x, pentagon_y are xy-coordinates of inner pentagon. 
% xmin,xmax,ymin,ymax are minimum, maximum xy-coordinates for normalization. 
%
% Returns: cP_x,cP_y,cP are polyshapes, pentagon_x,pentagon_y are
% normalized coordinates. 

[~,pentagonCornerNo] = size(pentagon_x);
for pentagon=1:pentagonCornerNo
    pent_x(1,pentagon)=datanorm(pentagon_x(1,pentagon),xmin,xmax);
end

for pentagon=1:pentagonCornerNo
    pent_y(1,pentagon)=datanorm(pentagon_y(1,pentagon),ymin,ymax);
end

cP_x=[alley_x(4,1);alley_x(3,1);alley_x(4,2);alley_x(3,2);alley_x(4,3);alley_x(3,3);...
    alley_x(4,4);alley_x(3,4);alley_x(4,5);alley_x(3,5);alley_x(4,1);NaN;...
    pent_x(1,1);pent_x(1,5);pent_x(1,4);pent_x(1,3);pent_x(1,2);pent_x(1,1)];
cP_y=[alley_y(4,1);alley_y(3,1);alley_y(4,2);alley_y(3,2);alley_y(4,3);alley_y(3,3);...
    alley_y(4,4);alley_y(3,4);alley_y(4,5);alley_y(3,5);alley_y(4,1);NaN;...
    pent_y(1,1);pent_y(1,5);pent_y(1,4);pent_y(1,3);pent_y(1,2);pent_y(1,1)];
cP=polyshape(cP_x,cP_y);

end