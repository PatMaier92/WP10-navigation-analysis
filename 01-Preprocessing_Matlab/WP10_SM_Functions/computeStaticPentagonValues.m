function [abs_zone,rel_zone,entry,rot_zone]=computeStaticPentagonValues(x,...
    y,r,pentagon_x,pentagon_y)
% computeStaticPentagonValues: Used for pentagon zone analysis. 
% 
% Input: 
% x, y, r are vectors with all recorded data points (x-/y-trajectory and z-rotation)
% pentagon_x, pentagon_y are x-/y-coordinates (boundaries) of inner pentagon 
% 
% Returns: 
% abs_zone is vector with number of data points in inner pentagon
% rel_zone is vector with relative percentage in inner pentagon
% entry is vector with number of (re-)entries in inner pentagon
% rot_zone is vector with rotation in inner pentagon

length_x=length(x); 
entry=0;
rot_zone=0; 

% absolute and relative numbers
[in,~]= inpolygon(x,y,pentagon_x,pentagon_y);
abs_zone=numel(x(in)); % absolut
rel_zone=abs_zone/length_x; % relativ

% rotation
yaw_rotation=r(in); 
for j=1:length(yaw_rotation)-1
    rot_zone=rot_zone+abs(yaw_rotation(j+1)-yaw_rotation(j)); % rotations
end 

% entries
if inpolygon(x(1,1),y(1,1),pentagon_x,pentagon_y) % activate if starts in inner pentagon 
    entry=entry+1; % initial entry/start
end
for k=2:length_x-1
    if inpolygon(x(k),y(k),pentagon_x,pentagon_y) && ~inpolygon(x(k-1),y(k-1),pentagon_x,pentagon_y)
        entry=entry+1; % entries
    end
end

end
