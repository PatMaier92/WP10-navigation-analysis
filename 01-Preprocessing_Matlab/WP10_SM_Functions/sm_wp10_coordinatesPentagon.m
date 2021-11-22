function [pentagon_zone,rel_pentagon_zone,pentagon_entry,pentagon_rot]=sm_wp10_coordinatesPentagon(x,...
    y,r,pentagon_x,pentagon_y)
% SM_WP10_COORDINATESPENTAGON Used for zone analysis. 
% 
% Input: 
% x, y, r are vectors with all recorded data points (x-/y-trajectory and z-rotation)
% pentagon_x, pentagon_y are x-/y-coordinates (boundaries) of inner pentagon 
% 
% Returns: 
% pentagon_zone is vector with number of data points in inner pentagon
% rel_pentagon_zone is vector with relative percentage in inner pentagon
% pentagon_entry is vector with number of (re-)entries in inner pentagon

lengthX=length(x); 
pentagon_entry=0;
pentagon_rot=0; 

[in,~]= inpolygon(x,y,pentagon_x,pentagon_y);
coordinates_i_a=numel(x(in));
pentagon_zone=coordinates_i_a; % absolut
rel_pentagon_zone=pentagon_zone/lengthX; % relativ
head_rotations=r(in); 
for j=1:length(head_rotations)-1
    pentagon_rot=pentagon_rot+abs(head_rotations(j+1)-head_rotations(j)); % rotations
end 
if inpolygon(x(1,1),y(1,1),pentagon_x,pentagon_y) % activate if starts in inner pentagon 
    pentagon_entry=pentagon_entry+1; % initial entry/start
end
for k=2:lengthX-1
    if inpolygon(x(k),y(k),pentagon_x,pentagon_y) && ~inpolygon(x(k-1),y(k-1),pentagon_x,pentagon_y)
        pentagon_entry=pentagon_entry+1; % entries
    end
end

end
