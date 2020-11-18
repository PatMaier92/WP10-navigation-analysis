function [pentagon_zone,rel_pentagon_zone,pentagon_entry]=sm_wp10_coordinatesPentagon(x,y,pentagon_x,pentagon_y,lengthX)
% SM_WP10_COORDINATESPENTAGON Used for zone analysis. 
% 
% Input: ? 
% 
% Returns: ? 

pentagon_entry=0;
pentagon_zone=0;

[in,~]= inpolygon(x,y,pentagon_x,pentagon_y);
coordinates_i_a=numel(x(in));
pentagon_zone=pentagon_zone+coordinates_i_a; %absolut
rel_pentagon_zone=pentagon_zone/lengthX; % relativ
for k=2:lengthX-1
    if inpolygon(x(k),y(k),pentagon_x,pentagon_y) && ~inpolygon(x(k-1),y(k-1),pentagon_x,pentagon_y)
        pentagon_entry=pentagon_entry+1;
    elseif inpolygon(x(1,1),y(1,1),pentagon_x,pentagon_y)
        pentagon_entry=pentagon_entry+1;
    end
end

end
