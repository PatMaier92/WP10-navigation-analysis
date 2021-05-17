function [tri_x,tri_y,tri]=sm_wp10_trianglePolyshape(alleyNo,alley_x, alley_y, pentagon_x, pentagon_y)
% SM_WP10_TRIANGLEPOLYSHAPE Creates a polyshape of Starmaze WP10 triangles. 
%
% Input: alleyNo is integer with number of available alleys, alley_x and
% alley_y are arrays with x-/y-coordinates of alley corners, pentagon_y and
% pentagon_y are vectors with x-/y-coordinates of inner pentagon. 
%
% Returns: triangle polyshapes

for alley=1:alleyNo
    tri_x(:,alley)=[alley_x(4,alley);alley_x(3,alley);pentagon_x(1,alley);alley_x(4,alley)];
    tri_y(:,alley)=[alley_y(4,alley);alley_y(3,alley);pentagon_y(1,alley);alley_y(4,alley)];
    tri{alley}=polyshape(tri_x(:,alley),tri_y(:,alley));
end

end
