function [rec_x,rec_y,rec]=sm_wp10_rectPolyshape(alleyNo,alley_x, alley_y, pentagon_x, pentagon_y)
% SM_WP10_RECTPOLYSHAPE Creates a polyshape of Starmaze WP10 rectangles. 
%
% Input: ?
%
% Returns: ?

for alley=1:alleyNo
    if alley==alleyNo
        x=-alleyNo;
    else
        x=0;
    end
    rec_x(:,alley)=[alley_x(3,alley);alley_x(4,alley+1+x);pentagon_x(1,alley+1+x);pentagon_x(1,alley);alley_x(3,alley)];
    rec_y(:,alley)=[alley_y(3,alley);alley_y(4,alley+1+x);pentagon_y(1,alley+1+x);pentagon_y(1,alley);alley_y(3,alley)];
    rec{alley}=polyshape(rec_x(:,alley),rec_y(:,alley));
end

end