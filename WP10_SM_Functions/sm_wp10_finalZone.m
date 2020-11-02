function [final_alley, final_pentagon]=sm_wp10_finalZone(x,y,alley_full_x,alley_full_y,cP_x, cP_y)

[row,col]=size(alley_full_x);

for c=1:col
    final_alley(c)=inpolygon(x(end,:),y(end,:),alley_full_x(:,c), alley_full_y(:,c));
end
    final_pentagon=inpolygon(x(end,:),y(end,:),cP_x, cP_y);
end