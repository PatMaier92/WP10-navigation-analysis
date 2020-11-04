function [final_alley, final_pentagon]=sm_wp10_finalZone(x,y,alley_full_x,alley_full_y,cP_x, cP_y)
% SM_WP10_FINALZONE Returns information on final position area in Starmaze
% WP10. 
%
% Input: 
% x,y,alley_full_x,alley_full_y,cP_x, cP_y are geometrical information
%
% Returns: final_alley and final_pentagon ?

[row,col]=size(alley_full_x);
for c=1:col
    final_alley(c)=inpolygon(x(end,:),y(end,:),alley_full_x(:,c), alley_full_y(:,c));
end
final_pentagon=inpolygon(x(end,:),y(end,:),cP_x, cP_y);

end