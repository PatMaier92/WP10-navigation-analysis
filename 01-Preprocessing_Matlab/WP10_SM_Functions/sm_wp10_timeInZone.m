function [alley_time, pentagon_time, triangle_time, rectangle_time]=sm_wp10_timeInZone(time, rel_alley_zone,...
    rel_pentagon_zone, rel_triangle_zone, rel_rectangle_zone)
% SM_WP10_TIME Returns time spent in different zones of Starmaze WP10. 
% 
% Input: 
% time is total trial time.
% rel_alley_zone, rel_pentagon_zone, rel_triangle_zone, rel_rectangle_zone ?
%
% Returns: alley_time, pentagon_time, triangle_time, rectangle_time is time
% spent in different zones.

[row,col]=size(rel_alley_zone);
for c=1:col
    alley_time(c)=time*rel_alley_zone(c);
    triangle_time(c)=time*rel_triangle_zone(c);
    rectangle_time(c)=time*rel_rectangle_zone(c);
end
pentagon_time=time*rel_pentagon_zone;

end