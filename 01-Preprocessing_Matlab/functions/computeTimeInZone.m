function [alley_time, pentagon_time, tri_time, rect_time]=computeTimeInZone(time, rel_alley,...
    rel_pentagon, rel_tri, rel_rect)
% computeTimeInZone: Returns time spent in different zones of Starmaze WP10. 
% 
% Input: 
% time is total trial time.
% rel_alley, rel_pentagon, rel_tri, rel_rect are percentage of presence in
% zones.
%
% Returns: 
% time spend in different zones: alleys, pentagon, triangles, rectangles. 

[~,col]=size(rel_alley);
alley_time=zeros(1,col);
tri_time=zeros(1,col);
rect_time=zeros(1,col);

for c=1:col
    alley_time(c)=time*rel_alley(c);
    tri_time(c)=time*rel_tri(c);
    rect_time(c)=time*rel_rect(c);
end

pentagon_time=time*rel_pentagon;

end