function [alley_time, pentagon_time, triangle_time, rectangle_time]=sm_wp10_time(time, rel_alley_zone,...
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

% %% Time in Zones            
% % time in alleys 1-5
%         table{p}.trial{k}.zone.Time_a1=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relAlley_1_zone;
%         table{p}.trial{k}.zone.Time_a2=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relAlley_2_zone;
%         table{p}.trial{k}.zone.Time_a3=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relAlley_3_zone;
%         table{p}.trial{k}.zone.Time_a4=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relAlley_4_zone;
%         table{p}.trial{k}.zone.Time_a5=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relAlley_5_zone;       
% % time in inner Structure (Pentagon)
%         table{p}.trial{k}.zone.Time_inPe=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinPe;       
% % time in Triangles 1-5
%         table{p}.trial{k}.zone.Time_inTri_1=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinTri_1;
%         table{p}.trial{k}.zone.Time_inTri_2=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinTri_2;
%         table{p}.trial{k}.zone.Time_inTri_3=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinTri_3;
%         table{p}.trial{k}.zone.Time_inTri_4=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinTri_4;
%         table{p}.trial{k}.zone.Time_inTri_5=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinTri_5;       
% % time in Rectangles 1-5
%         table{p}.trial{k}.zone.Time_inRec_1=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinRec_1;
%         table{p}.trial{k}.zone.Time_inRec_2=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinRec_2;
%         table{p}.trial{k}.zone.Time_inRec_3=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinRec_3;
%         table{p}.trial{k}.zone.Time_inRec_4=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinRec_4;
%         table{p}.trial{k}.zone.Time_inRec_5=table{p}.trial{k}.result.time*table{p}.trial{k}.zone.relinRec_5;  