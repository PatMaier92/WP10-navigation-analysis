function [abs_zone,rel_zone,entry]=sm_wp10_coordinatesZonesStatic(x,y,zone_full_x,zone_full_y,lengthX)
% SM_WP10_COORDINATESZONESSTATIC Used for zone analysis. 
% 
% Input: 
% x, y are vectors with all recorded data points (trajectory)
% lengthX is number of data points
% zone_full_x, zone_full_y are x-/y-coordinates (boundaries) 
% 
% Returns:
% abs_zone is vector with number of data points
% rel_zone is vector with relative percentage of data points
% entry is vector with number of (re-)entries

[row,col]=size(zone_full_x);
 abs_zone=zeros(1,col);
 rel_zone=zeros(1,col);
 entry=zeros(1,col);
 
 for c=1:col
     [in,~]= inpolygon(x,y,zone_full_x(:,c),zone_full_y(:,c));
     coordinates_i_a=numel(x(in));
     abs_zone(1,c)=abs_zone(1,c)+coordinates_i_a; % absolut
     rel_zone(1,c)=abs_zone(1,c)/lengthX; % relativ
     if inpolygon(x(1,1),y(1,1),zone_full_x(:,c),zone_full_y(:,c))
         entry(1,c)=entry(1,c)+1; % initial entry/start
     end
     for k=2:lengthX-1
         if inpolygon(x(k),y(k),zone_full_x(:,c),zone_full_y(:,c)) && ~inpolygon(x(k-1),y(k-1),zone_full_x(:,c),zone_full_y(:,c))
             entry(1,c)=entry(1,c)+1; % later entries
         end
     end
 end
 
end
