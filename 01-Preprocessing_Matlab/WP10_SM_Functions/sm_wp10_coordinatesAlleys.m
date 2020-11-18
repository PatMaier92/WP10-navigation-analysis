function [alley_zone,rel_alley_zone,entry]=sm_wp10_coordinatesAlleys(x,y,alley_full_x,alley_full_y,lengthX)
% SM_WP10_COORDINATESALLEYS Used for zone analysis. 
% 
% Input: ? 
% 
% Returns: ? 

[row,col]=size(alley_full_x);
 alley_zone=zeros(1,col);
 rel_alley_zone=zeros(1,col);
 entry=zeros(1,col);
 
 for c=1:col
     [in,~]= inpolygon(x,y,alley_full_x(:,c),alley_full_y(:,c));
     coordinates_i_a=numel(x(in));
     alley_zone(1,c)=alley_zone(1,c)+coordinates_i_a; %absolut
     rel_alley_zone(1,c)=alley_zone(1,c)/lengthX; % relativ
     if inpolygon(x(1,1),y(1,1),alley_full_x(:,c),alley_full_y(:,c))
         entry(1,c)=entry(1,c)+1;
     end
     for k=2:lengthX-1
         if inpolygon(x(k),y(k),alley_full_x(:,c),alley_full_y(:,c)) && ~inpolygon(x(k-1),y(k-1),alley_full_x(:,c),alley_full_y(:,c))
             entry(1,c)=entry(1,c)+1;
         end
     end
 end
end
