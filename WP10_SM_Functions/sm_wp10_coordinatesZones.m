function [alley_zone,rel_alley_zone,entry]= sm_wp10_coordinatesZones(x,y,alley_full_x,alley_full_y,lengthX)

[row,col]=size(alley_full_x);
 alley_zone=zeros(1,alleyNo);
 rel_alley_zone=zeros(1,alleyNo);
 entry=zeros(1,alleyNo);
 
 for c=1:row
        [in,~]= inpolygon(x,y,alley_full_x(:,c),alley_full_y(:,c));
        coordinates_i_a=numel(x(in));
        alley_zone(1,c)=alley_zone(1,c)+coordinates_i_a; %absolut
        rel_alley_zone(1,c)=alley_zone(1,c)/lengthX; % relativ
        for k=2:(lengthX-1)
            if inpolygon(x(k),y(k),alley_full_x(:,alley),alley_full_y(:,alley)) && ~inpolygon(x(k-1),y(k-1),alley_full_x(:,alley),alley_full_y(:,alley)) || inpolygon(x(k),y(k),alley_full_x(:,alley),alley_full_y(:,alley)) && ~inpolygon(x(k+1),y(k+1),alley_full_x(:,alley),alley_full_y(:,alley))
                entry(1,alley)=entry(1,alley)+1;
            end
        end
 end
end
