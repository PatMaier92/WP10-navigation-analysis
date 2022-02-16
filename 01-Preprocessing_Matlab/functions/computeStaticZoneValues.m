function [abs_zone,rel_zone,entry,rot_zone]=computeStaticZoneValues(x,y,r,zone_full_x,zone_full_y)
% computeStaticZoneValues: Used for static zone analysis. 
% 
% Input: 
% x, y, r are vectors with all recorded data points (x-/y-trajectory and z-rotation)
% zone_full_x, zone_full_y are x-/y-coordinates (boundaries)
% 
% Returns:
% abs_zone is vector with number of data points
% rel_zone is vector with relative percentage of data points
% entry is vector with number of (re-)entries
% rot_zone is vector with rotation data 

lengthX=length(x);
[~,col]=size(zone_full_x);
abs_zone=zeros(1,col);
rel_zone=zeros(1,col);
entry=zeros(1,col);
rot_zone=zeros(1,col);

for c=1:col
    [in,~]= inpolygon(x,y,zone_full_x(:,c),zone_full_y(:,c));
    coordinates_i_a=numel(x(in));
    abs_zone(1,c)=abs_zone(1,c)+coordinates_i_a; % absolute value
    rel_zone(1,c)=abs_zone(1,c)/lengthX; % relative value
    % rotations
    ind_1=find(diff(in) > 0); % start index rotation
    ind_2=find(diff(in) < 0); % end index rotation
    if ~isempty(ind_1) || ~isempty(ind_2)
        if length(ind_1) < length(ind_2) % unequal length: start missing
            ind_1=[1; ind_1];
        elseif length(ind_1) > length(ind_2)  % unequal length: end missing
            ind_2(end+1)=length(in);
        end
        for a=1:length(ind_1) % evaluate rotations per segment
            head_rotations=r(ind_1(a):ind_2(a));
            for j=1:(length(head_rotations)-1)
                rot_zone(1,c)=rot_zone(1,c)+abs(head_rotations(j+1)-head_rotations(j)); % rotation value
            end
        end
    end
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
