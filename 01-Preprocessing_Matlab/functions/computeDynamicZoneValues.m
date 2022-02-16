function [entry_mat]=computeDynamicZoneValues(x,y,zone_full_x,zone_full_y)
% computeDynamicZoneValues: Used for dynamic zone analysis. 
% 
% Input: 
% x, y are vectors with all recorded data points (trajectory)
% zone_full_x, zone_full_y are x-/y-coordinates (boundaries) 
% 
% Returns:
% entry_mat is matrix with sequence of number of re-entries in zones. 

length_x = length(x);
[~,col]=size(zone_full_x);
entry_mat=zeros(length_x,col);

for c=1:col
    if inpolygon(x(1,1),y(1,1),zone_full_x(:,c),zone_full_y(:,c))
        entry_mat(1:end,c)=1; % initial entry/start
    end
    for k=2:length_x-1
        if inpolygon(x(k),y(k),zone_full_x(:,c),zone_full_y(:,c)) && ~inpolygon(x(k-1),y(k-1),zone_full_x(:,c),zone_full_y(:,c))
            entry_mat(k:end,c)=entry_mat(k,c)+1; % later entries
        end
    end
end

end
