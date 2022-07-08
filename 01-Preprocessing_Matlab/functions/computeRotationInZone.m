function [rotation_degrees, rotation_turns]=computeRotationInZone(index, r)
% computeRotationInZone: Compute rotation in zone. 
% 
% Input: 
% index (boolean vector)
% r are z-coordinates with yaw rotation (float vector) 
% 
% Returns:
% rotation_degrees (float), rotation_turns (float) 

% default
rotation_degrees=0;

% find index(es) of segments in this zone
i_start=find(diff(index) > 0); % start index(es)
i_end=find(diff(index) < 0); % end index(es)
if ~isempty(i_start) || ~isempty(i_end)
    % correct index(es) for missing start or end
    if length(i_start) < length(i_end) % add start
        i_start=[1; i_start];
    elseif length(i_start) > length(i_end)  % add end
        i_end(end+1)=length(index);
    end
%     % compute rotation for each segment
%     for a=1:length(i_start)
    % compute rotation only for initial segment
        a=1;
        r_segment=r(i_start(a):i_end(a));
        for j=2:length(r_segment)
            temp=abs(r_segment(j)-r_segment(j-1));
            if temp > 180 % correct errors due to switch at 0° to 360°
                temp=360-temp;
            end
            rotation_degrees=rotation_degrees+temp;
        end
    % end
end
rotation_turns=rotation_degrees/360;
                
end
