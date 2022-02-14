function [seq]=computeZoneSequence(x,y,zone_alley_x,zone_alley_y,...
    zone_alley_out_x,zone_alley_out_y,zone_alley_in_x,zone_alley_in_y,...
    zone_rect_x,zone_rect_y,zone_tri_x,zone_tri_y,mode)
% computeZoneSequence Used for dynamic zone analysis. Returns a path
% sequence of zone entries (string). 
% 
% Input: 
% x, y are x-/y-coordinates (numeric vector)
% zone_*_x, zone_*_y are x-/y-coordinates (numeric matrix). 
% mode is either 10 zones or 20 zones (integer).
% 
% Returns:
% seq is path sequence of zone entries (string). 

seq=[];

% check mode 
if ~(mode==10 || mode==20)
    disp('Unknown mode in computeZoneSequence. Please enter valid number (mode=10 or mode=20).');
    return;
end 

% code for path sequence 
code_alley     = ['A','C','E','G','I'];
code_alley_in  = ['a','c','e','g','i'];
code_rect      = ['B','D','F','H','J'];
code_tri       = ['1','2','3','4','5'];

% compute path sequence 
[~,col]=size(zone_alley_x);
for i=1:length(x)
    % rectangles
    for c=1:col
        if evaluate_zone(i, c, zone_rect_x, zone_rect_y)
            add_zone(c, code_rect); continue;
        end
    end
    % plus alleys for mode 10 zones
    if mode==10
        for c=1:col
            if evaluate_zone(i, c, zone_alley_x, zone_alley_y) 
                add_zone(c, code_alley); continue;
            end
        end
    % plus outer alleys, inner alleys and triangles for mode 20 zones
    elseif mode==20
        for c=1:col
            if evaluate_zone(i, c, zone_alley_out_x, zone_alley_out_y) 
                add_zone(c, code_alley); continue;
            end
        end
        for c=1:col
            if evaluate_zone(i, c, zone_alley_in_x, zone_alley_in_y) 
                add_zone(c, code_alley_in); continue;
            end
        end
        % triangles
        for c=1:col
            if evaluate_zone(i, c, zone_tri_x, zone_tri_y)
                add_zone(c, code_tri); continue;
            end
        end
    end
end

    % helper function for evaluating if x/y are in zone
    function bool=evaluate_zone(i, c, zone_x, zone_y)
        bool=inpolygon(x(i),y(i),zone_x(:,c),zone_y(:,c)) && ...
            (i==1 || ~inpolygon(x(i-1),y(i-1),zone_x(:,c),zone_y(:,c)));
    end 

    % helper function for adding code info to sequence 
    function add_zone(c, code)
        seq = [seq, code(c)];
    end

end
