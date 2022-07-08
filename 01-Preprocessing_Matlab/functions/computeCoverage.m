function [abs_coverage, rel_coverage, abs_time_in_zone, index]=computeCoverage(alley_int,...
    x, y, alley_poly, tri_poly, time)
% computeCoverage: Compute absolute and relative coverage in zone. 
% 
% Input: 
% alley_int is zone identifier (integer) 
% x, y are x-/y-coordinates (float) 
% alley_poly, tri_poly are polyshapes
% time is total time (float)
% 
% Returns:
% abs_coverage (integer), rel_coverage (float), 
% abs_time_in_zone(float), index (boolean vector)

% convert alley_int (from [1 3 5 7 9] to [1 2 3 4 5])
alley_int=(alley_int+1)/2;

% get index(es) in zone 
target_poly=union(alley_poly{alley_int}, tri_poly{alley_int});
index=inpolygon(x,y,target_poly.Vertices(:,1),target_poly.Vertices(:,2));
% figure; plot(sm.coord.full_poly); hold on;
% plot(target_poly, 'FaceColor', 'k');
% plot(x(index), y(index), 'rx');

% compute values
abs_coverage=numel(x(index));
rel_coverage=abs_coverage/length(x);
abs_time_in_zone=time*rel_coverage;
                
end
