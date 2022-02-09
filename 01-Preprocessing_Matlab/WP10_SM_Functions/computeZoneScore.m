function n=computeZoneScore(alley_out, alley_in, rectangle, triangle)
% computeZoneScore Counts how many zones were entered in Starmaze WP10. 
% Does count re-entries in the same area (score). 
%
% Input: 
% alley_out, alley_in, rectangle, triangle contain number of entries 
% (i.e. not coordinates or data points) in this zone. 
%
% Returns: n is number of (re-)entered zones (integer). 

n = sum(alley_out) + sum(alley_in) + sum(rectangle) + sum(triangle); 

end