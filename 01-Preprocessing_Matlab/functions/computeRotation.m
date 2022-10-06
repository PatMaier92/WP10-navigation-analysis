function [rotation_degrees, rotation_turns]=computeRotation(r)
% computeRotation: Calculates total rotation in degrees as sum of
% absolute change in yaw rotation (first derivative). This value includes 
% rotation due to x-/y-trajectory (i.e. left-forward movement).
%
% Input: 
% r is vector with unwrapped (!) z-coordinates with yaw rotation (float) 
% 
% Returns:
% rotation_degrees (float), rotation_turns (float) 

rotation_degrees=sum(abs(diff(r)));
rotation_turns=rotation_degrees/360;
                
end
