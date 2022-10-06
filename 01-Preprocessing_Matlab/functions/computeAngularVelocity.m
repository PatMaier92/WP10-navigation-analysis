function [IdPhi]=computeAngularVelocity(r)
% computeAngularVelocity: Calculates integrated absolute angular 
% velocity in radians (IdPhi) as mean of absolute change
% in yaw rotation (first derivative).
%
% Input: 
% r is vector with unwrapped (!) z-coordinates with yaw rotation (float) 
% 
% Returns:
% IdPhi as mean integrated absolute angular velocity (float)

rotation_vector_rad=deg2rad(r); % convert r (in degrees) back to radians 
IdPhi=mean(abs(diff(rotation_vector_rad)));

% additional code from Deetje (windowing approach)
% 
% nSamples = 10;
% vector = NaN(1,nSamples);
% for Wi = 1:nSamples
%     vector(Wi) = mean(rotation_vector_rad(round((Wi-1)*...
%         numel(rotation_vector_rad)/nSamples+1):floor(Wi*numel(rotation_vector_rad)/nSamples)));
% end
% 
% iPhi = vector;
             
end
