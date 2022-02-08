function sum_rot=computeXYRotation(unique_x, unique_y)
% computeXYRotation Calculates rotation due to movement 
% trajectory as indicated by change in x-/y-coordinates. This differs from 
% yaw rotation (z-axis) on the spot.  
%
% Input: 
% unique_x, unique_y is vector with unique x-/y-coordinates. 
%
% Returns: sum_rot is sum of absolute x-/y-related rotation.
% 
% Example: 
% unique_y=[1 6 4]; unique_x=[5 3 1]; 
% plot(unique_x,unique_y,'bx'); xlim([0,10]); ylim([0,10]);

rot=zeros(1,length(unique_x)-2); 
for i=2:length(unique_x)-1
    % norm the previous and subsequent values by using the middle values
    % (i.e. set 'coordinate system' to middle values) 
    n1=([unique_x(i+1),unique_y(i+1)]-[unique_x(i),unique_y(i)])/norm([unique_x(i+1),unique_y(i+1)]-[unique_x(i),unique_y(i)]);
    n2=([unique_x(i-1),unique_y(i-1)]-[unique_x(i),unique_y(i)])/norm([unique_x(i-1),unique_y(i-1)]-[unique_x(i),unique_y(i)]);

    % calculate the rotation
    % atand2d() is four-quadrant inverse tangent in degrees 
    % note: mathematically equal to 180-acosd(dot(n1, n2)))
    % norm() results in absolute values (left/right rotation treated equal)
    % subtract from 180 to get outer angle (change in trajectory)
    rot(i-1) = 180-atan2d(norm(det([n1; n2])), dot(n1, n2)); 
end

sum_rot=sum(rot);              
