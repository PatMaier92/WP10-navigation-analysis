function BR=sm_b_rot(y1,x1,y2,x2)
% SM_B_ROT Calculates body rotation by subtracting current from previous
% value. 
%
% Input: 
% y1, x1 are values of previous time point.
% y2, x2 are values of current time point.
%
% Returns: BR is body rotation value. 

BR=mod((atan2d(y1,x1) - atan2d(y2,x2)),360); % atand2d for four-quadrant inverse tangent in degrees


                
