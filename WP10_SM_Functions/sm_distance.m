% Starmaze 
% variables functions

% function sm_distance
%  calculating distance

function D= sm_distance(x1,x2,y1,y2)
% calculates the distance traveled from the beginning to the end of the
% trial
D=sqrt((x2-x1)^2+(y2-y1)^2); 