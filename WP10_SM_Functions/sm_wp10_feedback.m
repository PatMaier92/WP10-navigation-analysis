function [fb]=sm_wp10_feedback(feedback)
% SM_WP10_FEEDBACK Returns integer for feedback type in Starmaze WP10.
% 
% Input: 
% feedback is string true/false.
%
% Returns feedback integer (0/1). 

s2='true';
fb=int8(strcmp(feedback,s2));

end