% UNUSED %

function [id_ti_sec,id_ti_next,id_ti_same, id_ti_inters_45,id_ti_inters_51,id_ti_inters_12,...
    distance_next,distance_sec, distance_12, distance_45, distance_51]=sm_wp10_idealTimeDistance(goal_x,goal_y, start_x,start_y,ne_x,ne_y,a1_x3, a1_y3,a2_x3,a2_y3, a3_x3,a3_y3, a3_x4,a3_y4,...
    p2_x_ne, p2_y_ne, p4_x_sw,p4_y_sw,p5_x_nw,p5_y_nw,...
    i_12_x,i_12_y,i_45_x,i_45_y,i_51_x,i_51_y)

disp('The function sm_wp10_idealTimeDistance is deactivated'); 

% % Distance
% %%%%%%%%%%%%%% Calculate ideal distances for distance calculations %%%%%%%%
% %%%%%%%%%%%% Feedback-sessions %%%%%%%%%%%%%%%%%%%
% % calculating distance target in next alley
% distance_next=sm_distance(ne_x,a2_x3,ne_y,a2_y3)+sm_distance(a2_x3,a3_x4,a2_y3,a3_y4)+sm_distance(a3_x4,goal_x,a3_y4,goal_y); % for ideal path length/path-accuracy
% % calculating distance target in second alley
% distance_sec=sm_distance(start_x,a1_x3,start_y, a1_y3)+sm_distance(a1_x3,p2_x_ne, a1_y3,p2_y_ne)+sm_distance(p2_x_ne,a3_x4, p2_y_ne, a3_y4)+sm_distance(a3_x4, goal_x, a3_y4, goal_y); % for ideal path length/path-accuracy   
% % calculating distance from intersection 12
% distance_12=sm_distance(i_12_x,p2_x_ne,i_12_y,p2_y_ne)+sm_distance(p2_x_ne,a3_x4,p2_y_ne,a3_y4)+sm_distance(a3_x4,goal_x,a3_y4,goal_y); % for ideal path length/path-accuracy    
% % calculating distance from intersection 45
% distance_45=sm_distance(i_45_x,p4_x_sw,i_45_y,p4_y_sw)+sm_distance(p4_x_sw,a3_x3,p4_y_sw,a3_y3)+sm_distance(a3_x3,goal_x,a3_y3,goal_y); % for ideal path length/path-accuracy               
% % calculating distance from intersection 51
% distance_51=sm_distance(i_51_x,p5_x_nw,i_51_y,p5_y_nw)+sm_distance(p5_x_nw,p4_x_sw,p5_y_nw,p4_y_sw)+sm_distance(p4_x_sw,a3_x3,p4_y_sw,a3_y3)+sm_distance(a3_x3,goal_x,a3_y3,goal_y); % for ideal path length/path-accuracy
% 
% % Time - ideal time to allocentric/ egocentric target %%%%%%%%
% % Attention time is an estimation (ideal player)
% % change, if star maze changed
% % Feedback
% id_ti_sec=17;   id_ti_next=11;  id_ti_same=1;   id_ti_inters_45=8;  id_ti_inters_51=12; id_ti_inters_12=8;

end