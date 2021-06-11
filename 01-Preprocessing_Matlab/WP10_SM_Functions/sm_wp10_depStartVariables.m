function [goal_x_ego, goal_y_ego, x_line, y_line, x_line_chosen, y_line_chosen, x_line_ego, y_line_ego,...
    ideal_path, ideal_path_chosen, ideal_ego_path, ego_alley, ideal_headturn]=sm_wp10_depStartVariables(myGraph,...
    graph_x, graph_y, start, goal, goal_x, goal_y, x_start, y_start, ...
    start_x, start_y, alley_x, alley_y, pentagon_x, pentagon_y, ...
    alley_full_x, alley_full_y, rec_x, rec_y, cP_polyshape)
% SM_WP10_DEPSTARTVARIABLES Function for determining starting point dependent
% variables in Starmaze WP1. Requires Matlab 2021a
%
% Input:
% start, goal are trial identifiers (integer).
% goal_x, goal_y, x_start, y_start are x-/y-coordinates for this trial.
% start_x, start_y, alley_x, alley_y, pentagon_x, pentagon_y 
% are arrays/vectors with general x-/y-coordinates.
% alley_full_x, alley_full_y, rec_x, rec_y, are the same vectors in polyshape-ready form 
% (i.e. repeating first x-/y values for closed shape).
% cP_polyshape is inner ring as one combined polyshape. 
%
% Returns:
% goal_x_ego, goal_y_ego are x-/y-coordinates of the egocentric goal
% x_line, y_line, x_line_ego, y_line_ego are vectors with ideal x-/y-coordinates
% ideal_path, ideal_ego_path are ideal distance values,
% ideal_headturn is ideal head turn number, 
% ego_alley is identifier for egocentric goal alley. 

%% shortest path from original start 

o_start_node=7; 
end_node=size(myGraph.Nodes,1)+1-goal;

[path_nodes,~]=shortestpath(myGraph, o_start_node, end_node);
o_x_line=graph_x(path_nodes); 
o_y_line=graph_y(path_nodes);

% % test plot
% figure; 
% plot(polyshape_array); 
% hold on; 
% % pl = plot(myGraph,'XData',graph_x,'YData',graph_y,'EdgeLabel',myGraph.Edges.Weight);
% pl = plot(myGraph,'XData',graph_x,'YData',graph_y);
% xlim([0 1]);
% ylim([0 1]);
% highlight(pl,path_nodes,'EdgeColor','r');
% hold off; 

%% rotation matrix for egocentric goal and path line
% define x- and y-data for original line
v = [o_x_line ; o_y_line];
% define center of rotation
x_center = 0.5; y_center = 0.5;
% create a matrix
center = repmat([x_center; y_center], 1, length(v));

% define rotation matrix
if start==9
    theta=-360/5*1; % to rotate 72° clockwise
elseif start==1
     theta=-360/5*2; 
elseif start==3
     theta=-360/5*3; 
elseif start==5
     theta=-360/5*4; 
else
     theta=-360/5*0; % no rotation for original and inner starts
end
R = [cosd(theta) -sind(theta); sind(theta) cosd(theta)];

% do rotation
vo = R*(v - center) + center;

% get rotated x- and y-data
r_x_line = vo(1,:);
r_y_line = vo(2,:);

% correct rotated x- and y-data to account for measurement/rotation errors
% i.e., find nearest vertex in actual starmaze boundaries
% otherwise, your egocentric paths might be slightly off/outside the maze. 
[vertexid,~,~] = nearestvertex(cP_polyshape,r_x_line(2:end-1),r_y_line(2:end-1));

% save ego path and goal location
if ~mod(start,2) % dummy for inner starts (even start integer)
    x_line_ego=[999; 998]; y_line_ego=[999; 998]; 
    goal_x_ego=0; goal_y_ego=0; 
else 
    x_line_ego=[r_x_line(1); cP_polyshape.Vertices(vertexid,1); r_x_line(end)]; 
    y_line_ego=[r_y_line(1); cP_polyshape.Vertices(vertexid,2); r_y_line(end)]; 
    goal_x_ego=r_x_line(end); goal_y_ego=r_y_line(end);
end 

% % test plot
% plot(cP_polyshape);
% hold on
% plot(o_x_line, o_y_line, 'k-', x_line_ego, y_line_ego, 'rx', x_center, y_center, 'bo');
% xlim([0 1]);
% ylim([0 1]);
% hold off

% get egocentric final alley integer
ego_alley=0;
[~,col]=size(alley_full_x);
for c=1:col
    if inpolygon(goal_x_ego,goal_y_ego,alley_full_x(:,c),alley_full_y(:,c)) 
        ego_alley=c*2-1;
    elseif inpolygon(goal_x_ego,goal_y_ego,rec_x(:,c),rec_y(:,c))
        ego_alley=c*2;
    end
end

% calculate ideal ego path length value (external function)
ideal_ego_path=sm_wp10_idealPathLength(x_line_ego, y_line_ego);

%% shortest path from actual start 

start_node=start;
end_node=size(myGraph.Nodes,1)+1-goal;

[path_nodes, ideal_path]=shortestpath(myGraph, start_node, end_node);
x_line=graph_x(path_nodes); 
y_line=graph_y(path_nodes);

% % test plot
% figure; 
% plot(polyshape_array); 
% hold on; 
% % pl = plot(myGraph,'XData',graph_x,'YData',graph_y,'EdgeLabel',myGraph.Edges.Weight);
% pl = plot(myGraph,'XData',graph_x,'YData',graph_y);
% xlim([0 1]);
% ylim([0 1]);
% highlight(pl,path_nodes,'EdgeColor','r');
% hold off; 

% calculate ideal number of turns 
ideal_headturn=numel(x_line)-2; % minus start and end points (no head turns there)

%% TBD

ideal_path_chosen=0; 
x_line_chosen=0; 
y_line_chosen=0;

end