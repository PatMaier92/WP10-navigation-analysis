function [x_line, y_line, o_x_line, o_y_line, x_line_chosen, y_line_chosen,...
    x_line_ego, y_line_ego, x_line_A, y_line_A, x_line_C, y_line_C,...
    x_line_E, y_line_E, x_line_G, y_line_G, x_line_I, y_line_I,...
    goal_x_ego, goal_y_ego, ego_alley,...
    ideal_path, ideal_path_chosen, ideal_ego_path, ideal_path_A, ...
    ideal_path_C, ideal_path_E, ideal_path_G, ideal_path_I]=computeStartDependentIdealVariables(Graph,...
    graph_x, graph_y, start, goal, chosen_x, chosen_y, goal_x_in_alleys, goal_y_in_alleys,...
    alley_full_x, alley_full_y, rec_x, rec_y, cp_polyshape, polyshape_array)
% computeStartDependentIdealVariables: Function for determining starting point dependent
% variables in Starmaze WP1. Requires Matlab 2021a for shortestPath function.
%
% Input:
% Graph is graph representation of all start-goal connections
% graph_x, graph_y are the underlying x-/y-coordinates 
% start, goal are identifying integers
% chosen_x, chosen_y are final x-/y-coordinates
% alley_full_*, rec_* are x-/y-coordinate vectors in polyshape-ready form (i.e. repeating initial x/y for closed shape)
% cp_polyshape is inner ring as one combined polyshape
% polyshape_array is array of all polyshape elements
%
% Returns:
% goal_x_ego, goal_y_ego are x-/y-coordinates of the egocentric goal
% *x_line*, *y_line* are vectors with ideal x-/y-coordinates
% ideal_path* are ideal distance values
% ego_alley is identifier for hypothetical egocentric goal alley

%% shortest path from original start to goal
o_start_node=7; 
end_node=size(Graph.Nodes,1)+1-goal;
[path_nodes,~]=shortestpath(Graph, o_start_node, end_node);

% path with all nodes
o_x1=graph_x(path_nodes); 
o_y1=graph_y(path_nodes);
length_o1=computePathLength(o_x1, o_y1);

% path with reduced nodes
o_x2=graph_x([path_nodes(1:end-2) path_nodes(end)]); 
o_y2=graph_y([path_nodes(1:end-2) path_nodes(end)]);
length_o2=computePathLength(o_x2, o_y2);
% interpolate last segment (using 'interparc' function by John D'Errico (Matlab File Exchanger)) 
[xi_s2,yi_s2]=interpolateData(o_x2(end-1:end),o_y2(end-1:end),length_o2);
% check if last segment is valid (i.e., not outside starmaze area)  
isValid=all(isinterior(union(polyshape_array),xi_s2,yi_s2));

% determine best option and set values
if length_o2 < length_o1 && isValid 
    o_x_line=o_x2; o_y_line=o_y2; 
else
    o_x_line=o_x1; o_y_line=o_y1; 
end
clear path_nodes o_x1 o_y1 o_x2 o_y2 length* isValid *_s2; 

% % test plot
% figure; plot(polyshape_array); hold on; 
% % pl = plot(Graph,'XData',graph_x,'YData',graph_y,'EdgeLabel',Graph.Edges.Weight);
% pl = plot(Graph,'XData',graph_x,'YData',graph_y);
% highlight(pl,path_nodes,'EdgeColor','r');
% xlim([0 1]); ylim([0 1]); hold off; 

%% shortest path from actual start to egocentric goal (using a rotation matrix) 
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
[vid,~,~] = nearestvertex(cp_polyshape,r_x_line(2:end-1),r_y_line(2:end-1));

% save ego path and goal location
if ~mod(start,2) % dummy for inner starts (even start integer)
    x_line_ego=[999; 998]; y_line_ego=[999; 998]; 
    goal_x_ego=0; goal_y_ego=0; 
else 
    x_line_ego=[r_x_line(1); cp_polyshape.Vertices(vid,1); r_x_line(end)]; 
    y_line_ego=[r_y_line(1); cp_polyshape.Vertices(vid,2); r_y_line(end)]; 
    goal_x_ego=r_x_line(end); goal_y_ego=r_y_line(end);
end 

% % test plot
% figure; plot(cp_polyshape); hold on;
% plot(o_x_line, o_y_line, 'k-', x_line_ego, y_line_ego, 'rx', x_center, y_center, 'bo');
% xlim([0 1]); ylim([0 1]); hold off;

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
ideal_ego_path=computePathLength(x_line_ego, y_line_ego);

%% shortest path from actual start to goal
start_node=start;
end_node=size(Graph.Nodes,1)+1-goal;
[path_nodes,~]=shortestpath(Graph, start_node, end_node);

% path with all nodes
x1=graph_x(path_nodes); 
y1=graph_y(path_nodes);
length_1=computePathLength(x1, y1);

% path with reduced nodes
x2=graph_x([path_nodes(1:end-2) path_nodes(end)]); 
y2=graph_y([path_nodes(1:end-2) path_nodes(end)]);
length_2=computePathLength(x2, y2);
% interpolate last segment (using 'interparc' function by John D'Errico (Matlab File Exchanger)) 
[xi_s2,yi_s2]=interpolateData(x2(end-1:end),y2(end-1:end),length_2);
% check if last segment is valid (i.e., not outside starmaze area)  
isValid=all(isinterior(union(polyshape_array),xi_s2,yi_s2));

% determine best option and set values
if length_2 < length_1 && isValid 
    x_line=x2; y_line=y2; ideal_path=length_2;
else
    x_line=x1; y_line=y1; ideal_path=length_1;
end
clear path_nodes x1 y1 x2 y2 length* isValid *_s2; 

% % test plot
% figure; plot(polyshape_array); hold on; 
% % pl = plot(Graph,'XData',graph_x,'YData',graph_y,'EdgeLabel',Graph.Edges.Weight);
% pl = plot(Graph,'XData',graph_x,'YData',graph_y);
% highlight(pl,path_nodes,'EdgeColor','r');
% xlim([0 1]); ylim([0 1]); hold off; 

%% shortest path from actual start to chosen goal 
[x_line_chosen, y_line_chosen, ideal_path_chosen]=computeShortestPath(cp_polyshape, polyshape_array,...
    Graph, graph_x, graph_y, start, chosen_x, chosen_y);

%% shortest path from actual start to hypothetical goal in all alleys (only for allocentric probe trials from outer starts)
[x_line_A, y_line_A, ideal_path_A]=computeShortestPath(cp_polyshape, polyshape_array,...
    Graph, graph_x, graph_y, start, goal_x_in_alleys(goal,1), goal_y_in_alleys(goal,1));

% central_polygon=cp_polyshape
% full_polyshape=polyshape_array
% graph=Graph
% graph_x
% graph_y
% start
% end_x=goal_x_in_alleys(goal,1)
% end_y=goal_y_in_alleys(goal,1)

[x_line_C, y_line_C, ideal_path_C]=computeShortestPath(cp_polyshape, polyshape_array,...
    Graph, graph_x, graph_y, start, goal_x_in_alleys(goal,2), goal_y_in_alleys(goal,2));

[x_line_E, y_line_E, ideal_path_E]=computeShortestPath(cp_polyshape, polyshape_array,...
    Graph, graph_x, graph_y, start, goal_x_in_alleys(goal,3), goal_y_in_alleys(goal,3));

[x_line_G, y_line_G, ideal_path_G]=computeShortestPath(cp_polyshape, polyshape_array,...
    Graph, graph_x, graph_y, start, goal_x_in_alleys(goal,4), goal_y_in_alleys(goal,4));

[x_line_I, y_line_I, ideal_path_I]=computeShortestPath(cp_polyshape, polyshape_array,...
    Graph, graph_x, graph_y, start, goal_x_in_alleys(goal,5), goal_y_in_alleys(goal,5));

% test plot
figure; plot(polyshape_array); hold on; 
plot(x_line_A, y_line_A, 'k-.',...
    x_line_C, y_line_C, 'r-.',...
    x_line_E, y_line_E, 'b-.',...
    x_line_G, y_line_G, 'g-.',...
    x_line_I, y_line_I, 'y-.',...
    x_line_chosen, y_line_chosen, 'p--');
xlim([0 1]); ylim([0 1]); hold off; 

figure; plot(polyshape_array); hold on; 
plot(x_line_chosen, y_line_chosen, 'p--');
xlim([0 1]); ylim([0 1]); hold off; 

end