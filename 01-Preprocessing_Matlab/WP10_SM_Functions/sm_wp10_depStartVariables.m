function [goal_x_ego, goal_y_ego, x_line, y_line, x_line_chosen, y_line_chosen, x_line_ego, y_line_ego,...
    ideal_path, ideal_path_chosen, ideal_ego_path, ego_alley, ideal_headturn]=sm_wp10_depStartVariables(myGraph,...
    graph_x, graph_y, start, goal, chosen_x, chosen_y,...
    alley_full_x, alley_full_y, rec_x, rec_y, cP_polyshape, polyshape_array)
% SM_WP10_DEPSTARTVARIABLES Function for determining starting point dependent
% variables in Starmaze WP1. Requires Matlab 2021a
%
% Input:
% myGraph is graph representation of all start-goal connections
% graph_x, graph_y are the underlying x-/y-coordinates 
% start, goal are integer identifiers
% chosen_x, chosen_y are final x-/y-coordinates
% alley_full_x, alley_full_y, rec_x, rec_y are x-/y-coordinate vectors in polyshape-ready form (i.e. repeating initial x/y for closed shape).
% cP_polyshape is inner ring as one combined polyshape, polyshape_array is array of all polyshape elements. 
%
% Returns:
% goal_x_ego, goal_y_ego are x-/y-coordinates of the egocentric goal
% x_line, y_line, x_line_ego, y_line_ego, x_line_chose, y_line_chosen are vectors with ideal x-/y-coordinates
% ideal_path, ideal_chosen_path, ideal_ego_path are ideal distance values
% ego_alley is identifier for hypothetical egocentric goal alley
% ideal_headturn is ideal number of head turns. 

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

%% shortest path to egocentric goal with rotation matrix 
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

%% shortest path to chosen goal (only relevant for probe trials)
% find nearest vertex in central polygon for chosen goal 
[vertexid,~,~] = nearestvertex(cP_polyshape,chosen_x,chosen_y);
node_x=cP_polyshape.Vertices(vertexid,1);
node_y=cP_polyshape.Vertices(vertexid,2);

% find graph index of this vertex
[~,node_index]=ismember([node_x node_y],[graph_x' graph_y'],'rows');
[node_path,~]=shortestpath(myGraph,start,node_index);

% extract xy-coordinates and add goal location xy-coordinates 
x_line_all_nodes=[graph_x(node_path) chosen_x];
y_line_all_nodes=[graph_y(node_path) chosen_y];
path_length_all_nodes=sm_wp10_idealPathLength(x_line_all_nodes, y_line_all_nodes);

% check if it is really the shortest path
% try path excluding last node 
x_line_not_last_node=[graph_x(node_path(1:end-1)) chosen_x];
y_line_not_last_node=[graph_y(node_path(1:end-1)) chosen_y];
path_length_without_last_nodes=sm_wp10_idealPathLength(x_line_not_last_node, y_line_not_last_node);

% interpolate data (for last segment of path excluding last node) 
% using 'interparc' function by John D'Errico (Matlab File Exchanger) 
[xi_ch_short,yi_ch_short]=sm_wp10_dataInterpolation(x_line_not_last_node(end-1:end), ...
    y_line_not_last_node(end-1:end), path_length_without_last_nodes);

% check if last segment (of path excluding last node) is valid (i.e., not outside starmaze area)   
validPath=all(isinterior(union(polyshape_array),xi_ch_short,yi_ch_short)); 

% set values 
if path_length_without_last_nodes < path_length_all_nodes && validPath % exclude last node 
    x_line_chosen=x_line_not_last_node;
    y_line_chosen=y_line_not_last_node;
    ideal_path_chosen=path_length_without_last_nodes;
else % keep all nodes 
    x_line_chosen=x_line_all_nodes;
    y_line_chosen=y_line_all_nodes;
    ideal_path_chosen=path_length_all_nodes;
end 

% % test plot
% figure;
% plot(polyshape_array);
% hold on
% plot(x_line_chosen, y_line_chosen, 'r-');
% %plot(xi_ch, yi_ch, 'k-'); 
% plot(chosen_x,chosen_y, 'bo', node_x, node_y, 'bx');
% xlim([0 1]);
% ylim([0 1]);
% hold off

end