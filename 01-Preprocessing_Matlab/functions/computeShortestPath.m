function [x_line, y_line, ideal_path]=computeShortestPath(central_polygon, ...
    full_polyshape, graph, graph_x, graph_y, start, end_x, end_y)
% computeShortestPath: Helper function for computing the shortest path
% between any two locations in the WP10 Starmaze environment. 
% 
% Input: 
% central_polygon is inner ring as one combined polyshape
% full_polyshape is full combined polyshape
% graph is graph representation of all start-goal connections
% graph_x, graph_y are the underlying x-/y-coordinates 
% start is identifying integer for start in graph
% end_x, end_y are x-/y-coordinates of final path element 
% 
% Returns: x-/y-coordinates for vertices of shortest path (x_line, y_line) 
% and the ideal length of this path. 

% find nearest vertices for chosen goal in central polygon 
% nearest
[vid,~,~] = nearestvertex(central_polygon,end_x,end_y);
node_x1=central_polygon.Vertices(vid,1);
node_y1=central_polygon.Vertices(vid,2);
% second nearest 
central_polygon.Vertices(vid,:)=[]; % remove nearest vertex and check again
[vid,~,~] = nearestvertex(central_polygon,end_x,end_y);
node_x2=central_polygon.Vertices(vid,1);
node_y2=central_polygon.Vertices(vid,2); clear vid central_polygon;  

% find graph indices of vertices
% for nearest
[~,node_index]=ismember([node_x1 node_y1],[graph_x' graph_y'],'rows');
[n_path1,~]=shortestpath(graph,start,node_index);
% for second nearest
[~,node_index2]=ismember([node_x2 node_y2],[graph_x' graph_y'],'rows');
[n_path2,~]=shortestpath(graph,start,node_index2); clear node*; 

% create four path options with added goal location xy-coordinates
% 1a) all nodes to nearest vertex
x_p1_all=[graph_x(n_path1) end_x]; 
y_p1_all=[graph_y(n_path1) end_y]; 
length_p1_all=computePathLength(x_p1_all, y_p1_all);

% 1b) reduced nodes to nearest vertex
x_p1_reduced=[graph_x(n_path1(1:end-1)) end_x];
y_p1_reduced=[graph_y(n_path1(1:end-1)) end_y];
length_p1_reduced=computePathLength(x_p1_reduced, y_p1_reduced);
% interpolate last segment (using 'interparc' function by John D'Errico (Matlab File Exchanger)) 
[xi_s1,yi_s1]=interpolateData(x_p1_reduced(end-1:end),y_p1_reduced(end-1:end),length_p1_reduced);
% check if last segment is valid (i.e., not outside starmaze area)  
p1Valid=all(isinterior(union(full_polyshape),xi_s1,yi_s1)); clear *_s1; 

% 2a) all nodes to second nearest vertex
x_p2_all=[graph_x(n_path2) end_x]; 
y_p2_all=[graph_y(n_path2) end_y]; 
length_p2_all=computePathLength(x_p2_all, y_p2_all);

% 2b) reduced nodes to second nearest vertex
x_p2_reduced=[graph_x(n_path2(1:end-1)) end_x];
y_p2_reduced=[graph_y(n_path2(1:end-1)) end_y];
length_p2_reduced=computePathLength(x_p2_reduced, y_p2_reduced);
% interpolate last segment (using 'interparc' function by John D'Errico (Matlab File Exchanger)) 
[xi_s2,yi_s2]=interpolateData(x_p2_reduced(end-1:end),y_p2_reduced(end-1:end),length_p2_reduced);
% check if last segment is valid (i.e., not outside starmaze area)  
p2Valid=all(isinterior(union(full_polyshape),xi_s2,yi_s2)); clear *_s2; 

% determine best option and set values
% compare 1a) and 1b) 
if length_p1_reduced < length_p1_all && p1Valid 
    x_line_p1=x_p1_reduced; y_line_p1=y_p1_reduced; length_p1=length_p1_reduced;
else
    x_line_p1=x_p1_all; y_line_p1=y_p1_all; length_p1=length_p1_all;
end
% compare 2a) and 2b) 
if length_p2_reduced < length_p2_all && p2Valid 
    x_line_p2=x_p2_reduced; y_line_p2=y_p2_reduced; length_p2=length_p2_reduced;
else
    x_line_p2=x_p2_all; y_line_p2=y_p2_all; length_p2=length_p2_all;
end
% compare 1) and 2) 
if length_p1 < length_p2
    x_line=x_line_p1; y_line=y_line_p1; ideal_path=length_p1;
else 
    x_line=x_line_p2; y_line=y_line_p2; ideal_path=length_p2;
end 
clear length* *_p1_* *_p2_*; 

% % test plot
% figure; plot(polyshape_array); hold on; 
% plot(end_x, end_y, 'ro', x_p1_all, y_p1_all, 'm-', x_p2_all, y_p2_all, 'r-',...
%     x_p1_reduced, y_p1_reduced,'b--', x_p2_reduced, y_p2_reduced, 'y--',...
%     x_line_chosen, y_line_chosen, 'k-.');
% xlim([0 1]); ylim([0 1]); hold off; 

end