function [goal_x_ego, goal_y_ego, x_line, y_line, x_line_ego, y_line_ego,...
    ideal_path, ideal_ego_path, ideal_headturn, ego_alley]=sm_wp10_depStartVariables(start, ...
    goal_x, goal_y, goal, x_start, y_start, ...
    start_x, start_y, alley_x, alley_y, pentagon_x, pentagon_y, ...
    alley_full_x, alley_full_y, rec_x, rec_y, cP_polyshape)
% SM_WP10_DEPSTARTVARIABLES Function for determining starting point dependent
% variables in Starmaze WP1.
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

%% define original direct path from original start
% define original coordinate points
kxs=start_x(7); kys=start_y(7);
kxf=goal_x; kyf=goal_y;
kx2=0; kx3=0; kx4=0; ky2=0; ky3=0; ky4=0; steps=0; 
if goal==1 % A, goal_alley 1
    kx2=alley_x(3,4); ky2=alley_y(3,4);  kx3=pentagon_x(5); ky3=pentagon_y(5);  kx4=alley_x(4,1); ky4=alley_y(4,1);
elseif goal==2 % C, goal_alley 3
    kx2=alley_x(4,4); ky2=alley_y(4,4);  kx3=pentagon_x(3); ky3=pentagon_y(3);  kx4=alley_x(3,2); ky4=alley_y(3,2);
elseif goal==3 % I, goal_alley 9
    kx2=alley_x(3,4); ky2=alley_y(3,4);  kx3=alley_x(4,5);  ky3=alley_y(4,5);
else
    disp('Goal not identified in sm_wp1_depStartVariables.m');
end

% define number of steps
if kx4==0 && ky4==0
    steps=2;
else
    steps=3;
end

% combine original path lines
if steps==2
    o_x_line=[kxs kx2 kx3 kxf];  o_y_line=[kys ky2 ky3 kyf];
elseif steps==3
    o_x_line=[kxs kx2 kx3 kx4 kxf];  o_y_line=[kys ky2 ky3 ky4 kyf];
else
    disp('Unknown number of steps in sm_wp1_depStartVariables.m');
end

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

%% define direct path from various start positions
% define coordinate points
kxs=x_start; kys=y_start;
kxf=goal_x; kyf=goal_y;
kx2=0; kx3=0; kx4=0; ky2=0; ky3=0; ky4=0; steps=0;
if start==7 % original start G
    x_line=o_x_line; y_line=o_y_line;
elseif start==1 % A
    if goal==1
        kx2=0; ky2=0; % start and goal in same alley 
    elseif goal==2
        kx2=alley_x(3,1); ky2=alley_y(3,1);  kx3=alley_x(4,2); ky3=alley_y(4,2);
    elseif goal==3
        kx2=alley_x(4,1); ky2=alley_y(4,1);  kx3=alley_x(3,5); ky3=alley_y(3,5); 
    else
        fprintf('Error path to goal %d in new start %d in sm_wp1_depStartVariables.\n',goal,start);
    end
elseif start==2 % B (inner)
    if goal==1
        kx2=alley_x(3,1); ky2=alley_y(3,1);
    elseif goal==2
        kx2=alley_x(4,2); ky2=alley_y(4,2);
    elseif goal==3
        kx2=pentagon_x(1); ky2=pentagon_y(1);  kx3=alley_x(3,5); ky3=alley_y(3,5);  
    else
        fprintf('Error path to goal %d in new start %d in sm_wp1_depStartVariables.\n',goal,start);
    end
elseif start==3 % C
    if goal==1
        kx2=alley_x(4,2); ky2=alley_y(4,2);  kx3=alley_x(3,1); ky3=alley_y(3,1);
    elseif goal==2
        kx2=0; ky2=0; % start and goal in same alley 
    elseif goal==3
        kx2=alley_x(4,2); ky2=alley_y(4,2);  kx3=pentagon_x(1); ky3=pentagon_y(1);  kx4=alley_x(3,5); ky4=alley_y(3,5);
    else
        fprintf('Error path to goal %d in new start %d in sm_wp1_depStartVariables.\n',goal,start);
    end
elseif start==4 % D (inner)
    if goal==1
        kx2=pentagon_x(2); ky2=pentagon_y(2);  kx3=alley_x(3,1); ky3=alley_y(3,1);
    elseif goal==2
        kx2=alley_x(3,2); ky2=alley_y(3,2);
    elseif goal==3
        % TBD or other side around 
        kx2=pentagon_x(2); ky2=pentagon_y(2);  kx3=pentagon_x(1); ky3=pentagon_y(1);  kx4=alley_x(3,5); ky4=alley_y(3,5);
    else
        fprintf('Error path to goal %d in new start %d in sm_wp1_depStartVariables.\n',goal,start);
    end
elseif start==5 % E
    if goal==1
        kx2=alley_x(4,3); ky2=alley_y(4,3);  kx3=pentagon_x(2); ky3=pentagon_y(2);  kx4=alley_x(3,1); ky4=alley_y(3,1);
    elseif goal==2
        kx2=alley_x(4,3); ky2=alley_y(4,3);  kx3=alley_x(3,2); ky3=alley_y(3,2);
    elseif goal==3
        kx2=alley_x(3,3); ky2=alley_y(3,3);  kx3=pentagon_x(4); ky3=pentagon_y(4);  kx4=alley_x(4,5); ky4=alley_y(4,5);
    else
        fprintf('Error path to goal %d in new start %d in sm_wp1_depStartVariables.\n',goal,start);
    end
elseif start==6 % F 
    if goal==1
        % TBD or other side around 
        kx2=pentagon_x(3); ky2=pentagon_y(3);  kx3=pentagon_x(2); ky3=pentagon_y(2);  kx4=alley_x(3,1); ky4=alley_y(3,1);
    elseif goal==2
        kx2=pentagon_x(3); ky2=pentagon_y(3);  kx3=alley_x(3,2); ky3=alley_y(3,2);
    elseif goal==3
        kx2=pentagon_x(4); ky2=pentagon_y(4);  kx3=alley_x(4,5); ky3=alley_y(4,5);
    else
        fprintf('Error path to goal %d in new start %d in sm_wp1_depStartVariables.\n',goal,start);
    end
elseif start==8 % H 
    if goal==1
        kx2=pentagon_x(5); ky2=pentagon_y(5);  kx3=alley_x(4,1); ky3=alley_y(4,1);
    elseif goal==2
        % TBD or other side around 
        kx2=pentagon_x(4); ky2=pentagon_y(4);  kx3=pentagon_x(3); ky3=pentagon_y(3);  kx4=alley_x(3,2); ky4=alley_y(3,2);
    elseif goal==3
        kx2=alley_x(4,5); ky2=alley_y(4,5);
    else
        fprintf('Error path to goal %d in new start %d in sm_wp1_depStartVariables.\n',goal,start);
    end
elseif start==9 % I 
    if goal==1
        kx2=alley_x(3,5); ky2=alley_y(3,5);  kx3=alley_x(4,1); ky3=alley_y(4,1);
    elseif goal==2
        kx2=alley_x(3,5); ky2=alley_y(3,5);  kx3=pentagon_x(1); ky3=pentagon_y(1);  kx4=alley_x(4,2); ky4=alley_y(4,2);
    elseif goal==3
        kx2=0; ky2=0; % start and goal in same alley 
    else
        fprintf('Error path to goal %d in new start %d in sm_wp1_depStartVariables.\n',goal,start);
    end
elseif start==10 % J
    if goal==1
        kx2=alley_x(4,1); ky2=alley_y(4,1);  
    elseif goal==2
        kx2=pentagon_x(1); ky2=pentagon_y(1);  kx3=alley_x(4,2); ky3=alley_y(4,2);
    elseif goal==3
        kx2=alley_x(3,5); ky2=alley_y(3,5);
    else
        fprintf('Error path to goal %d in new start %d in sm_wp1_depStartVariables.\n',goal,start);
    end
else
    fprintf('Error in start %d in sm_wp1_depStartVariables\n', start);
end

% combine path lines for new starts
if start~=7
    % define number of steps
    if kx2==0 && ky2==0
        steps=0;
    elseif kx3==0 && ky3==0
        steps=1;
    elseif kx4==0 && ky4==0
        steps=2;
    else
        steps=3;
    end
    
    % combine path lines
    if steps==0
        x_line=[kxs kxf];  y_line=[kys kyf];
    elseif steps==1
        x_line=[kxs kx2 kxf];  y_line=[kys ky2 kyf];
    elseif steps==2
        x_line=[kxs kx2 kx3 kxf];  y_line=[kys ky2 ky3 kyf];
    elseif steps==3
        x_line=[kxs kx2 kx3 kx4 kxf];  y_line=[kys ky2 ky3 ky4 kyf];
    else
        fprintf('Error steps is %d in sm_wp1_depStartVariables\n', steps);
    end
end

% calculate ideal path length value (external function)
ideal_path=sm_wp10_idealPathLength(x_line, y_line);

% calculate ideal number of turns 
ideal_headturn=numel(x_line)-2; % minus start and end points (no head turns there)

end