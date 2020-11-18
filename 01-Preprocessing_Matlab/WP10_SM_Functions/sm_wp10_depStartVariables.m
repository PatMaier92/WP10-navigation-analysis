function [goal_x_ego, goal_y_ego, x_line, y_line, x_line_ego, y_line_ego,...
    ideal_path, ideal_ego_path, ideal_time_allo, ideal_time_ego, id_ht,...
    ideal_vel]=sm_wp10_depStartVariables(start, goal_x, goal_y, goal, x_start,y_start,...
    start_x, start_y, alley_x, alley_y, pentagon_x, pentagon_y)
% SM_WP10_DEPSTARTVARIABLES Function for determining start point dependent
% variables in Starmaze WP10. 
%
% Input: ?
% goal==1 is MA, goal==3 is MC, goal==9 is MI
%
% Returns: ? 

ideal_vel=0.05; % adjust if required

% % Create rotation matrix
% theta = 180; % to rotate 90 counterclockwise
% R = [cosd(theta) -sind(theta); sind(theta) cosd(theta)];

kx1=x_start; ky1=y_start;
kxf=goal_x; kyf=goal_y;
if start==1
    if goal==1
        ekx2=alley_x(3,1); eky2=alley_y(3,1); ekx3=pentagon_x(2); eky3=pentagon_y(2); ekx4=alley_x(3,4); eky4=alley_y(3,4); ekx5=start_x(5); eky5=start_y(5);
        goal_x_ego=ekx5;    goal_y_ego=eky5;
        ideal_path=sm_distance(kx1, kxf,ky1, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4)+sm_distance(ekx4, ekx5,eky4, eky5);
        x_line=[kx1 kxf]; y_line=[ky1 kyf];
        x_line_ego=[kx1 ekx2 ekx3 ekx4 ekx5]; y_line_ego=[ky1 eky2 eky3 eky4 eky5];
    elseif goal==3
        kx2=alley_x(3,1);ky2=alley_y(3,1);   kx3=alley_x(4,2); ky3=alley_y(4,2);
        ekx2=alley_x(4,1); eky2=alley_y(4,1); ekx3=pentagon_x(5); eky3=pentagon_y(5); ekx4=alley_x(3,4); eky4=alley_y(3,4);
        goal_x_ego=0.3;    goal_y_ego=0.225;
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4)+sm_distance(ekx4, goal_x_ego,eky4, goal_y_ego);
        x_line=[kx1 kx2 kx3 kxf]; y_line=[ky1 ky2 ky3 kyf];
        x_line_ego=[kx1 ekx2 ekx3 ekx4 goal_x_ego]; y_line_ego=[ky1 eky2 eky3 eky4 goal_y_ego];
    elseif goal==9
        kx2=alley_x(4,1);ky2=alley_y(4,1);   kx3=alley_x(3,5); ky3=alley_y(3,5);
        ekx2=alley_x(3,1); eky2=alley_y(3,1); ekx3=alley_x(4,2); eky3=alley_y(4,2); ekx4=start_x(3); eky4=start_y(3);
        goal_x_ego=ekx4;    goal_y_ego=eky4;
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4);
        x_line=[kx1 kx2 kx3 kxf]; y_line=[ky1 ky2 ky3 kyf];
        x_line_ego=[kx1 ekx2 ekx3 ekx4]; y_line_ego=[ky1 eky2 eky3 eky4];
    end
        
elseif start==2
    if goal==1
        kx2=alley_x(3,1); ky2=alley_y(3,1);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kxf,ky2, kyf);
        x_line=[kx1 kx2 kxf]; y_line=[ky1 ky2 kyf];
    elseif goal==3
        kx2=alley_x(4,2);ky2=alley_y(4,2);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kxf,ky2, kyf);
        x_line=[kx1 kx2 kxf]; y_line=[ky1 ky2 kyf];
    elseif goal==9
        kx2=pentagon_x(1);ky2=pentagon_y(1);   kx3=alley_x(3,5); ky3=alley_y(3,5);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        x_line=[kx1 kx2 kx3 kxf]; y_line=[ky1 ky2 ky3 kyf];
    end
    goal_x_ego=goal_x;    goal_y_ego=goal_y;
    ideal_ego_path=ideal_path;
    x_line_ego=x_line; y_line_ego=y_line;
    
elseif start==3
    if goal==1
        kx2=alley_x(4,2);ky2=alley_y(4,2);   kx3=alley_x(3,1); ky3=alley_y(3,1);
        ekx2=alley_x(3,2); eky2=alley_y(3,2); ekx3=pentagon_x(3); eky3=pentagon_y(3); ekx4=alley_x(4,4); eky4=alley_y(4,4); ekx5=start_x(7); eky5=start_y(7);
        goal_x_ego=ekx5;    goal_y_ego=eky5;
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4)+sm_distance(ekx4, ekx5,eky4, eky5);
        x_line=[kxf kx3 kx2 kx1]; y_line=[kyf ky3 ky2 ky1]; %
        x_line_ego=[kx1 ekx2 ekx3 ekx4 ekx5]; y_line_ego=[ky1 eky2 eky3 eky4 eky5];
    elseif goal==3
        ekx2=alley_x(4,2); eky2=alley_y(4,2); ekx3=p1_x; eky3=p1_y; ekx4=alley_x(3,5); eky4=alley_y(3,5); ekx5=start_x(9); eky5=start_y(9);
        goal_x_ego=goal_x;    goal_y_ego=goal_y;
        ideal_path=sm_distance(kx1, kxf,ky1, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4)+sm_distance(ekx4, ekx5,eky4, eky5);
        x_line=[kx1 kxf]; y_line=[ky1 kyf];
        x_line_ego=[kx1 ekx2 ekx3 ekx4 ekx5]; y_line_ego=[ky1 eky2 eky3 eky4 eky5];
    elseif goal==9
        kx2=alley_x(4,2);ky2=alley_y(4,2);   kx3=pentagon_x(1); ky3=pentagon_y(1); kx4=alley_x(3,5); ky4=alley_y(3,5);
        ekx2=alley_x(3,2); eky2=alley_y(3,2); ekx3=alley_x(4,3); eky3=alley_y(4,3); ekx4=0.74; eky4=0.175;
        goal_x_ego=ekx4;    goal_y_ego=eky4;
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kx4,ky3, ky4)+sm_distance(kx4, kxf,ky4, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4);
        x_line=[kxf kx4 kx3 kx2 kx1]; y_line=[kyf ky4 ky3 ky2 ky1];
        x_line_ego=[ekx4 ekx3 ekx2 kx1]; y_line_ego=[eky4 eky3 eky2 ky1];
    end
    
elseif start==4
    if goal==1
        kx2=pentagon_x(2);ky2=pentagon_y(2);   kx3=alley_x(3,1); ky3=alley_y(3,1);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        x_line=[kxf kx3 kx2 kx1]; y_line=[kyf ky3 ky2 ky1];
    elseif goal==3
        kx2=alley_x(3,2);ky2=alley_y(3,2);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kxf,ky2, kyf);
        x_line=[kx1 kx2 kxf]; y_line=[ky1 ky2 kyf];
    elseif goal==9
        kx2=pentagon_x(3);ky2=pentagon_y(3);   kx3=pentagon_x(4); ky3=pentagon_y(4); kx4=alley_x(4,5); ky4=alley_y(4,5);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kx4,ky3, ky4)+sm_distance(kx4, kxf,ky4, kyf);
        x_line=[kx1 kx2 kx3 kx4 kxf]; y_line=[ky1 ky2 ky3 ky4 kyf];
    end
    goal_x_ego=goal_x;    goal_y_ego=goal_y;
    ideal_ego_path=ideal_path;
    x_line_ego=x_line; y_line_ego=y_line;
    
elseif start==5
    if goal==1
        kx2=alley_x(4,3);ky2=alley_y(4,3);   kx3=pentagon_x(2); ky3=pentagon_y(2); kx4=alley_x(3,1); ky4=alley_y(3,1);
        ekx2=alley_x(3,3); eky2=alley_y(3,3); ekx3=pentagon_x(4); eky3=pentagon_y(4); ekx4=0.24; eky4=0.6;
        goal_x_ego=ekx4;    goal_y_ego=eky4;
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4);%+sm_distance(ekx4, ekx5,eky4, eky5);
        x_line=[kxf kx4 kx3 kx2 kx1]; y_line=[kyf ky4 ky3 ky2 ky1];
        x_line_ego=[ekx4 ekx3 ekx2 kx1]; y_line_ego=[eky4 eky3 eky2 ky1];
    elseif goal==3
        kx2=alley_x(4,3);ky2=alley_y(4,3);   kx3=alley_x(3,2); ky3=alley_y(3,2);
        ekx2=alley_x(4,3); eky2=alley_y(4,3); ekx3=pentagon_x(2); eky3=pentagon_y(2); ekx4=alley_x(3,1); eky4=alley_y(3,1); ekx5=start_x(1); eky5=start_y(1);
        goal_x_ego=ekx5;    goal_y_ego=eky5;
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4)+sm_distance(ekx4, ekx5,eky4, eky5);
        x_line=[kx1 kx2 kx3 kxf]; y_line=[ky1 ky2 ky3 kyf];
        x_line_ego=[kx1 ekx2 ekx3 ekx4 ekx5]; y_line_ego=[ky1 eky2 eky3 eky4 eky5];
    elseif goal==9
        kx2=alley_x(3,3);ky2=alley_y(3,3);   kx3=pentagon_x(4); ky3=pentagon_y(4); kx4=alley_x(4,5); ky4=alley_y(4,5);
        ekx2=alley_x(3,3); eky2=alley_y(3,3); ekx3=alley_x(4,4); eky3=alley_y(4,4); ekx4=0.28; eky4=0.175;
        goal_x_ego=ekx4;    goal_y_ego=eky4;
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kx4,ky3, ky4)+sm_distance(kx4, kxf,ky4, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,ky3, eky4);
        x_line=[kxf kx4 kx3 kx2 kx1]; y_line=[kyf ky4 ky3 ky2 ky1];
        x_line_ego=[ekx4 ekx3 ekx2 kx1]; y_line_ego=[eky4 eky3 eky2 ky1];
    end
    
elseif start==6
    if goal==1
        kx2=pentagon_x(4);ky2=pentagon_y(4);   kx3=pentagon_x(5); ky3=pentagon_y(5); kx4=alley_x(4,1); ky4=alley_y(4,1);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kx4,ky3, ky4)+sm_distance(kx4, kxf,ky4, kyf);
        x_line=[kx1 kx2 kx3 kx4 kxf]; y_line=[ky1 ky2 ky3 ky4 kyf];
    elseif goal==3
        kx2=pentagon_x(3);ky2=pentagon_y(3); kx3=alley_x(3,2); ky3=alley_y(3,2);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        x_line=[kxf kx3 kx2 kx1]; y_line=[kyf ky3 ky2 ky1   ];
    elseif goal==9
        kx2=pentagon_x(4);ky2=pentagon_y(4); kx3=alley_x(4,5); ky3=alley_y(4,5);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        x_line=[kxf kx3 kx2 kx1]; y_line=[kyf ky3 ky2 ky1]; % ideal y-coordinates to allocentric target
    end
    goal_x_ego=goal_x;    goal_y_ego=goal_y;
    ideal_ego_path=ideal_path;
    x_line_ego=x_line; y_line_ego=y_line;
    
elseif start==7
    if goal==1
        kx2=alley_x(3,4);ky2=alley_y(3,4);   kx3=pentagon_x(5); ky3=pentagon_y(5);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        x_line=[kx1 kx2 kx3 kxf]; y_line=[ky1 ky2 ky3 kyf];
    elseif goal==3
        kx2=alley_x(4,4);ky2=alley_y(4,4);   kx3=pentagon_x(3); ky3=pentagon_y(3); kx4=alley_x(3,2); ky4=alley_y(3,2);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kx4,ky3, ky4)+sm_distance(kx4, kxf,ky4, kyf);
        x_line=[kx1 kx2 kx3 kx4 kxf]; y_line=[ky1 ky2 ky3 ky4 kyf];
    elseif goal==9
        kx2=alley_x(3,4);ky2=alley_y(3,4);   kx3=alley_x(4,5); ky3=alley_y(4,5);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        x_line=[kxf kx3 kx2 kx1]; y_line=[kyf ky3 ky2 ky1];
    end
    ideal_ego_path=ideal_path;
    x_line_ego=x_line; y_line_ego=y_line;
    goal_x_ego=goal_x;    goal_y_ego=goal_y;
    
elseif start==8
    if goal==1
        kx2=pentagon_x(5); ky2=pentagon_y(5);   kx3=alley_x(4,1); ky3=alley_y(4,1);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        x_line=[kxf kx3 kx2 kx1]; y_line=[kyf ky3 ky2 ky1];
    elseif goal==3
        kx2=pentagon_x(4);  ky2=pentagon_y(4); kx3=pentagon_x(3); ky3=pentagon_y(3); kx4=alley_x(3,2); ky4=alley_y(3,2);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kx4,ky3, ky4)+sm_distance(kx4, kxf,ky4, kyf);
        x_line=[kx1 kx2 kx3 kxf]; y_line=[ky1 ky2 ky3 kyf];
    elseif goal==9
        kx2=alley_x(4,5);ky2=alley_y(4,5);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kxf,ky2, kyf);
        x_line=[kxf kx2 kx1]; y_line=[kyf ky2 ky1];
    end
    goal_x_ego=goal_x;    goal_y_ego=goal_y;
    ideal_ego_path=ideal_path;
    x_line_ego=x_line; y_line_ego=y_line;
    
elseif start==9
    if goal==1
        kx2=alley_x(3,5);ky2=alley_y(3,5);   kx3=alley_x(4,1); ky3=alley_y(4,1);
        ekx2=alley_x(3,5); eky2=alley_y(3,5); ekx3=pentagon_x(1); eky3=pentagon_y(1); ekx4=alley_x(4,2); eky4=alley_y(4,2); ekx5=start_x(3); eky5=start_y(3);
        goal_x_ego=ekx5;    goal_y_ego=eky5;
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4)+sm_distance(ekx4, ekx5,eky4, eky5);
        x_line=[kx1 kx2 kx3 kxf]; y_line=[ky1 ky2 ky3 kyf];
        x_line_ego=[kx1 ekx2 ekx3 ekx4 ekx5]; y_line_ego=[ky1 eky2 eky3 eky4 eky5];
    elseif goal==3
        kx2=alley_x(3,5);ky2=alley_y(3,5);   kx3=pentagon_x(1); ky3=pentagon_y(1); kx4=alley_x(4,2); ky4=alley_y(4,2);
        ekx2=alley_x(4,5); eky2=alley_y(4,5); ekx3=pentagon_x(4); eky3=pentagon_y(4); ekx4=alley_x(3,3); eky4=alley_y(3,3); ekx5=0.68; eky5=0.2;
        goal_x_ego=ekx5;    goal_y_ego=eky5;
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kx4,ky3, ky4)+sm_distance(kx4, kxf,ky4, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4)+sm_distance(ekx4, ekx5,eky4, eky5);
        x_line=[kx1 kx2 kx3 kx4 kxf]; y_line=[ky1 ky2 ky3 ky4 kyf];
        x_line_ego=[kx1 ekx2 ekx3 ekx4 ekx5]; y_line_ego=[ky1 eky2 eky3 eky4 eky5];
    elseif goal==9
        ekx2=alley_x(3,5); eky2=alley_y(3,5); ekx3=alley_x(4,1); eky3=alley_y(4,1); ekx4=start_x(1); eky4=start_y(1);
        goal_x_ego=ekx4;    goal_y_ego=eky4;
        ideal_path=sm_distance(kx1, kxf,ky1, kyf);
        ideal_ego_path=sm_distance(kx1, ekx2,ky1, eky2)+sm_distance(ekx2, ekx3,eky2, eky3)+sm_distance(ekx3, ekx4,eky3, eky4);
        x_line=[kx1 kxf]; y_line=[ky1 kyf];
        x_line_ego=[kx1 ekx2 ekx3 ekx4]; y_line_ego=[ky1 eky2 eky3 eky4];
    end
    
elseif start==10
    if goal==1
        ideal_path=sm_distance(kx1, kxf,ky1, kyf);
        x_line=[kx1 kxf]; y_line=[ky1 kyf];
    elseif goal==3
        kx2=pentagon_x(1);ky2=pentagon_y(1);   kx3=alley_x(4,2); ky3=alley_y(4,2);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kx3,ky2, ky3)+sm_distance(kx3, kxf,ky3, kyf);
        x_line=[kxf kx3 kx2 kx1]; y_line=[kyf ky3 ky2 ky1];
    elseif goal==9
        kx2=alley_x(3,5);ky2=alley_y(3,5);
        ideal_path=sm_distance(kx1, kx2,ky1, ky2)+sm_distance(kx2, kxf,ky2, kyf);
        x_line=[kx1 kx2 kxf]; y_line=[ky1 ky2 kyf];
    end
    goal_x_ego=goal_x;    goal_y_ego=goal_y;
    ideal_ego_path=ideal_path;
    x_line_ego=x_line; y_line_ego=y_line;
end

% ideal time
ideal_time_ego=ideal_ego_path/ideal_vel;    
ideal_time_allo=ideal_path/ideal_vel;  id_ht=numel(x_line); 

end