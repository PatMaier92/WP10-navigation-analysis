function [goal_x,goal_y]=sm_wp10_goal(goal,xmin,xmax,ymin,ymax)

    [row,col]=size(goal);
    % Data-normalization goal position
    for r=1:row
    goal_x(r,1)=datanorm(goal(r,1),xmin,xmax);
    goal_y(r,1)= datanorm(goal(r,2),ymin,ymax);
    end
end