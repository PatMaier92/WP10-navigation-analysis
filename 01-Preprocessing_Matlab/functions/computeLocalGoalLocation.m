function [goal_x_local, goal_y_local]=computeLocalGoalLocation(chosen_alley,...
    goal_alley, goal_x, goal_y)
% computeLocalGoalLocation: Function for determining theoretical local goal
% location in chosen goal alley. 
%
% Input:
% chosen alley, goal alley (integer)
% goal_x, goal_y are coordinates (float)
% 
% Returns: goal_x_local, goal_y_local are local coordinates in chosen alley (float)

% set default (used when chosen alley in inner rectangle)
goal_x_local=999; goal_y_local=999;

% compute local goal location if chosen alley is in any outer alley (incl. triangle) 
if mod(chosen_alley,2)~=0
    diff=chosen_alley-goal_alley;
    if diff~=0
        % correction for negative diff (i.e. when chosen=1, goal=9)
        if diff<0
            diff=diff+10;
        end
        
        % original goal
        v = [goal_x ; goal_y];
        % define center of rotation
        x_center = 0.5; y_center = 0.5;
        % create a matrix
        center = repmat([x_center; y_center], 1, length(v));
        
        % create rotation matrix
        theta=-360/5*diff/2; % rotates clockwise in 72° steps
        R = [cosd(theta) -sind(theta); sind(theta) cosd(theta)];
        
        % do rotation
        vo = R*(v - center) + center;
        
        % get rotated x- and y-data
        goal_x_local = vo(1,1);
        goal_y_local = vo(2,1);
    else
        goal_x_local=goal_x; goal_y_local=goal_y;
    end
end

% % test plot
% figure; plot(sm.coord.full_poly); hold on;
% plot(goal_x, goal_y, 'kx', goal_x_local, goal_y_local, 'ro');
% xlim([0 1]); ylim([0 1]); hold off;

end