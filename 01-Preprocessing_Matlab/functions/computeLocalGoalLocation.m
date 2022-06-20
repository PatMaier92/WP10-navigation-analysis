function [goal_x_local, goal_y_local]=computeLocalGoalLocation(goal_x_in_alleys,...
    goal_y_in_alleys, chosen_alley, goal)
% computeLocalGoalLocation: Function for determining hypothetical local goal
% location in chosen goal alley. 
%
% Input:
% goal_x_in_alleys, goal_y_in_alleys are matrices with coordinates (float)
% chosen alley, goal alley (integer)
% 
% Returns: goal_x_local, goal_y_local are local coordinates in chosen alley (float)

% set default (used when chosen alley is in inner rectangle)
goal_x_local=999; goal_y_local=999;

% get local goal location if chosen alley is in any outer alley (incl. triangle) 
if mod(chosen_alley,2)~=0
    goal_x_local=goal_x_in_alleys(goal,(chosen_alley + 1)/2);
    goal_y_local=goal_y_in_alleys(goal,(chosen_alley + 1)/2);
end

% % test plot
% figure; plot(sm.coord.full_poly); hold on;
% plot(goal_x, goal_y, 'kx', goal_x_local, goal_y_local, 'ro');
% xlim([0 1]); ylim([0 1]); hold off;

end