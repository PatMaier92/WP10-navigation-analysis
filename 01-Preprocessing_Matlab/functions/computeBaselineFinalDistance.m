function baseline_fd=computeBaselineFinalDistance(goal_x_in_alleys,...
    goal_y_in_alleys, goal, goal_x, goal_x_ego, x_n, y_n)
% computeBaselineFinalDistance: Compute baseline for the final distance
% value in WP10 Starmaze to compare with final_distance and
% final_distance_ego. 
%
% Input: 
%
% Returns: T is time difference. 

% get integers (for x only is sufficient)
col_g=find(abs(goal_x_in_alleys(goal,:) - goal_x) > 1e-6);
col_e=find(abs(goal_x_in_alleys(goal,:) - goal_x_ego) > 1e-6);
cols=intersect(col_e, col_g);

% calculate final distances
distances=zeros(1,length(cols));
i=1;
for j=cols
    distances(i)=computeDistance(...
        goal_x_in_alleys(goal,j), x_n,...
        goal_y_in_alleys(goal,j), y_n);
    i=i+1;
end

% calculate mean
baseline_fd=mean(distances);

end

