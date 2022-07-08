function distance_score=computeDistanceScore(avg_distance, x, y, ...
    random_x, random_y)
% computeDistanceScore: Computes target distance score, i.e. average 
% cumulative distance to target is set in relation to randomly distributed 
% points in the WP10 Starmaze. 
%
% Input: 
%
% Returns: standardized score (0=low, 1=high) (float) 

random_distances=zeros(numel(random_x),1);
for i=1:numel(random_distances)
    random_distances(i,1)=computeTargetDistance(x,y,random_x(i),random_y(i));
end

% compute score 
distance_score = sum(random_distances < avg_distance) / numel(random_x);

% % helper plot
% index=random_distances < avg_distance;
% figure; plot(sm.coord.full_poly); hold on;
% plot(random_x(index), random_y(index), 'b*');
% xlim([0 1]); ylim([0 1]); hold off;

end