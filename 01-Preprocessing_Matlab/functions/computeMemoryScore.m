function memory_score=computeMemoryScore(distribution, final_distance, goal)
% computeMemoryScore: Computes memory score, i.e. final distance to goal 
% is set in relation to randomly distributed points in the WP10 Starmaze. 
%
% Input: 
% distribution is matrix of final distance values to random points (float)
% final_distance is final distance value for this trial (float)
% goal indicates goal location for this trial (integer) 
%
% Returns: standardized memory_score (0=low, 1=high) (float) 

percentage_below = sum(distribution(goal,:) < final_distance) / 1000;
memory_score = 1 - percentage_below;
   
end

