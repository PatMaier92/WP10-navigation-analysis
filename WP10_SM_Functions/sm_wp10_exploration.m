function exploration=sm_wp10_exploration(alley_zone)

[row,col]=size(alley_zone);
% exploration mark, determine how many alleys have beend entered
exploration=0;
for c=1:col
    if alley_zone(:,c) > 0
        exploration= exploration+1;
    end
end
end