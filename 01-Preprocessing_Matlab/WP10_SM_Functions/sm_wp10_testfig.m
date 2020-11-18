function sm_wp10_testfig(polyshape_array,goal_x,goal_y,start_x,start_y)
% SM_WP10_TESTFIG Create Starmaze WP10 testfigure for visualization of all
% xy-coordinates, goals and starting points. 

% figure
figure('Position',[500 200 580 500]);
set(gca,'xtick',[0 1],'ytick',[0 1]);
plot(polyshape_array)
axis([0 1 0 1])
title('Starmaze WP10')
hold on
for g=1:length(goal_x)
    viscircles([goal_x(g) goal_y(g)], 0.01)
end
viscircles([start_x(7) start_y(7)], 0.02)
hold off

text(0.1,0.75,'Alley 5 - I')
text(0.9,0.75,'Alley 2 - C')
text(0.05,0.2,'Alley 4 - G')
text(0.8,0.2,'Alley 3 - E')
text(0.6,0.9,'Alley 1 - A')

text(goal_x(1)+0.02, goal_y(1), 'MA')
text(goal_x(2)+0.02, goal_y(2), 'MC')
text(goal_x(3)+0.02, goal_y(3), 'MI')

text(start_x(7)-0.05, start_y(7)-0.07, 'Start Learn')

end