function sm_wp10_testfig(polyshape_array,goal_x,goal_y)
% SM_WP10_TESTFIG Create Starmaze WP10 testfigure for visualization of all
% xy-coordinates, goals and starting points. 

% figure
figure('Position',[500 200 580 500]);
set(gca,'xtick',[0 1],'ytick',[0 1]);
plot(polyshape_array)
axis([0 1 0 1])
title('Star-Maze')
hold on
for g=1:length(goal_x)
    viscircles([goal_x(g) goal_y(g)], 0.01)
end
hold off

text(0.1,0.8,'Alley 5')
text(0.9,0.9,'Alley 2')
text(0.1,0.2,'Alley 4')
text(0.9,0.2,'Alley 3')
text(0.6,0.9,'Alley 1')

end