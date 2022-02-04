function plotMotorControlTrial(trial,session,id,Group,...
    pract_polyshape,goal_x,goal_y,start_x,start_y,...
    pract_goal_locs,x,y,xi_al,yi_al,output_folder)
% plotMotorControlTrial: Creates plot for motor control trial. 
%
% Input: Information for creating and naming the plot, including
% geometrical information and egocentric and allocentric tracks. 
%
% Returns: Saves one nice plot.

T=int2str(trial);
Session=num2str(session);
ID=num2str(id); 
type=' (Motor Control)'; 

wfig=figure('Position',[500 200 580 500]);
set(gca,'xtick',[0 1],'ytick',[0 1]);
plot(pract_polyshape)
axis([0 1 0 1])
hold on
title({[ID ', ' Group];['Session: ' Session' ', Trial: ' T type]});
for g=1:length(goal_x)
    viscircles([goal_x(g) goal_y(g)], 0.02)
end
viscircles([start_x start_y], 0.03)

% labels
for i=1:length(pract_goal_locs)
    text(goal_x(i)+0.02, goal_y(i), pract_goal_locs(i))
end 
text(start_x-0.05, start_y-0.05, 'Start') 

% lines and legend 
line1=plot(x,y,'k -', 'LineWidth', 1);
line2=plot(xi_al, yi_al,'r -.', 'LineWidth', 1);
legend([line1 line2],{'actual path','ideal path'},'Location','north'); 
hold off

% save plot
file_name =['Motor_Plot_' Group '_' ID '_' Session '_' T '.jpeg'];
saveas(wfig, fullfile(output_folder, file_name)); 

end