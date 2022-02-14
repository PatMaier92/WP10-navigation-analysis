function plotTrialTrack(trial,session,condition,start,id,Group,correct_goal,shortest_path,strategy,...
    polyshape,x,y,x_line_ego,y_line_ego,x_line,y_line,goal_x,goal_y,folder)
% plotTrialTrack Creates track plots for each individual trial.
%
% Input: Information for creating and naming the plot
%
% Returns: A nice trial track plot.

Trial=int2str(trial);
Session=num2str(session);
ID=num2str(id);
Correct_goal=num2str(correct_goal); 
Shortest_path=num2str(shortest_path);
Strategy=num2str(strategy);

wfig=figure('visible','off');
plot(polyshape,'FaceColor',[0.6 0.6 0.6],'FaceAlpha',0.1);
axis([0 1 0 1]); xticks(0:0.1:1); yticks(0:0.1:1); 
hold on;

viscircles([goal_x goal_y],0.01); 

if condition==0 || condition==2 || condition==3 ||(condition==1 && ~mod(start,2)) % training, egocentric or allocentric inner starts 
    line1=plot(x,y,'k -', 'LineWidth', 1);
    line2=plot(x_line,y_line,'r -.', 'LineWidth', 1);
    legend([line1 line2],{'actual path','ideal path'});
else % allocentric outer starts
    line1=plot(x,y,'k -', 'LineWidth', 1);
    line2=plot(x_line,y_line,'r -.', 'LineWidth', 1);
    line3=plot(x_line_ego,y_line_ego,'g .:', 'LineWidth', 0.7);
    legend([line1 line2 line3],{'actual path','ideal path','egocentric path'});
end

if condition==0
    type = ' (Training)'; 
elseif condition==1
    type = ' (Allocentric Probe)';
elseif condition==2
    type = ' (Egocentric Probe)';
elseif condition==3
    type = ' (Training Probe)';
else
    type = ' (XXXXX)'; 
end
title({[ID ' (' Group ') Session: ' Session ', Trial: ' Trial type];['Correct goal: ', Correct_goal, ', Shortest path: ' Shortest_path ', Strategy: ' Strategy]});
hold off; 

% save plot
file_name = ['Plot_' Group '_' ID '_' Session '_' Trial '.jpeg'];
file_path = fullfile(folder, file_name);
saveas(wfig,file_path);

end