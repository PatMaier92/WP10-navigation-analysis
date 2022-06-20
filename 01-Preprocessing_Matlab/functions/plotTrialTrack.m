function plotTrialTrack(trial,session,condition,start,id,Group,...
    correct_alley,strategy,...
    polyshape,x,y,x_line,y_line,x_line_ego,y_line_ego,...
    x_line_chosen,y_line_chosen,goal_x,goal_y,folder)
% plotTrialTrack Creates track plots for each individual trial.
%
% Input: Information for creating and naming the plot
%
% Returns: A nice trial track plot.

Trial=int2str(trial);
Session=num2str(session);
ID=num2str(id);
Accuracy=num2str(correct_alley); 
Strategy=char(strategy);

wfig=figure('visible','off');
plot(polyshape,'FaceColor',[0.6 0.6 0.6],'FaceAlpha',0.1);
axis([0 1 0 1]); xticks(0:0.1:1); yticks(0:0.1:1); 
hold on;

viscircles([goal_x goal_y],0.01); 

if condition=="main_learn" || condition=="main_ret" || condition=="ego_ret" ||(condition=="allo_ret" && ~mod(start,2))
    line1=plot(x,y,'k -', 'LineWidth', 1);
    line2=plot(x_line,y_line,'r -.', 'LineWidth', 1);
    line3=plot(x_line_chosen,y_line_chosen,'b .:', 'LineWidth', 1);
    legend([line1 line2 line3],{'actual path','ideal path target','ideal path chosen'});
else % allocentric outer starts
    line1=plot(x,y,'k -', 'LineWidth', 1);
    line2=plot(x_line,y_line,'r -.', 'LineWidth', 1);
    line3=plot(x_line_chosen,y_line_chosen,'b .:', 'LineWidth', 1);
    line4=plot(x_line_ego,y_line_ego,'g .:', 'LineWidth', 1);
    legend([line1 line2 line3 line4],{'actual path','ideal path target','ideal path chosen','ideal path ego'});
end

if condition=="main_learn"
    type = ' (Training)'; 
elseif condition=="allo_ret"
    type = ' (Allocentric Probe)';
elseif condition=="ego_ret"
    type = ' (Egocentric Probe)';
elseif condition=="main_ret"
    type = ' (Training Probe)';
else
    type = ' (XXXXX)'; 
end
title({[ID ' (' Group ') Session: ' Session ', Trial: ' Trial type]; ...
    ['Acc.: ', Accuracy, ', Strategy: ' Strategy]});
hold off; 

% save plot
file_name = ['Plot_' Group '_' ID '_' Session '_' Trial '.jpeg'];
file_path = fullfile(folder, file_name);
saveas(wfig,file_path);

end