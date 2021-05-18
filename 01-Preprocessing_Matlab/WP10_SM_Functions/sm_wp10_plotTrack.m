function sm_wp10_plotTrack(trial,session,cond,id,Group, ...
    alley_polyshape_1,alley_polyshape_2,tri,rec,...
    x,y,x_line_ego,y_line_ego,x_line,y_line,goal_x,goal_y,folderOut)
% SM_WP_10_PLOTTRACK Creates track plots for each individual trial.
%
% Input: A lot of information for creating and naming the plot, including
% geometrical information and egocentric and allocentric tracks. 
%
% Returns: A nice track plot.

polyshape_array=[alley_polyshape_1{1,1} alley_polyshape_2{1,1} alley_polyshape_1{1,2} alley_polyshape_2{1,2}...
    alley_polyshape_1{1,3} alley_polyshape_2{1,3} alley_polyshape_1{1,4} alley_polyshape_2{1,4}...
    alley_polyshape_1{1,5} alley_polyshape_1{1,5} alley_polyshape_2{1,5} tri{1,1} tri{1,2} tri{1,3} tri{1,4} tri{1,5}...
    rec{1,1} rec{1,2} rec{1,3} rec{1,4} rec{1,5}];

T=int2str(trial);
Session=num2str(session);
ID=num2str(id);

wfig=figure('visible', 'off');
set(gca,'xtick',[0 1],'ytick',[0 1]);
axis([0 1 0 1])
hold on

if cond==0
    type = ' (Learning)'; 
elseif cond==1
    type = ' (Allocentric Probe)';
elseif cond==2
    type = ' (Egocentric Probe)';
elseif cond==3
    type = ' (Learning Probe)';
else
    type = ' (XXXXX)'; 
end
title({[ID ', ' Group];['Session: ' Session' ', Trial: ' T type]});

plot(polyshape_array, 'FaceColor',[0.6 0.6 0.6],'FaceAlpha',0.1)
viscircles([goal_x goal_y], 0.01)

%  plot tracks
if cond==2
    line1=plot(x,y,'k -', 'LineWidth', 1);
    line3=plot(x_line_ego,y_line_ego,'g .:', 'LineWidth', 1);
    legend([line1 line3],{'actual path','ideal egocentric path'});
else
    line1=plot(x,y,'k -', 'LineWidth', 1);
    line2=plot(x_line,y_line,'r -.', 'LineWidth', 1);
    line3=plot(x_line_ego,y_line_ego,'g .:', 'LineWidth', 0.7);
    legend([line1 line2 line3],{'actual path','ideal path','egocentric path'});
end
hold off

% save image
filename1 =['Plot_' Group '_' ID '_' Session '_' T];
filename2 = [filename1,'.jpeg'];
fullFileName2 = fullfile(folderOut, filename2);
saveas(wfig,fullFileName2)

% % save track
% zfig=figure('visible', 'off');
% set(gca,'XColor', 'none','YColor','none')
% axis([0 1 0 1])
% hold on
% plot(x,y,'k -', 'LineWidth', 1);
% hold off
% 
% filename =['Track_' Group '_' ID '_' Session '_' T];
% filename2 = [filename,'.jpeg'];
% fullFileName2 = fullfile(folderOut, filename2);
% saveas(zfig,fullFileName2)

hold off