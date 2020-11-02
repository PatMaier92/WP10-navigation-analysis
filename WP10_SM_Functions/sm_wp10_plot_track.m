function sm_wp10_plot_track(wp,k,feedback,session,TC,id,Group,name,...
    alley_polyshape_1,alley_polyshape_2, tri, rec,x,y,x_line_ego,y_line_ego,x_line,y_line,folderOut,goal_x,goal_y)

polyshape_array=[alley_polyshape_1{1,1} alley_polyshape_2{1,1} alley_polyshape_1{1,2} alley_polyshape_2{1,2}...
    alley_polyshape_1{1,3} alley_polyshape_2{1,3} alley_polyshape_1{1,4} alley_polyshape_2{1,4}...
    alley_polyshape_1{1,5} alley_polyshape_1{1,5} alley_polyshape_2{1,5} tri{1,1} tri{1,2} tri{1,3} tri{1,4} tri{1,5}...
    rec{1,1} rec{1,2} rec{1,3} rec{1,4} rec{1,5}];


m=k;
K=int2str(m);
Session=num2str(session);
ID=num2str(id);

wfig=figure('visible', 'off');
 set(gca,'xtick',[0 1],'ytick',[0 1]);
 axis([0 1 0 1])
 hold on

 s2='true';
if strcmp(feedback,s2)
    F='Feedback';
else
    F= 'no feedback';
end

if session==2 && TC==1
    title({ID;Group;['Trial ' K ' (Allocentric) ' F];Session});
elseif session==2 && TC==2
     title({ID;Group;['Trial ' K ' (Egocentric) ' F];Session});
elseif session==2 && TC==3
    title({ID;Group;['Trial ' K ' (Mixed) ' F];Session});
elseif session==2 && TC==0
    title({ID;Group;['Trial ' K ' (Training Probetrial) ' F];Session});
elseif session==2 
    title({ID;Group;['Trial ' K ' ' F];Session});
elseif session==1 && TC==1
    title({ID;Group;['Trial ' K ' (Allocentric) ' F];Session});
elseif session==1 && TC==2
    title({ID;Group;['Trial ' K ' (Egocentric) ' F];Session});
elseif session==1 && TC==3
     title({ID;Group;['Trial ' K ' (Mixed) ' F];Session});
elseif session==1 && TC==0
     title({ID;Group;['Trial ' K ' (Training) ' F];Session});
else
    title({ID;Group;'XXXXXXX';Session});
end
    
plot(polyshape_array, 'FaceColor',[0.6 0.6 0.6],'FaceAlpha',0.1)
     viscircles([goal_x goal_y], 0.01)
    
%  plot tracks    
    if TC==2
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
    filename1 =['Plot_' wp '_' Group '_' ID '_' Session '_' K];
    filename2 = [filename1,'.jpeg'];
    fullFileName2 = fullfile(folderOut, filename2);
     saveas(wfig,fullFileName2)
     
% save plot
zfig=figure('visible', 'off');
    set(gca,'XColor', 'none','YColor','none')
    axis([0 1 0 1])
    hold on
    plot(x,y,'k -', 'LineWidth', 1);
    hold off
    filename =['Track_' wp '_' Group '_' ID '_' Session '_' K];
    filename2 = [filename,'.jpeg'];
    fullFileName2 = fullfile(folderOut, filename2);
    saveas(zfig,fullFileName2)

hold off