    %% wp10 simulator: distance to ideal path ('integral')
     
    % actual path longer than ideal one
    xy=[x,y]; % longer
    xi_yi=[xi_al, yi_al]; % shorter
    [~,dist] = dsearchn(xi_yi, xy); 
    fd=sm_distance(xy(end,1), xi_yi(end,1), xy(end,2), xi_yi(end,2)); % final distance
    dist = [dist; fd];
    sum(dist)
    mean(dist)
    
    % actual path shorter than ideal one 
    s2=round(data_length/3); 
    xy2=[x(1:s2),y(1:s2)]; % 2/3 shorter
    [~,dist2] = dsearchn(xi_yi, xy2); 
    fd2=sm_distance(xy2(end,1), xi_yi(end,1), xy2(end,2), xi_yi(end,2)); 
    dist2 = [dist2; fd2]; 
    sum(dist2)
    mean(dist2)
    
    s3=round(data_length/2); 
    xy3=[x(1:s3),y(1:s3)]; % 1/2 shorter
    [~,dist3] = dsearchn(xi_yi, xy3); 
    fd3=sm_distance(xy3(end,1), xi_yi(end,1), xy3(end,2), xi_yi(end,2)); 
    dist3 = [dist3; fd3]; 
    sum(dist3)
    mean(dist3)
    
    s4=round(data_length/1.5); 
    xy4=[x(1:s4),y(1:s4)]; % even shorter
    [~,dist4] = dsearchn(xi_yi, xy4); 
    fd4=sm_distance(xy4(end,1), xi_yi(end,1), xy4(end,2), xi_yi(end,2)); 
    dist4 = [dist4; fd4]; 
    sum(dist4)
    mean(dist4)
    
    figure;
    plot(polyshape_array);
    hold on
    plot(xi_al, yi_al, 'k-', x, y, 'b-', xy4(:,1), xy4(:,2), 'y-', xy3(:,1), xy3(:,2), 'g-', xy2(:,1), xy2(:,2), 'r-');
    xlim([0 1]);
    ylim([0 1]);
    hold off
    
    figure; 
    plot(dist,'b-'); 
    hold on; 
    plot(dist4,'y-'); 
    plot(dist3,'g-'); 
    plot(dist2,'r-'); 
    hline = refline(0, mean(dist));
    hline.Color = 'b';
    hline = refline(0, mean(dist2));
    hline.Color = 'r';
    hline = refline(0, mean(dist3));
    hline.Color = 'g';
    hline = refline(0, mean(dist4));
    hline.Color = 'y';
    hold off; 
    
    
    % cleaned path (removes waiting/rotating at one spot)
    xy_unique=unique(xy,'rows','stable'); 
    len_unique=size(xy_unique,1); 
    
    [~,dist5] = dsearchn(xi_yi, xy_unique); 
    dist5 = [dist5; fd]; 
    sum(dist5)
    mean(dist5)
    
    figure;
    plot(polyshape_array);
    hold on
    plot(xi_al, yi_al, 'k-', xy_unique(:,1), xy_unique(:,2), 'b+');
    xlim([0 1]);
    ylim([0 1]);
    hold off
    
    figure; 
    plot(dist,'b-'); 
    hold on; 
    plot(dist5,'y-');  
    hline = refline(0, mean(dist));
    hline.Color = 'b';
    hline = refline(0, mean(dist5));
    hline.Color = 'y';
    hold off; 
    