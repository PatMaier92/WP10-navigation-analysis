function [start_x,start_y]=sm_wp10_start(start,xmin,xmax,ymin,ymax)

    [row,col]=size(start);
    % Data-normalization goal position
    for r=1:row
    start_x(r,1)=datanorm(start(r,1),xmin,xmax);
    start_y(r,1)= datanorm(start(r,2),ymin,ymax);
    end
end