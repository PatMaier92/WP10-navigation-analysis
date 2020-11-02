function [a1_x1,a1_y1,a1_x2,a1_y2,a1_x3,a1_y3,a1_x4,a1_y4,...
    a2_x1,a2_y1,a2_x2,a2_y2,a2_x3,a2_y3,a2_x4,a2_y4,...
    a3_x1,a3_y1,a3_x2,a3_y2,a3_x3,a3_y3,a3_x4,a3_y4,...
    a4_x1,a4_y1,a4_x2,a4_y2,a4_x3,a4_y3,a4_x4,a4_y4,...
    a5_x1,a5_y1,a5_x2,a5_y2,a5_x3,a5_y3,a5_x4,a5_y4,...
    p1_x,p1_y,p2_x_ne, p2_y_ne, p3_x_se, p3_y_se, p4_x_sw, p4_y_sw, p5_x_nw, p5_y_nw]=sm_wp10_corner(xmin,xmax,ymin,ymax)

    %Alley 1
    a1_x1=346.700; a1_x2=353.300; a1_x3= 353.30; a1_x4= 346.700; % Begin upper left
    a1_y1=381.700; a1_y2=381.700; a1_y3= 362.800; a1_y4= 362.800;           
    %Alley 2
    a2_x1=379.100; a2_x2=381.100; a2_x3= 363.200; a2_x4= 361.100; % Begin top-corner
    a2_y1=362.900; a2_y2=356.700; a2_y3= 350.700; a2_y4= 357.200;
    %Alley 3
    a3_x1=371.300; a3_x2=366.000; a3_x3=354.900; a3_x4=360.300;
    a3_y1=326.300; a3_y2=322.400; a3_y3=337.600; a3_y4=341.600; 
    %Alley 4
    a4_x1=334.000; a4_x2=328.700;  a4_x3=339.700; a4_x4=345.100;
    a4_y1=322.400; a4_y2=326.300;  a4_y3=341.600; a4_y4=337.600;     
    %Alley 5
    a5_x1=318.900; a5_x2=320.900; a5_x3= 338.900; a5_x4= 336.800;
    a5_y1=356.700; a5_y2=362.900; a5_y3= 357.200; a5_y4= 350.700; 
  
%%%%%%%%%% Provide Coordinates of inner pentagon %%%%%%%%%%%%%%%%%%%%
    p1_x=350.000; p1_y= 360.000; % Top
    p2_x_ne= 359.600; p2_y_ne= 353.100; % Northeast
    p3_x_se= 355.900; p3_y_se= 341.800; % Southeast
    p4_x_sw= 344.100; p4_y_sw= 341.800; % Southwest
    p5_x_nw= 340.4; p5_y_nw= 353.100; % Northwest
    
% Coordinates for outer pentagon (same as the ends of the alleys)    

% Data-normalization
        %alleys
    a1_x1=datanorm(a1_x1,xmin,xmax); a1_x2=(a1_x2- xmin)/(xmax-xmin); a1_x3=(a1_x3- xmin)/(xmax-xmin); a1_x4=(a1_x4- xmin)/(xmax-xmin);
    a1_y1=(a1_y1- ymin)/(ymax-ymin); a1_y2=(a1_y2- ymin)/(ymax-ymin); a1_y3=(a1_y3- ymin)/(ymax-ymin); a1_y4=(a1_y4- ymin)/(ymax-ymin);    
    
    a2_x1=(a2_x1- xmin)/(xmax-xmin); a2_x2=(a2_x2- xmin)/(xmax-xmin); a2_x3=(a2_x3- xmin)/(xmax-xmin); a2_x4=(a2_x4- xmin)/(xmax-xmin);
    a2_y1=(a2_y1- ymin)/(ymax-ymin); a2_y2=(a2_y2- ymin)/(ymax-ymin); a2_y3=(a2_y3- ymin)/(ymax-ymin); a2_y4=(a2_y4- ymin)/(ymax-ymin);
    
    a3_x1=(a3_x1- xmin)/(xmax-xmin); a3_x2=(a3_x2- xmin)/(xmax-xmin); a3_x3=(a3_x3- xmin)/(xmax-xmin); a3_x4=(a3_x4- xmin)/(xmax-xmin);
    a3_y1=(a3_y1- ymin)/(ymax-ymin); a3_y2=(a3_y2- ymin)/(ymax-ymin); a3_y3=(a3_y3- ymin)/(ymax-ymin); a3_y4=(a3_y4- ymin)/(ymax-ymin);
    
    a4_x1=(a4_x1- xmin)/(xmax-xmin); a4_x2=(a4_x2- xmin)/(xmax-xmin); a4_x3=(a4_x3- xmin)/(xmax-xmin); a4_x4=(a4_x4- xmin)/(xmax-xmin);
    a4_y1=(a4_y1- ymin)/(ymax-ymin); a4_y2=(a4_y2- ymin)/(ymax-ymin); a4_y3=(a4_y3- ymin)/(ymax-ymin); a4_y4=(a4_y4- ymin)/(ymax-ymin);
    
    a5_x1=(a5_x1- xmin)/(xmax-xmin); a5_x2=(a5_x2- xmin)/(xmax-xmin); a5_x3=(a5_x3- xmin)/(xmax-xmin); a5_x4=(a5_x4- xmin)/(xmax-xmin);
    a5_y1=(a5_y1- ymin)/(ymax-ymin); a5_y2=(a5_y2- ymin)/(ymax-ymin); a5_y3=(a5_y3- ymin)/(ymax-ymin); a5_y4=(a5_y4- ymin)/(ymax-ymin);
    
    % inner pentagon
    p1_x=(p1_x-xmin)/(xmax-xmin); p2_x_ne=(p2_x_ne-xmin)/(xmax-xmin); p3_x_se=(p3_x_se-xmin)/(xmax-xmin); p4_x_sw=(p4_x_sw-xmin)/(xmax-xmin); p5_x_nw=(p5_x_nw-xmin)/(xmax-xmin); 
    p1_y=(p1_y-ymin)/(ymax-ymin); p2_y_ne=(p2_y_ne-ymin)/(ymax-ymin); p3_y_se=(p3_y_se-ymin)/(ymax-ymin); p4_y_sw=(p4_y_sw-ymin)/(ymax-ymin); p5_y_nw=(p5_y_nw-ymin)/(ymax-ymin); 
end