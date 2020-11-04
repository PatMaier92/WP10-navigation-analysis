% UNUSED 

function [p1_x,p1_y,p2_x_ne, p2_y_ne, p3_x_se, p3_y_se, p4_x_sw, p4_y_sw, p5_x_nw, p5_y_nw]=sm_wp10_pentagon_corner(xmin,xmax,ymin,ymax)  

disp('The function sm_wp10_pentagon_corner is deactivated'); 

Provide Coordinates of inner pentagon
    p1_x=350.000; p1_y= 360.000; % Top
    p2_x_ne= 359.600; p2_y_ne= 353.100; % Northeast
    p3_x_se= 355.900; p3_y_se= 341.800; % Southeast
    p4_x_sw= 344.100; p4_y_sw= 341.800; % Southwest
    p5_x_nw= 340.4; p5_y_nw= 353.100; % Northwest
     
data-normalization
    p1_x=(p1_x-xmin)/(xmax-xmin); p2_x_ne=(p2_x_ne-xmin)/(xmax-xmin); p3_x_se=(p3_x_se-xmin)/(xmax-xmin); p4_x_sw=(p4_x_sw-xmin)/(xmax-xmin); p5_x_nw=(p5_x_nw-xmin)/(xmax-xmin); 
    p1_y=(p1_y-ymin)/(ymax-ymin); p2_y_ne=(p2_y_ne-ymin)/(ymax-ymin); p3_y_se=(p3_y_se-ymin)/(ymax-ymin); p4_y_sw=(p4_y_sw-ymin)/(ymax-ymin); p5_y_nw=(p5_y_nw-ymin)/(ymax-ymin); 

end