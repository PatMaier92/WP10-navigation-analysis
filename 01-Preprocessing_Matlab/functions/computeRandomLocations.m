%% Create a random set of coordinates inside Starmaze WP10
    
    % define parameters
    n = 10000; % number of points
    R = 0.5; % radius of a circle
    x0 = 0.5; % center of a circle
    y0 = 0.5; % center of a circle
    
    % create random set of points in a circle (seed=1)
    t = 2*pi*rand(n,1);
    r = R*sqrt(rand(n,1));
    x = x0 + r.*cos(t);
    y = y0 + r.*sin(t);

    % find those points that overlap with the Starmaze shape 
    union_poly = union(sm.coord.full_poly);   
    in = inpolygon(x,y,union_poly.Vertices(:,1),union_poly.Vertices(:,2)); 
    x_in = x(in); 
    y_in = y(in);
           
    % reduce data points to 1000
    n = numel(x(in)); 
    in2 = sort(randperm(n,1000)); 
    x_in = x_in(in2); 
    y_in = y_in(in2); 
    
    % check in plot
    plot(sm.coord.full_poly); hold on; 
    plot(x_in,y_in, 'ro', 'MarkerSize', 5);
    hold off; 