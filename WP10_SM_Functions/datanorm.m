% Starmaze 
% Data preparation
% functions

% Data-normalization
    % data normalization for coordinates
    function DN=datanorm(c,cmin,cmax)
          DN=((c-cmin)/(cmax-cmin));

