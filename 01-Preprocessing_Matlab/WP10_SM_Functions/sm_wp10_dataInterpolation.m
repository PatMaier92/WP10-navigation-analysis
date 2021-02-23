function [xi_al, yi_al, xi_eg, yi_eg]=sm_wp10_dataInterpolation(interpol_crit,...
    x_line, y_line, x_line_ego, y_line_ego)
% SM_WP10_DATAINTERPOLATION Interpolating data for Starmaze WP10 depending
% on path line vectors. 
%
% Input:
% interpol_crit is interpolation criterium 
% x_line,y_line,x_line_ego,y_line_ego are vectors with x-/y-coordinates
%
% Returns:
% xi_al,yi_al,xi_eg,yi_eg are vectors with interpolated x-/y-coordinates

% interpolate data 
xi_al=[]; 
yi_al=[]; 
for i=1:length(x_line)-1
    if x_line(i) < x_line(i+1)
        tx=x_line(i):interpol_crit:x_line(i+1); 
    else
        tx=x_line(i+1):interpol_crit:x_line(i);
        tx=flip(tx); 
    end
    xi_al=[xi_al tx]; 
    yi_al=[yi_al interp1(x_line(i:i+1),y_line(i:i+1),tx,'linear')];
end 

xi_eg=[]; 
yi_eg=[]; 
for i=1:length(x_line_ego)-1
    if x_line_ego(i) < x_line_ego(i+1)
        tx=x_line_ego(i):interpol_crit:x_line_ego(i+1); 
    else
        tx=x_line_ego(i+1):interpol_crit:x_line_ego(i);
        tx=flip(tx); 
    end
    xi_eg=[xi_eg tx]; 
    yi_eg=[yi_eg interp1(x_line_ego(i:i+1),y_line_ego(i:i+1),tx,'linear')];
end 

end