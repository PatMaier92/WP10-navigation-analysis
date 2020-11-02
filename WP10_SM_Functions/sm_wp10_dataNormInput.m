function [xmin,ymin,xmax,ymax]=sm_wp10_dataNormInput(wp)
    if wp==3 || wp==2 || wp==6 || wp==31
        ymin= 307.000; ymax= 400.000;     xmin= 301.000; xmax=397.000;
        disp('For wp2,3,31,6 min-/max-values are set already.');
    elseif wp==10
                ymin= 315.000; ymax= 385.000;     xmin= 315.000; xmax=385.000;
        disp('For wp10 min-/max-values are set already.');
    else
        disp('Provide max/min-values for data-normalization.');
        try
            xmin=input('Enter xmin: ');
            xmax=input('Enter xmax: ');
            ymin=input('Enter ymin: ');
            ymax=input('Enter ymax: ');
        catch
            disp('Invalid input. Please reenter and use positive doubles.');
        end
    end
end