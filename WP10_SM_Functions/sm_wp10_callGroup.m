function [group,Group]=sm_wp10_callGroup(id)

    if (id <=11999 && id>=10000) % group 0=Control, 1=Experimentalgroup
            group=1;
            Group='Dev';
    elseif (id <=12999 && id>=12000)
            group=0;
            Group='Control';
    else
            group=100;
            Group='F';
            disp('The provided id is out of limits. Group is set to "F"')
    end

end
