function [group,Group]=sm_wp10_callGroup(id)

    if (id <=11999 && id>=10000) %% CHECK
            group=0;
            Group='YoungKids';
    elseif (id <=12999 && id>=12000)
            group=1;
            Group='OldKids';
    elseif (id <=13999 && id>=13000) %% CHECK
            group=2;
            Group='YoungAdults';
    elseif (id <=14999 && id>=14000) %% CHECK
            group=3;
            Group='OldAdults';      
    else
            group=100;
            Group='F';
            disp('The provided id is out of limits. Group is set to "F"')
    end

end
