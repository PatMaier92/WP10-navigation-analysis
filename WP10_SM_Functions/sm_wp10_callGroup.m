function [group,Group]=sm_wp10_callGroup(id)
% SM_WP10_CALLGROUP Assigns group information as string and integer based on
% ID integer. Unfamiliar IDs are set to group 100, Group "F". 
%
% Input: 
% ID value(integer).
%
% Returns: group (integer) and Group (string) information. 

if (id <=11999 && id>=10000) 
    group=1;
    Group='YoungKids';
elseif (id <=12999 && id>=12000)
    group=2;
    Group='OldKids';
elseif (id <=15999 && id>=15000) 
    group=5;
    Group='YoungAdults';
elseif (id <=16999 && id>=16000) 
    group=6;
    Group='OldAdults';
else
    group=999;
    Group='F';
    disp('The provided id is out of limits. Group is set to "F"')
end

end
