function [group,Group,sex,Sex]=sm_wp10_callGroup(id)
% SM_WP10_CALLGROUP Assigns group information and sex information based on
% ID integer. Unfamiliar IDs are set to group 999, Group "F" and sex 0, Sex "Other".  
%
% Input: 
% ID value(integer). Is converted to string in this function. 
%
% Returns: group (integer) and Group (string) information,  
% sex (integer) and Sex (string) information. 

% Group info 
if (id <=11999 && id>=11000) || (id <=21999 && id>=21000)
    group=1;
    Group='YoungKids';
elseif (id <=12999 && id>=12000) || (id <=22999 && id>=22000)
    group=2;
    Group='OldKids';
elseif (id <=15999 && id>=15000) || (id <=25999 && id>=25000)
    group=5;
    Group='YoungAdults';
elseif (id <=16999 && id>=16000) || (id <=26999 && id>=26000)
    group=6;
    Group='OldAdults';
else
    group=999;
    Group='F';
    disp('The ID is out of limits. Group is set to "F"')
end

% Sex info
idstring = num2str(id);
if idstring(1) == '1'
    sex = 1;
    Sex = 'Male';
elseif idstring(1) == '2'
    sex = 2;
    Sex = 'Female';
else 
    sex = 0;
    Sex = 'Other';
    disp('The ID is out of limits. Sex is set to "Other"')
end
    
end
