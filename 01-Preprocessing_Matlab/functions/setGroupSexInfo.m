function [group,group_string,sex,sex_string]=setGroupSexInfo(id)
% setGroupSexInfo: Assigns group and sex information based on
% ID integer. Unfamiliar IDs are set to group 999, Group "F", sex 0, Sex "Other".  
%
% Input: 
% ID value(integer).
%
% Returns: group (integer) and Group (string) information,  
% sex (integer) and Sex (string) information. 

% group info 
if (id <=11999 && id>=11000) || (id <=21999 && id>=21000)
    group=1;
    group_string='YoungKids';
elseif (id <=12999 && id>=12000) || (id <=22999 && id>=22000)
    group=2;
    group_string='OldKids';
elseif (id <=15999 && id>=15000) || (id <=25999 && id>=25000)
    group=5;
    group_string='YoungAdults';
elseif (id <=16999 && id>=16000) || (id <=26999 && id>=26000)
    group=6;
    group_string='OldAdults';
else
    group=999;
    group_string='F';
    disp('ID is out of limits. Group is set to "F"')
end

% sex info
idstring = num2str(id);
if idstring(1) == '1'
    sex = 1;
    sex_string = 'Male';
elseif idstring(1) == '2'
    sex = 2;
    sex_string = 'Female';
else 
    sex = 0;
    sex_string = 'Other';
    disp('ID is out of limits. Sex is set to "Other"')
end
    
end
