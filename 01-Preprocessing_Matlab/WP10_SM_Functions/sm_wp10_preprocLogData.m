function [rand_dict]=sm_wp10_preprocLogData(log_data, subject, pstr, sstr)
% SM_WP10_PREPROCLOGDATA Preprocessing of log data with information on
% randomization of goals and starts.
%
% Input: cleaned log_data (cell structure) from csv. file
%
% Returns: rand_dict (structure) contains randomization info for each
% participant, session and goal location. 

if size(log_data,1) ~= 4
    disp('Error in log data extraction.');
else
    for i=1:4
        line=split(log_data(i),' ');
        line=line(~cellfun('isempty',line)); 
        
        % id and key
        id=str2double(line{3});
        if id ~= subject
            disp('Error: ID mismatch during log data randomization check.');
            break
        end
        key=line{5};
        
        % rest of information 
        inner_dict={};
        line=line(7:end); 
        for n=1:2:14
            k=line{n}; v=line{n+1};           
            inner_dict.(k)=v;
        end
        
        % add T1 recall and T2 goal order (hard-coded in Unity)
        if str2double(inner_dict.T1)==1
            inner_dict.T1_R=3;
            inner_dict.T2_R=2; 
        elseif str2double(inner_dict.T1)==2
            inner_dict.T1_R=1;
            inner_dict.T2_R=3;
        elseif str2double(inner_dict.T1)==3
            inner_dict.T1_R=2;
            inner_dict.T2_R=1;
        elseif str2double(inner_dict.T1)==0
            inner_dict.T1_R=0;
            inner_dict.T2_R=0;
        end
        
        % collect data
        rand_dict.(pstr).(sstr).(key)=inner_dict;
    end
end

end