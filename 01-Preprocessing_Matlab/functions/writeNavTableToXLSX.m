function writeNavTableToXLSX(data_folder)
% writeNavTableToXLSX Write data summary table in xlsx format. 
%
% Input: path to data folder 
%
% Returns: data summary table as xlsx file 

% load data 
file_path=[data_folder '\WP10_results']; 
file_name='\wp10_results_navigation.mat';
full_file=fullfile(file_path, file_name); 
if isfile(full_file)
    load(full_file, 'sm'); 
else 
    disp('Your input data does not exist. Please check your data folder.'); 
    return; 
end 
data=sm.participant; clear sm; 

% process data (from structure to table) 
temp=[];
[~,p]=size(data);
for i=1:p
    for j=1:length(data(i).session)
        [r,~]=size(struct2table(data(i).session(j).trial));
        g_data=table(repmat(data(i).id,r,1), string(repmat(data(i).group,r,1)), string(repmat(data(i).sex,r,1)), ...
            repmat(data(i).session(j).session_num,r,1), repmat(data(i).session(j).session_duration,r,1),...
            'VariableNames',{'id' 'group' 'sex' 'session' 'duration'}); 
        if j==3
            t_data=struct2table(data(i).session(j).trial(2)); 
            t_data.goal_s=[]; t_data.start_s=[];  g_data(1,:)=[]; 
            gt_data=[g_data t_data];
            temp=outerjoin(temp,gt_data,'MergeKeys',true);
        else
            t_data=struct2table(data(i).session(j).trial);
            gt_data=[g_data t_data]; 
            temp=vertcat(temp,gt_data);
        end
        clear *_data r;
    end
end

% sort order 
temp=sortrows(temp,{'id','session','trial'});

% write data 
format='yymmdd'; date=datestr(now, format); 
file_name2=['wp10_navigation_data_' date '.xlsx']; 
writetable(temp,fullfile(file_path,file_name2));

end