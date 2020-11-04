function [direct_run,reoriented,serial,centralFocus,random_search,unclassified,failed_strategy...
    allocentric,egocentric,search_strategy_no]=sm_wp10_searchStrategy(trial_condition,direct_path,success,success_ego,...
        path_score,exploration,rel_pentagon_zone, alley_zone_out)
% SM_WP10_SEARCHSTRATEGY Determine search strategies in Starmaze WP10. 
%
% Input: Different variables indicating trial exploration behavior. 
% 
% Returns: Boolean values for different search strategy options. 

% set default to 'false'
allocentric=0; egocentric=0; 
direct_run=0; reoriented=0; serial=0; 
centralFocus=0; random_search=0; unclassified=0;
failed_strategy=0;

% evaluate exploration behavior 
if direct_path==1
    direct_run=1;
    search_strategy_no=1;
    if trial_condition==1
        allocentric=1;
    elseif trial_condition==2
        egocentric=1;
    end
elseif direct_path==0 && success==0 && success_ego==1 && exploration==2
    egocentric=1;
    search_strategy_no=2;
elseif direct_path==0 && success ==1 && exploration <=3 && path_score <=13 && sum(alley_zone_out)==0
    reoriented=1;
    search_strategy_no=2;
elseif rel_pentagon_zone <= 0.6 && exploration >=4 && (alley_zone_out(1) || alley_zone_out(2) || alley_zone_out(3) || alley_zone_out(5)) >=0
    serial=1;
    search_strategy_no=3;
elseif rel_pentagon_zone >= 0.6 && exploration <=3
    centralFocus=1;
    search_strategy_no=4;
elseif path_score >=15 && exploration >=4
    random_search=1;
    search_strategy_no=5;
elseif success==0
    failed_strategy=1;
    search_strategy_no=7;
else
    unclassified=1;
    search_strategy_no=6;
end

end