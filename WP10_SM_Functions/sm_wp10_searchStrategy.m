function [direct_run, egocentric, allocentric, ...    
    reoriented,serial,centralFocus,random,unclassified,...
    search_success_no, search_strategy_no]=sm_wp10_searchStrategy(trial_condition, ...
    direct_path,success,success_ego,path_score,arm_score,arm_explored, ...
    rel_pentagon_zone,alley_entry_out)
% SM_WP10_SEARCHSTRATEGY Determine search strategies in Starmaze WP10. 
%
% Input: Different variables indicating trial exploration behavior. 
% 
% Returns: Boolean values for different search strategy options and two
% search strategy integer. 
% % Search succes: 0 = Failed, 1 = Success non-optimal, 2 = Success optimal
% % Search strategy: 
% 0 = direct run in learning trial (egocentric + allocentric components)
% 1 = egocentric 
% 2 = allocentric
% 3 = reoriented
% 4 = central focus
% 5 = serial 
% 6 = random
% 7 = unclassified 

% set default to 'false'
direct_run=0; egocentric=0; allocentric=0; 
reoriented=0; serial=0; centralFocus=0; random=0; 
unclassified=0;

% evaluate exploration behavior 
if success == 0
    search_success_no = 0; % failed
    % add options: reoriented, centralfocus, random
    if success_ego==1 && arm_explored==2 %% plus exclude central focus crit
        egocentric=1;
        search_strategy_no=1; 
    elseif arm_explored >=3 && sum(alley_entry_out) > 2 && rel_pentagon_zone <= 0.6
        serial=1;
        search_strategy_no=5;
    else 
        unclassified=1;
        search_strategy_no=7;
    end  
else 
    if direct_path == 1
        search_success_no = 2; % optimal
        if trial_condition==1
            allocentric=1;
            search_strategy_no = 2; 
        elseif trial_condition==2
            egocentric=1;
            search_strategy_no = 1; 
        else 
            direct_run = 1;
            search_strategy_no = 0; 
        end
    else 
        search_success_no = 1; % non-optimal
        % options: reoriented, centralfocus, serial, random,
        % unclassified
    end
end

% if direct_path==0 && success==0 && success_ego==1 && arm_score==2
%     egocentric=1;
%     search_strategy_no=2;
% elseif direct_path==0 && success ==1 && arm_score <=3 && path_score <=13 && sum(alley_zone_out)==0
%     reoriented=1;
%     search_strategy_no=2;
% elseif rel_pentagon_zone <= 0.6 && arm_score >=4 && (alley_zone_out(1) || alley_zone_out(2) || alley_zone_out(3) || alley_zone_out(5)) >=0
%     serial=1;
%     search_strategy_no=3;
% elseif rel_pentagon_zone >= 0.6 && arm_score <=3
%     centralFocus=1;
%     search_strategy_no=4;
% elseif path_score >=15 && arm_score >=4
%     random=1;
%     search_strategy_no=5;
% elseif success==0
%     search_strategy_no=7;
% else
%     unclassified=1;
%     search_strategy_no=6;
% end

end