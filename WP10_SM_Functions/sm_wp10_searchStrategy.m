function [direct, egocentric, allocentric, failed, ...    
    reoriented, serial, centralFocus, random, unclassified, ...
    search_strategy_no]=sm_wp10_searchStrategy(trial_condition, ...
    direct_path, success, success_ego, arm_explored, path_explored, path_score,...
    rel_pentagon_zone, alley_entry, rectangle_entry, triangle_entry)
% SM_WP10_SEARCHSTRATEGY Determine search strategies in Starmaze WP10. 
%
% Input: Different variables indicating trial exploration behavior. 
% 
% Returns: Boolean values for different search strategy options and one
% search strategy integer. 
%
% % Search strategy integer: 
% 1 = direct strategy 
% -- allocentric (only in allocentric trials)
% -- egocentric (in egocentric trials, i.e. correct)
% -- egocentric (in allocentric trials, i.e. incorrect)
% -- unclear, either failed or not failed (in all trials) 
% 2 = central focus
% 3 = reoriented
% 4 = serial 
% 5 = random 
% 6 = unclassified 

% set default to 'false'
direct=0; egocentric=0; allocentric=0; 
reoriented=0; serial=0; centralFocus=0; random=0; failed=0; 
unclassified=0; search_strategy_no=6; % default strategy: unclassified

% get integers for re-entries in zones 
doubleAlley=0;
noAlley=0;
for i=1:length(alley_entry)
    if alley_entry(i) > 1
        doubleAlley=doubleAlley+1; 
    elseif alley_entry(i)==0
        noAlley=noAlley+1;
    end
end

doubleRect=0;
noRect=0;
for i=1:length(rectangle_entry)
    if rectangle_entry(i) > 1
        doubleRect=doubleRect+1;
    elseif rectangle_entry(i)==0
        noRect=noRect+1;
    end
end

doubleTri=0;
noTri=0; 
for i=1:length(triangle_entry)
    if triangle_entry(i) > 1
        doubleTri=doubleTri+1;
    elseif triangle_entry(i)==0
        noTri=noTri+1;
    end
end

% copy success 
if success==0
    failed=1; 
end

% evaluate exploration behavior 
if path_explored==path_score % did not re-enter same area: direct or central focus
    if direct_path==1 % shortest path to correct goal
        search_strategy_no=1; % 1=direct strategy
        direct=1; 
        if trial_condition==1 
            allocentric=1; % sub: allocentric 
        elseif trial_condition==2
            egocentric=1; % sub: egocentric 
        end
    elseif direct_path==0 % not shortest path or not correct goal
        if success==0 && success_ego==0 && path_explored <=9
            search_strategy_no=1; % 1=direct strategy, but sub: failed
            direct=1;
        elseif success==0 && success_ego==1 && path_explored <=9
            search_strategy_no=1; % 1=direct strategy, but sub: failed
            egocentric=1; % sub: egocentric
            direct=1;
        else
            search_strategy_no=2; % 2=central_focus
            centralFocus=1; % sub: central focus
        end
    end
else % did re-enter same area: central, serial, random or reoriented
    if arm_explored<=3 && doubleAlley==0 && rel_pentagon_zone >= 0.6 && ... % focus in inner pentagon
            (doubleRect==0 || noRect==0) % either no rect re-entered (no reorientation)
                                         % or at least one full circle (all rects entered once)
        search_strategy_no=2; % 2=central_focus
        centralFocus=1; % sub: central focus    
    elseif arm_explored>=3 ... % at least 3 arms visited
            && doubleTri > 0 && doubleRect==0 && doubleAlley==0 % at least one triangle re-entered for turning 
                                                                % no re-entries in rect or alleys
        search_strategy_no=4; %4=serial
        serial=1; % sub: serial
    elseif arm_explored<=3 && ... % maximum 3 arms visited
            (doubleTri > 0 || doubleRect > 0) % at least one rect or tri re-entered
        search_strategy_no=3; %3=reoriented
        reoriented=1; % sub: reoriented  
    elseif arm_explored>=4 && path_score >= 15 % at least 4 arms visited
                                               % and lots of zones visited
        search_strategy_no=5; % 5=random
        random=1; % sub: random  
    else
        unclassified=1; % sub: unclassified
    end
end

end