function [start_int]=sm_wp10_trialStart(trial_start, start_locs)
% SM_WP10_TRIALSTART Return start position information for this trial for Starmaze
% WP10. 
%
% Input: 
% trial_start ist start position (string).
% start_locs is ordered string array of starting points. 
%
% Returns: start position information (integer).

% integer
start_int=find(contains(start_locs, trial_start)); 

% correct for ego: same start_int as in learning 
if start_int==11 % MX
    start_int=7; % MG
end

% correct for false input 
if isempty(start_int)
    disp('Unknown starmaze input information in trialStart.m. Set to 999.');
    start_int=999; 
end

end