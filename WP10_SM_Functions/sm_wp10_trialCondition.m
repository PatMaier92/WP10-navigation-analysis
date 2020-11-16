function [TC]=sm_wp10_trialCondition(trial_type,feedback)
% SM_WP10_TRIALCONDITION Function for assinging trial condition 
% for Starmaze WP10.
%
% Input: 
% trial_type is trial condition information (string) 
% feedback indicates whether goal is visible (yes/no) (boolean)
%
% Returns: TC is trial type (integer)
% 0 = main_learn, 3 = main_retrieval, 1 = allocentric, 2 = egocentric

training='main_learn';
% note: main_learn + no Feedback (simple retrieval) is not marked as
% extra category in Starmaze WP10 output files yet.
allo='main_allo';
ego='main_ego';

TC_training=contains(trial_type,training);
TC_allo=contains(trial_type,allo);
TC_ego=contains(trial_type,ego);

if TC_training==1
    if feedback==0
        TC=3;
    else
        TC=0;
    end
elseif TC_allo==1
    TC=1;
elseif TC_ego==1
    TC=2;
end

end
