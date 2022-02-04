function [TC]=setTrialCondition(condition,feedback)
% setTrialCondition Assigns trial condition for Starmaze WP10.
%
% Input: 
% condition is trial condition information (string) 
% feedback indicates whether goal is visible (yes/no) (boolean)
%
% Returns: TC is trial type (integer)
% 0 = main_learn, 3 = main_retrieval, 1 = allocentric, 2 = egocentric, 
% 4 = practise motor control

training='main_learn';
% note: main_learn + no feedback (simple retrieval) is not marked as
% extra category in Starmaze WP10 output files yet.
allo='main_allo';
ego='main_ego';
mc='practise_motor'; 

TC_training=contains(condition,training);
TC_allo=contains(condition,allo);
TC_ego=contains(condition,ego);
TC_mc=contains(condition,mc); 

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
elseif TC_mc==1
    TC=4;
else
    TC=999;
end

end
