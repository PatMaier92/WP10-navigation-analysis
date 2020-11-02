function [TC]=sm_trialCondition(wp,trial_type)

if wp==3 || wp==6 || wp==31
    training='training';
    allo='allo';
    ego='ego';
elseif wp==10
    training='main_learn';
    allo='main_allo';
    ego='main_ego';
end

TC_training=contains(trial_type,training);
TC_allo=contains(trial_type,allo);
TC_ego=contains(trial_type,ego);

if TC_training==1
    TC=0;
elseif TC_allo==1
    TC=1;
elseif TC_ego==1
    TC=2;
end
end
