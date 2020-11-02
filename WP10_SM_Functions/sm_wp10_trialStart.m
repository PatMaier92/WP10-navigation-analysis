function [start]=sm_wp10_trialStart(trial_start)

Start1='Player_MA';
Start2='Player_MB';
Start3='Player_MC';
Start4='Player_MD';
Start5='Player_ME';
Start6='Player_MF';
Start7='Player_MG';
Start8='Player_MH';
Start9='Player_MI';
Start10='Player_MJ';    
Start11='Player_MX';


start_1=contains(trial_start,Start1);
start_2=contains(trial_start,Start2);
start_3=contains(trial_start,Start3);
start_4=contains(trial_start,Start4);
start_5=contains(trial_start,Start5);
start_6=contains(trial_start,Start6);
start_8=contains(trial_start,Start8);
start_9=contains(trial_start,Start9);
start_10=contains(trial_start,Start10);

if contains(trial_start,Start7) || contains(trial_start,Start11)
    start_7=1;
else
    start_7=0;
end

if start_1==1
    start=1;
elseif start_2==1
    start=2;
elseif start_3==1
    start=3;
elseif start_4==1
    start=4;
elseif start_5==1
    start=5;
elseif start_6==1
    start=6;
elseif start_7==1
    start=7;
elseif start_8==1
    start=8;
elseif start_9==1
    start=9;
elseif start_10==1
    start=10;
end

end