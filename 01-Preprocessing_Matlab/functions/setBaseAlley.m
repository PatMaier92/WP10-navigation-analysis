function [base_alley]=setBaseAlley(start_alley, goal_alley, ego_alley)
% setBaseAlley: Returns alley that is not goal, start, original start and egocentric alley.  
%
% Input: 
% start_alley, goal_alley, ego_alley (integer)
%
% Returns: 
% base_alley (integer) 

alleys=[1 3 5 7 9];
alleys(alleys==7)=[]; 
alleys(alleys==start_alley)=[];
alleys(alleys==goal_alley)=[];
alleys(alleys==ego_alley)=[];

base_alley=alleys; 

end
