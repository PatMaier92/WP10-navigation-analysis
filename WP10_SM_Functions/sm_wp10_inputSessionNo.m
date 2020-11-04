function [sessionNo]=sm_wp10_inputSessionNo()
% SM_WP10_INPUTSESSION Takes a number and returns it as integer session
% information. 
%
% Input: 
% A number, e.g. 1, 2, 3, 99 but not "eins"
% 
% Returns: sessionNo (integer) 

invalidSession = true;
while invalidSession
    sessionNo = str2num(input('Enter total number of sessions: ','s'));
    if isempty(sessionNo)
        disp('Your did not enter a valid number. Please enter an integer.')
    else
        invalidSession = false;
    end
end

end

