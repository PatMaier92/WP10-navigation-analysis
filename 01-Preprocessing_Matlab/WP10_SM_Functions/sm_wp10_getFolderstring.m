function [finalFolderString]=sm_wp10_getFolderstring(session)
% SM_WP10_GETFOLDERSTRING Takes session integer and returns
% correct folder information for this session. 
%
% Input:
% session (integer)
%
% Returns: finalFolderString (string) is folder information for this session. 

if session==1
    finalFolderString='S001';
elseif session==2
    finalFolderString='S002';
elseif session==3
    finalFolderString='S003';
elseif session==4
    finalFolderString='S004'; % Session 4 PostTests require different data analysis
else
    disp('Wrong input')
end

end

