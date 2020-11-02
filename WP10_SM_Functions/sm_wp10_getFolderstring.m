function [finalFolderString]=sm_wp10_getFolderstring(session)

     if session==1
        finalFolderString='S001';
     elseif session==2
        finalFolderString='S002';
     elseif session==3
        finalFolderString='S003';
     else
         disp('Wrong input')
     end
end

