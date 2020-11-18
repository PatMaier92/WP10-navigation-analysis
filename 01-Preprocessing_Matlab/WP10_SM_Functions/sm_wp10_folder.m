function [folderIn,folderOut]=sm_wp10_folder(dataFolder,sub,finalFolderString)
% SM_WP10_FOLDER Returns individual folder paths for input and output.
% Creates output folder, if it does not exist. 
%
% Input:
% dataFolder is path to all data.
% sub is participant identifier.
% finalFolderString is session information.
%
% Returns: folderIn,folderOut are folder paths to individual data files. 

Sub=num2str(sub);

invalidFolder=true;
while invalidFolder
    folderIn=[dataFolder '\' Sub '\' finalFolderString];
    invalidFolder=false;
    if ~exist(folderIn, 'dir')
        sub=sub+1;
        Sub=num2str(sub);
        invalidFolder=true;
    end
end

folderOut=[dataFolder '\' Sub '\results'];
if ~exist(folderOut, 'dir')
    mkdir(folderOut);
    disp('Your outputfolder for the subject didn''t exist, a new folder was created')
end

end