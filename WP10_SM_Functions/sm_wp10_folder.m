function [folderIn,folderOut]=sm_wp10_folder(dataFolder,sub,finalFolderString)
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