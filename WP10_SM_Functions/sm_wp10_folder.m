function [folderIn,folderOut]=sm_wp10_folder(sub,finalFolderString)
    Sub=num2str(sub);
    
    invalidFolder=true;
    while invalidFolder
        folderIn=['C:\Users\deetj\OneDrive\Dokumente\Wissenschaft\SFB1315\WP10_B04\WP10_data\' Sub '\' finalFolderString];
        invalidFolder=false;
        if ~exist(folderIn, 'dir')
            sub=sub+1;
            Sub=num2str(sub);
            invalidFolder=true;
        end  
    end
    
    folderOut=['C:\Users\deetj\OneDrive\Dokumente\Wissenschaft\SFB1315\WP10_B04\WP10_data\' Sub '\results'];
    if ~exist(folderOut, 'dir')
        mkdir(folderOut);
        disp('Your outputfolder for the subject didn''t exist, a new folder was created')
    end
    

end