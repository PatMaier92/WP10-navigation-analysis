function [data_path]=sm_inputPath()

%% input path
    invalidPath = true;
    while invalidPath
      data_path = input('Enter data folder: ','s');
         if ~exist(data_path, 'dir')
            disp('Your input folder does not exist. Enter a valid folder.')
         else
             invalidPath = false;
         end
    end
    
end

