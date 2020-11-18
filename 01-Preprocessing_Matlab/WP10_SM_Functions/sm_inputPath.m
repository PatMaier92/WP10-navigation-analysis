function [data_path]=sm_inputPath()
% SM_INPUTPATH Takes an input path and checks for its validity. 
%
% Input: 
% e.g D:\Temp Home Office\Analysis\Functions.
% 
% Returns: data_path is saved path. 

invalidPath = true;
while invalidPath
    data_path = input('Enter input folder, where all data is located: ','s');
    if ~exist(data_path, 'dir')
        disp('Your input folder does not exist. Enter a valid folder.')
    else
        invalidPath = false;
    end
end

end

