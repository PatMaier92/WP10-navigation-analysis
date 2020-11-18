function [subject_start,subject_end]=sm_inputSubjectsNo()
% SM_INPUTSUBJECT Takes two integers as input. Returns them as seperate
% variables. 
%
% Input: 
% e.g. 1111, 1120
%
% Returns: subject_start, subject_end. 

% first subject
invalidSubjectStartNo = true;
while invalidSubjectStartNo
    subject_start = str2num(input('Enter first subjectNo: ','s'));
    if ~isreal(subject_start)
        clear('subject_start');
    else
        invalidSubjectStartNo = false;
    end
end

% last subject
invalidSubjectEndNo = true;
while invalidSubjectEndNo
    subject_end = str2num(input('Enter last subjectNo: ','s'));
    if ~isreal(subject_start)
        clear('subject_start');
    else
        invalidSubjectEndNo = false;
    end
end

end

