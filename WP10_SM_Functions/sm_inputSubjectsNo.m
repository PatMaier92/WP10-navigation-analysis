function [subject_start,subject_end]=sm_inputSubjectsNo()

%% 1st subject
    invalidSubjectStartNo = true;
    while invalidSubjectStartNo
      subject_start = str2num(input('Enter first subjectNo: ','s'));
         if ~isreal(subject_start)
            clear('subject_start');
         else
             invalidSubjectStartNo = false;
         end
    end

%% last subject
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

