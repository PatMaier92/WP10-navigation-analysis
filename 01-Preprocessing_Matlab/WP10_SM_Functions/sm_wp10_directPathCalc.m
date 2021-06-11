function [ideal_no, match_abs, match_perc, final_match]=sm_wp10_directPathCalc(uniq_alley, uniq_rect,...
    alley_entry_mat, rectangle_entry_mat)
% SM_WP10_DIRECTPATHCALC Determines match to direct path 
% in Starmaze WP10. 
% 
% Input:
% uniq_alley, uniq_rect is matrix sequence of ideal entries per zone
% alley_entry_mat,rectangle_entry_mat is matrix with sequence of actual
% entries per zone (same length). 
%
% Returns: 
% ideal_no is absolute number of zones for ideal entries. 
% match_abs is absolute number of zones that match sequence of ideal entries per zone. 
% match_perc is %-match of the sequence of actual to ideal entries per zone. 
% Caution: Does not take into account if person moved elsewhere after taking ideal path. 
% final_match additionally checks if final line corresponds to ideal entries 
% per zone. 

ideal_no=size(uniq_alley,1) + size(uniq_rect,1); 
match_abs=0;
k=1; 
while k<=size(uniq_alley,1)
    for i=1:size(alley_entry_mat,1)
        if isequal(alley_entry_mat(i,:),uniq_alley(k,:))
            match_abs=match_abs+1;
            break
        end
    end
    k=k+1;
end

m=1;
while m<=size(uniq_rect,1)
    for i=1:size(rectangle_entry_mat,1)
        if isequal(rectangle_entry_mat(i,:),uniq_rect(m,:))
            match_abs=match_abs+1;
            break
        end
    end
    m=m+1;
end

match_perc=match_abs/ideal_no;

final_match=0; 
if match_perc==1  && isequal(uniq_rect(end,:), rectangle_entry_mat(end,:)) && isequal(uniq_alley(end,:), alley_entry_mat(end,:))
    final_match=1; 
end
    
end