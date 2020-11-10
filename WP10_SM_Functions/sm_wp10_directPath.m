function direct_path=sm_wp10_directPath(start,goal,correct_alley,alley_entry,rectangle_entry,triangle_entry)
% SM_WP10_DIRECTPATH Determines whether participant took direct path to
% goal location in Starmaze WP10. 
% 
% Input:
% start is start alley (integer)
% goal is goal location (integer)
% correct_alley indicates chosen goal is in correct alley (boolean)
% % alternatively use: success indicates chosen goal is close to actual goal (boolean)
% alley_entry,rectangle_entry,triangle_entry is number of entries in each zone
%
% Returns: direct_path (boolean). 

direct_path=0; % default no
if correct_alley==1 % if trial successful
    if start==1
        if goal==1 && alley_entry(1)==1 && triangle_entry(1)==0 % no option 
            direct_path=1;
        elseif goal==2 && alley_entry(1)==1 && rectangle_entry(1)==1 ...
                && alley_entry(2)==1 && rectangle_entry(5)==0 ...
                && rectangle_entry(2)==0
            direct_path=1;
        elseif goal==3 && alley_entry(1)==1 && rectangle_entry(5)==1 ...
                && alley_entry(5)==1 && rectangle_entry(4)==0 ...
                && rectangle_entry(1)==0
            direct_path=1;
        end
    elseif start==2
        if goal==1 && rectangle_entry(1)==1 &&  alley_entry(1)==1 ...
                && triangle_entry(2)==0 && rectangle_entry(5)==0
            direct_path=1;
        elseif goal==2 && rectangle_entry(1)==1 && alley_entry(2)==1 ...
                && triangle_entry(1)==0 && rectangle_entry(2)==0
            direct_path=1;
        elseif goal==3 && rectangle_entry(1)==1 && rectangle_entry(5)==1 ...
                && alley_entry(5)==1 && rectangle_entry(4)==0 ...
                && triangle_entry(2)==0 && alley_entry(1)==0
            direct_path=1;
        end
    elseif start==3
        if goal==1 && alley_entry(2)==1 && rectangle_entry(1)==1 ...
                && alley_entry(1)==1 && rectangle_entry(2)==0 ...
                && rectangle_entry(5)==0
            direct_path=1;
        elseif goal==2 && alley_entry(2)==1 && triangle_entry(2)==0 % no option
            direct_path=1;
        elseif goal==3 && alley_entry(2)==1 && rectangle_entry(1)==1 ...
                && rectangle_entry(5)==1 && alley_entry(5)==1 ...
                && rectangle_entry(4)==0 && rectangle_entry(2)==0 ...
                && alley_entry(1)==0
            direct_path=1;
        end
    elseif start==4
        if goal==1 && rectangle_entry(2)==1 && rectangle_entry(1)==1 ...
                &&  alley_entry(1)==1 && triangle_entry(3) ...
                && rectangle_entry(5)==0 && alley_entry(2)==0
            direct_path=1;
        elseif goal==2 && rectangle_entry(2)==1 && alley_entry(2)==1 ...
                && rectangle_entry(1)==0 && triangle_entry(3)==0
            direct_path=1;
        elseif goal==3 && rectangle_entry(2)==1 && rectangle_entry(1)==1 ...
                && rectangle_entry(5)==1 && alley_entry(5)==1 ...
                && rectangle_entry(4)==0 && triangle_entry(3)==0 ...
                && alley_entry(1)==0 && alley_entry(2)==0
            direct_path=1;
        elseif goal==3 && rectangle_entry(2)==1 && rectangle_entry(3)==1 ...
                && rectangle_entry(4)==1 && alley_entry(5)==1 ...
                && rectangle_entry(5)==0 && triangle_entry(2)==0 ...
                && alley_entry(3)==0 && alley_entry(4)==0
            direct_path=1;
        end
    elseif start==5
        if goal==1 && alley_entry(3)==1 && rectangle_entry(2)==1 ...
                &&  rectangle_entry(1)==1 &&  alley_entry(1)==1 ...
                && rectangle_entry(5)==0 && rectangle_entry(3)==0 ...
                && alley_entry(2)==0 
            direct_path=1;
        elseif goal==2 && alley_entry(3)==1 && rectangle_entry(2)==1 ...
                && alley_entry(2)==1 && rectangle_entry(1)==0 ...
                && rectangle_entry(3)==0
            direct_path=1;
        elseif goal==3 && alley_entry(3)==1 && rectangle_entry(3)==1 ...
                && rectangle_entry(4)==1 && alley_entry(5)==1 ...
                && rectangle_entry(5)==0 && rectangle_entry(2)==0 ...
                && alley_entry(4)==0 
            direct_path=1;
        end
    elseif start==6
        if goal==1 && rectangle_entry(3)==1 && rectangle_entry(4)==1 ...
                && rectangle_entry(5)==1 && alley_entry(1)==1 ...
                && triangle_entry(3)==0 && rectangle_entry(1)==0 ...
                && alley_entry(4)==0 && alley_entry(5)==0
            direct_path=1;
        elseif goal==1 && rectangle_entry(3)==1 && rectangle_entry(2)==1 ...
                && rectangle_entry(1)==1 && alley_entry(1)==1 ...
                && triangle_entry(4)==0 && rectangle_entry(5)==0 ...
                && alley_entry(3)==0 && alley_entry(2)==0
            direct_path=1;
        elseif goal==2 && rectangle_entry(3)==1 && rectangle_entry(2)==1 ...
                && alley_entry(2)==1 && rectangle_entry(1)==0 ...
                && triangle_entry(4)==0 && alley_entry(3)==0
            direct_path=1;
        elseif goal==3 && rectangle_entry(3)==1 && rectangle_entry(4)==1 ...
                && alley_entry(5)==1 && rectangle_entry(5)==0 ...
                && triangle_entry(3)==0 && alley_entry(4)==0
            direct_path=1;
        end
    elseif start==7
        if goal==1 && alley_entry(4)==1 && rectangle_entry(4)==1 ...
                && rectangle_entry(5)==1 &&  alley_entry(1)==1 ...
                && rectangle_entry(1)==0 && rectangle_entry(3)==0 ...
                && alley_entry(5)==0
            direct_path=1;
        elseif goal==2 && alley_entry(4)==1 && rectangle_entry(3)==1 ...
                && rectangle_entry(2)==1 && alley_entry(2)==1 ...
                && rectangle_entry(1)==0 && rectangle_entry(4)==0 ...
                && alley_entry(3)==0
            direct_path=1;
        elseif goal==3 && alley_entry(4)==1 && rectangle_entry(4)==1 ...
                && alley_entry(5)==1 && rectangle_entry(5)==0 ...
                && rectangle_entry(3)==0
            direct_path=1;
        end
    elseif start==8
        if goal==1 && rectangle_entry(4)==1 && rectangle_entry(5)==1 ...
                && alley_entry(1)==1 && rectangle_entry(1)==0 ...
                && triangle_entry(4)==0 && alley_entry(5)==0
            direct_path=1;
        elseif goal==2 && rectangle_entry(4)==1 && rectangle_entry(5)==1 ...
                && rectangle_entry(1)==1  && alley_entry(2)==1 ....
                && triangle_entry(4)==0 && rectangle_entry(2)==0 ...
                && alley_entry(5)==0 && alley_entry(1)==0
            direct_path=1;
        elseif goal==2 && rectangle_entry(4)==1 && rectangle_entry(3)==1 ...
                && rectangle_entry(2)==1  && alley_entry(2)==1 ...
                && triangle_entry(5)==0 && rectangle_entry(1)==0 ...
                && alley_entry(3)==0 && alley_entry(4)==0
            direct_path=1;
        elseif goal==3 && rectangle_entry(4)==1 && alley_entry(5)==1 ...
                && triangle_entry(4)==0 && rectangle_entry(5)==0
            direct_path=1;
        end
    elseif start==9
        if goal==1 && alley_entry(5)==1 && rectangle_entry(5)==1 ...
                && alley_entry(1)==1 && rectangle_entry(1)==0 ...
                && rectangle_entry(4)==0
            direct_path=1;
        elseif goal==2 && alley_entry(5)==1 && rectangle_entry(5)==1 ...
                && rectangle_entry(1)==1 && alley_entry(2)==1 ...
                && rectangle_entry(2)==0 && rectangle_entry(4)==0 ...
                && alley_entry(1)==0
            direct_path=1;
        elseif goal==3 && alley_entry(5)==1 && triangle_entry(5)==0 % no option
            direct_path=1;
        end
    elseif start==10
        if goal==1 && rectangle_entry(5)==1  && alley_entry(1)==1 ...
                && rectangle_entry(1)==0 && triangle_entry(5)==0
            direct_path=1;
        elseif goal==2 && rectangle_entry(5)==1  && rectangle_entry(1)==1 ...
                && alley_entry(2)==1 && rectangle_entry(2)==0 ...
                && triangle_entry(5)==0 && alley_entry(1)==0
            direct_path=1;
        elseif goal==3 && rectangle_entry(5)==1 && alley_entry(5)==1 ...
                && triangle_entry(1)==0 && rectangle_entry(4)==0
            direct_path=1;
        end
    end
end

end