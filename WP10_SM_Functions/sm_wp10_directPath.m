function direct_path= sm_wp10_directPath(start,goal,success,alley_entry,rectangle_entry,triangle_entry)
%          [row,alleyNo]=size(alley_entry);
direct_path=0;

if success ==1
        if start==1  
            if goal==1 && alley_entry(:,start)==1 && triangle_entry(:,start)==0 
                direct_path=1;
            elseif goal==2 && alley_entry(:,start)==1 && rectangle_entry(:,start)==1 && alley_entry(:,goal)==1 && rectangle_entry(:,5)==0 && rectangle_entry(goal+1)==0 
                direct_path=1;
            elseif goal==3 && alley_entry(:,start)==1 && rectangle_entry(:,5)==1 && alley_entry(:,5)==1 && rectangle_entry(:,4)==0 && rectangle_entry(:,start)==0
                direct_path=1;
            end
        elseif start==2  
            if goal==1 && rectangle_entry(:,1)==1 &&  alley_entry(:,goal)==1 && triangle_entry(:,start)==0 && rectangle_entry(:,5)==0
                direct_path=1;
            elseif goal==2 && rectangle_entry(:,1)==1 && alley_entry(:,goal)==1 && triangle_entry(:,1)==0 && rectangle_entry(:,2)==0
                direct_path=1;
            elseif goal==3 && rectangle_entry(:,1)==1 && rectangle_entry(5)==1 && alley_entry(5)==1 && rectangle_entry(:,4)==0 && triangle_entry(:,2)==0 && alley_entry(:,1)==0 
                direct_path=1;
            end
        elseif start==3 
            if goal==1 && alley_entry(:,2)==1 && rectangle_entry(:,1)==1 &&  alley_entry(:,1)==1 && triangle_entry(:,2)==0 && rectangle_entry(:,5)==0
                direct_path=1;
            elseif goal==3 && alley_entry(:,2)==1 && rectangle_entry(:,1)==1 && rectangle_entry(:,5)==1 && alley_entry(:,5)==1 && rectangle_entry(:,4)==0 && rectangle_entry(:,2)==0 && alley_entry(:,1)==0 
                direct_path=1;
            elseif goal==2 && alley_entry(:,2)==1 && triangle_entry(:,2)==0
                direct_path=1;
            end
        elseif start==4 
            if goal==1 && rectangle_entry(:,2)==1 && rectangle_entry(:,1)==1 &&  alley_entry(:,1)==1 && alley_entry(:,1)==0 && rectangle_entry(:,5)==0 && alley_entry(:,2)==0
                direct_path=1;
            elseif goal==2 && rectangle_entry(:,2)==1 && alley_entry(:,2)==1 && rectangle_entry(:,1)==0 && triangle_entry(:,3)==0 
                direct_path=1;
            elseif goal==3 && rectangle_entry(:,2)==1 && rectangle_entry(:,1)==1 && rectangle_entry(:,5)==1 && alley_entry(:,5)==1 && rectangle_entry(:,4)==0 && triangle_entry(:,3)==0 && alley_entry(:,1)==0 && alley_entry(:,2)==0 
                direct_path=1;
            elseif goal==3 && rectangle_entry(:,2)==1 && rectangle_entry(:,3)==1 && rectangle_entry(:,4)==1 && alley_entry(:,5)==1 && rectangle_entry(:,5)==0 && triangle_entry(:,2)==0 && alley_entry(:,3)==0 && alley_entry(:,4)==0
                direct_path=1;
            end
        elseif start==5   
            if goal==1 && alley_entry(:,3)==1 && rectangle_entry(:,2)==1 &&  rectangle_entry(:,1)==1 &&  alley_entry(:,1)==1 && rectangle_entry(:,5)==0 && rectangle_entry(:,3)==0
                direct_path=1;
            elseif goal==2 && alley_entry(:,3)==1 && rectangle_entry(:,2)==1 && alley_entry(:,2)==1 && rectangle_entry(:,1)==0 && rectangle_entry(:,3)==0 
                direct_path=1;
            elseif goal==3 && alley_entry(:,3)==1 && rectangle_entry(:,3)==1 && rectangle_entry(:,4)==1 && alley_entry(:,5)==1 && rectangle_entry(:,5)==0 && rectangle_entry(:,2)==0
                direct_path=1;
            end
        elseif start==6 
            if goal==1 && rectangle_entry(:,3)==1 && rectangle_entry(:,4)==1 && rectangle_entry(:,5)==1 && alley_entry(:,1)==1 && triangle_entry(:,3)==0 && rectangle_entry(:,1)==0 && alley_entry(:,4)==0 && alley_entry(:,5)==0
                direct_path=1;
            elseif goal==1 && rectangle_entry(:,3)==1 && rectangle_entry(:,2)==1 && rectangle_entry(:,1)==1 && alley_entry(:,1)==1 && triangle_entry(:,4)==0 && rectangle_entry(:,5)==0 && alley_entry(:,3)==0 && alley_entry(:,2)==0 
                direct_path=1;
            elseif goal==3 && rectangle_entry(:,3)==1 && rectangle_entry(:,4)==1 && alley_entry(:,5)==1 && rectangle_entry(:,5)==0 && triangle_entry(:,3)==0 && alley_entry(:,4)==0 
                direct_path=1;
            elseif goal==2 && rectangle_entry(:,3)==1 && rectangle_entry(:,2)==1 && alley_entry(:,2)==1 && rectangle_entry(:,1)==0 && triangle_entry(:,4)==0 && alley_entry(:,3)==0 
                direct_path=1;
            end
        elseif start==7 
            if goal==1  && alley_entry(:,4)==1 && rectangle_entry(:,4)==1 &&  rectangle_entry(:,5)==1 &&  alley_entry(:,1)==1 && rectangle_entry(:,1)==0 && rectangle_entry(:,3)==0
                direct_path=1;
            elseif goal==3 && alley_entry(:,4)==1 && rectangle_entry(:,4)==1 && alley_entry(:,5)==1 && rectangle_entry(:,5)==0 && rectangle_entry(:,3)==0 
                direct_path=1;
            elseif goal==2  && alley_entry(:,4)==1 && rectangle_entry(:,3)==1 && rectangle_entry(:,2)==1 && alley_entry(:,2)==1 && rectangle_entry(:,1)==0 && rectangle_entry(:,4)==0 && alley_entry(:,3)==0
                direct_path=1;
            end
        elseif start==8 
            if goal==1 && rectangle_entry(:,4)==1 && alley_entry(:,goal)==1 && rectangle_entry(:,goal)==0 && alley_entry(:,5)==0 && triangle_entry(start/2)==0
                direct_path=1;
            elseif goal==3 && rectangle_entry(:,4)==1 && alley_entry(:,5)==1 && triangle_entry(:,4)==0 && rectangle_entry(:,5)==0 
                direct_path=1;
            elseif goal==2 && rectangle_entry(:,4)==1 && rectangle_entry(:,5)==1 && rectangle_entry(:,1)==1  && alley_entry(:,2)==1 && triangle_entry(:,4)==0 && rectangle_entry(:,2)==0 && alley_entry(:,5)==0 && alley_entry(:,1)==0 
                direct_path=1;
            elseif goal==2 && rectangle_entry(:,4)==1 && rectangle_entry(:,3)==1 && rectangle_entry(:,2)==1  && alley_entry(:,2)==1 && triangle_entry(:,5)==0 && rectangle_entry(:,1)==0 && alley_entry(:,3)==0 && alley_entry(:,4)==0 
                direct_path=1;
            end
        elseif start==9 
            if goal==1 && alley_entry(:,5)==1 && rectangle_entry(:,5)==1 && alley_entry(:,goal)==1 && rectangle_entry(:,goal)==0 && rectangle_entry(:,4)==0
                direct_path=1;
            elseif goal==2 && alley_entry(:,5)==1 && rectangle_entry(:,5)==1 && rectangle_entry(:,1)==1 && alley_entry(:,goal)==1 && rectangle_entry(:,goal)==0 && rectangle_entry(:,4)==0 && alley_entry(:,1)==0 
                direct_path=1;
            elseif goal==3 && alley_entry(:,5)==1 && triangle_entry(:,5)==0
                direct_path=1;
            end
        elseif start==10
            if goal==1 && rectangle_entry(start/2)==1  && alley_entry(:,goal)==1 && rectangle_entry(:,goal)==0 && triangle_entry(:,5)==0
                direct_path=1;
            elseif goal==2 && rectangle_entry(start/2)==1  && alley_entry(:,goal)==1 && rectangle_entry(:,goal)==0 && triangle_entry(start/2)==0 && alley_entry(:,1)==0
                direct_path=1;
            elseif goal==3 && rectangle_entry(start/2)==1 && alley_entry(start/2)==1 && triangle_entry(:,1)==0 && rectangle_entry(:,4)==0
                direct_path=1;
            end
        end
end
end