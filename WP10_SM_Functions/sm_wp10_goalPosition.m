function [goal_x,goal_y,goal_x2,goal_y2, goal_x3,goal_y3]=sm_wp10_goalPosition(wp,xmin,xmax,ymin,ymax)

if wp==3 || wp==2 || wp==6 || wp==31
        goal_x= 375.1;    goal_y=314; % real target
%         goal_x_width=377.25; goal_y_length=313;
%         goal_x_nf=377.25;    goal_y_nf=311;
elseif wp==10
        goal_x= 351.2;    goal_y=368.4; % real target
%         goal_x_width=349.0; goal_y_length=367.4;
%         goal_x_nf=goal_x;    goal_y_nf=3;
        goal_x2= 321.7;    goal_y2=358.1; % real target
%         goal_x3_width=319.7; goal_y3_length=357.65;
%         goal_x3_nf=goal_x3;    goal_y3_nf=goal_y3;
        
        goal_x3= 371.9;    goal_y3=356.9; % real target
%         goal_x2_width=369.9; goal_y2_length=356.35;
%         goal_x2_nf=goal_x2;    goal_y2_nf=goal_y2;
        

        
else
    askForStartpositions=input('Would you like to change goal-positions? 1=yes, otherwise = no');
    if askForStartpositions==1
        try
        goal_x=input('Enter goalposition_x: ');    
        goal_y=input('Enter goalposition_y: '); % real target
        goal_x_width=input('Enter width of goal: '); 
        goal_y_length=input('Enter length of goal: '); 
        catch
            disp('Invalid input. Please reenter and use positive doubles.');
        end
    else
        %goal_x= 377.050;    
        %goal_y=312.800; % real target
        goal_x= 374;    
        goal_y=315.75; % real target
        goal_x_width=377.25; 
        goal_y_length=313;
        disp('Your goalposition was set to default.');
    end
end
        % Data-normalization goal position
        goal_x=datanorm(goal_x,xmin,xmax);goal_y= datanorm(goal_y,ymin,ymax);
        goal_x2=datanorm(goal_x2,xmin,xmax);goal_y2= datanorm(goal_y2,ymin,ymax);
        goal_x3=datanorm(goal_x3,xmin,xmax);goal_y3= datanorm(goal_y3,ymin,ymax);
%         goal_x_width=datanorm(goal_x_width,xmin,xmax);goal_y_length=datanorm(goal_y_length,ymin,ymax);
%         goal_x_nf=datanorm(goal_x_nf,xmin,xmax);goal_y_nf= datanorm(goal_y_nf,ymin,ymax);
end