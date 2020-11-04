% UNUSED %

function [n_x,n_y,se_x,se_y,sw_x,sw_y,ne_x,ne_y,nw_x,nw_y,...
    i_12_x,i_12_y,i_23_x,i_23_y,i_34_x,i_34_y,i_45_x,i_45_y,i_51_x,i_51_y]=sm_wp10_startPositions(wp, xmin,xmax,ymin,ymax)

disp('The function sm_wp10_startPositions is deactivated'); 

% if wp==10
%     n_x= 350.000;    n_y=380.000;  % Training start --> mid (North-mid)
%     se_x= 367.63;    se_y=325.73;        % start --> lower-right (se=Sooutheast)
%     sw_x=332.37; sw_y=325.73;          % start --> lower-left (sw=Southwest)
%     ne_x= 378.53;     ne_y=359.27;     % start --> upper right (ne=Northeast)
%     nw_x=321.47; nw_y= 359.27;         % start --> upper left (nw=Northwest)
%     i_12_x=356.07; i_12_y= 358.39;     % start --> intersection 12 upper right
%     i_23_x=359.86; i_23_y= 346.82;
%     i_34_x=350.02; i_34_y= 339.64;
%     i_45_x=340.16; i_45_y= 346.78;     % start --> intersection 45 lower left
%     i_51_x=343.89; i_51_y= 358.37;     % start --> intersection 51 upper left
% else
%     try
%         askForStartpositions=input('Would you like to change start-positions? 1=yes, otherwise = no');
%     catch
%         disp('Invalid input. Please reenter and use positive doubles.');
%     end
%     if askForStartpositions==1
%         try
%             start_x=input('Enter start_x: ');    start_y=input('Enter start_y: ');  % Training start --> mid (North-mid)
%             se_x=input('Enter start_x for southeastern alley: ');
%             se_y=input('Enter start_y for southeastern alley: '); % start --> lower-right (se=Sooutheast)
%             sw_x=input('Enter start_x for southwestern alley: ');
%             sw_x=input('Enter start_y for southwestern alley: '); % start --> lower-left (sw=Southwest)
%             ne_x=input('Enter start_x for northeastern alley: ');
%             ne_y=input('Enter start_y for northeastern alley: '); % start --> upper right (ne=Northeast)
%             nw_x=input('Enter start_x for northwestern alley: ');
%             nw_y=input('Enter start_y for northwestern alley: ');% start --> upper left (nw=Northwest)
%             i_12_x=input('Enter start_x for intersection upper-right: ');
%             i_12_y=input('Enter start_y for intersection upper-right: ');% start --> intersection 12 upper right
%             i_45_x=input('Enter start_x for intersection lower-left: ');
%             i_45_y=input('Enter start_y for intersection lower-left: ');  % start --> intersection 45 lower left
%             i_51_x=input('Enter start_x for intersection upper-left: ');
%             i_51_y=input('Enter start_y for intersection upper-left: '); % start --> intersection 51 upper left
%         catch
%             disp('Invalid input. Please reenter and use positive doubles.');
%         end
%     else
%         start_x= 350.000;    start_y=396.000;  % Training start --> mid (North-mid)
%         se_x= 375;    se_y=314;        % start --> lower-right (se=Sooutheast)
%         sw_x=323.0207; sw_y=312.8661;          % start --> lower-left (sw=Southwest)
%         ne_x= 393.7486;     ne_y=364.2148;     % start --> upper right (ne=Northeast)
%         nw_x=306.2514; nw_y= 364.2148;         % start --> upper left (nw=Northwest)
%         i_12_x=358.8168; i_12_y= 362.1353;     % start --> intersection 12 upper right
%         i_45_x=335.7342; i_45_y= 345.3647;     % start --> intersection 45 lower left
%         i_51_x=341.1832; i_51_y= 362.1353;     % start --> intersection 51 upper left
%         disp('Yor startpositions were set to default.');
%     end
% end
% % Datanormalization
% % data-normalization startpositions
% n_x=datanorm(n_x,xmin,xmax); n_y=datanorm(n_y,ymin,ymax); % Alley 1
% ne_x=datanorm(ne_x,xmin,xmax); ne_y=datanorm(ne_y,ymin,ymax); %Alley 2
% nw_x=datanorm(nw_x,xmin,xmax); nw_y=datanorm(nw_y,ymin,ymax); % Alley 3
% se_x=datanorm(se_x,xmin,xmax); se_y=datanorm(se_y,ymin,ymax); %Alley 4
% sw_x=datanorm(sw_x,xmin,xmax); sw_y=datanorm(sw_y,ymin,ymax); %Alley 5
% i_12_x=datanorm(i_12_x,xmin,xmax); i_12_y=datanorm(i_12_y,ymin,ymax); %intersection 12
% i_23_x=datanorm(i_23_x,xmin,xmax); i_23_y=datanorm(i_23_y,ymin,ymax); % intersection 45
% i_34_x=datanorm(i_34_x,xmin,xmax); i_34_y=datanorm(i_34_y,ymin,ymax); % intersection 45
% i_45_x=datanorm(i_45_x,xmin,xmax); i_45_y=datanorm(i_45_y,ymin,ymax); % intersection 45
% i_51_x=datanorm(i_51_x,xmin,xmax); i_51_y=datanorm(i_51_y,ymin,ymax); % intersection 51

end