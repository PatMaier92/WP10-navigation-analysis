% %%% EXPLORATORY
%
% % DTW 
% dtw([xi_al, yi_al]',xy_unique');
% dtw([xi_al, yi_al]',[x, y]');
% 
% [sdtw,~,~] = dtw([xi_al, yi_al]', xy_unique'); 
% ncdtw = exp(-(sdtw/length(xi_al)*10)); %*0.1 * 100
% ndtw = exp(-(sdtw/length(xi_al))); 
%
% [dtw_trajectory,i1,i2] = dtw([xi_al, yi_al]',xy_unique');
% ndtw = exp(-(dtw_trajectory/length(xy_unique)*0.1)); 
% X = [xi_al(i1),xi_al(i1)]; Y = [x_unique(i2),y_unique(i2)];
% [dissimimlarityRatio,Z,~] = procrustes(X,Y,'Scaling',false,'reflection',false);
% figure; 
% plot(X(:,1),X(:,2),'r.',Y(:,1),Y(:,2),'y.',...
%     Z(:,1),Z(:,2),'b.'); 
% xlim([0 1]);
% ylim([0 1]);
% 
% % test plot
% figure;
% plot(sm.coord.full_poly);
% hold on
% plot(xi_al, yi_al, 'k-', x, y, 'r-',...
%     x_unique, y_unique, 'g--');
% xlim([0 1]);
% ylim([0 1]);
% hold off
%
%
% % VERLÄNGERUNG x_line/y_line um MITTELPUNKT  
% temp_x=zeros(1,length(x_line)-1); temp_y=zeros(1,length(y_line)-1);
% for i=1:length(x_line)-1
%     temp_x(i) = (x_line(i)+x_line(i+1))/2;
%     temp_y(i) = (y_line(i)+y_line(i+1))/2;
% end
% 
% x_ext = x_line(1);
% y_ext = y_line(1);
% for j=1:length(x_line)-1
%     x_ext = [x_ext temp_x(j) x_line(j+1)];
%     y_ext = [y_ext temp_y(j) y_line(j+1)];
% end
% 
% % test plot
% figure;
% plot(sm.coord.full_poly);
% hold on
% plot(x_line, y_line, 'k+', x_ext, y_ext, 'r-x');
% xlim([0 1]);
% ylim([0 1]);
% hold off
% 
% 
% %%% SAMPLING RATE CORRECTION in case sampling rate differs %%% 
% %%% needs to be checked if appropriate (alternative function 'decimate', 'resample' or activate only for higher sampling rates) %%% 
% % temporal normalization (downsampling) 
% data = downsample(data,round(sm.participant(p).session(s).trial(k).sampling_rate*1000));
% 
% % new sampling rate %%% needs to be checked %%%
% sampling_rate_new=zeros(length(data.time)-1,1);
% for i=1:length(data.time)-1
%     sampling_rate_new(i)=data.time(i+1)-data.time(i);
% end
% sm.participant(p).session(s).trial(k).new_sampling_rate=sum(sampling_rate_new)/length(sampling_rate_new);