% %%% EXPLORATORY
% dtw([xi_al, yi_al]',xy_unique');
% dtw([xi_al, yi_al]',[x, y]');
% 
% [sdtw,~,~] = dtw([xi_al, yi_al]', xy_unique'); 
% ncdtw = exp(-(sdtw/length(xi_al)*10)); %*0.1 * 100
% ndtw = exp(-(sdtw/length(xi_al))); 
% 
% %%%
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
% %%% EXPLORATORY