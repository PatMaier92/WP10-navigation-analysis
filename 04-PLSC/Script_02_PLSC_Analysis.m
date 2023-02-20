clear; close all; clc; format long;
addpath(genpath(pwd)) % add subfolder functions to path

%% Partial Least Square Correlation (PLSC) Analysis 
% @ date 2022-09-05 @ author Patrizia Maier & tracked by git 

% This script applies Partial Least Squares Correlation (PLSC; Krishnan et al., 2011) 
% to extract a latent variable (LV) capturing memory-related differences in
% navigation behavior. 

% Note: exclude data with missings on one variable.

% This script requires the plscmd toolbox (AUTHORS).

%%  Run PLSC between memory score and navigation variables  
%--------------------------------------------------------------------------
% LOAD DATA 
%--------------------------------------------------------------------------
path = '../WP10_data/WP10_results/';


load([path, 'wp10_plsc_2_by_1_c.mat']);
data_2_by_1_c = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_2_by_learn_1_c.mat']);
data_2_by_learn_1_c = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_1_2_by_1.mat']);
data_1_2_by_1 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_1_2_by_learn_1.mat']);
data_1_2_by_learn_1 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_ego_1_2_by_1_c.mat']);
data_ego_2_by_1_c = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_allo_1_2_by_1_c.mat']);
data_allo_2_by_1_c = cellfun(@str2num, m); clear m;


% load([path, 'wp10_plsc_1_by_1.mat']);
% data_1_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_2_by_1.mat']);
% data_2_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_2_by_2.mat']);
% data_2_by_2 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ret_by_1.mat']);
% data_ret_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ret_by_learn_1.mat']);
% data_ret_by_learn_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ret_by_learn_1_init.mat']);
% data_ret_by_learn_1_init = cellfun(@str2num, m); clear m;
% 
% 
% load([path, 'wp10_plsc_allo_1_by_1.mat']);
% data_allo_1_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allo_2_by_1.mat']);
% data_allo_2_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allo_2_by_2.mat']);
% data_allo_2_by_2 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allo_ret_by_1.mat']);
% data_allo_ret_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allo_ret_by_learn_1.mat']);
% data_allo_ret_by_learn_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allo_ret_by_learn_1_init.mat']);
% data_allo_ret_by_learn_1_init = cellfun(@str2num, m); clear m;
% 
% 
% load([path, 'wp10_plsc_ego_1_by_1.mat']);
% data_ego_1_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ego_2_by_1.mat']);
% data_ego_2_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ego_2_by_2.mat']);
% data_ego_2_by_2 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ego_ret_by_1.mat']);
% data_ego_ret_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ego_ret_by_learn_1.mat']);
% data_ego_ret_by_learn_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ego_ret_by_learn_1_init.mat']);
% data_ego_ret_by_learn_1_init = cellfun(@str2num, m); clear m;
% 
% 
% load([path, 'wp10_plsc_wl_1_by_1.mat']);
% data_wl_1_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_wl_2_by_1.mat']);
% data_wl_2_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_wl_2_by_2.mat']);
% data_wl_2_by_2 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_wl_ret_by_1.mat']);
% data_wl_ret_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_wl_ret_by_learn_1.mat']);
% data_wl_ret_by_learn_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_wl_ret_by_learn_1_init.mat']);
% data_wl_ret_by_learn_1_init = cellfun(@str2num, m); clear m;
% 
% 
% load([path, 'wp10_plsc_allo_wl_1_by_1.mat']);
% data_allo_wl_1_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allo_wl_2_by_1.mat']);
% data_allo_wl_2_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allo_wl_2_by_2.mat']);
% data_allo_wl_2_by_2 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allo_wl_ret_by_1.mat']);
% data_allo_wl_ret_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allo_wl_ret_by_learn_1.mat']);
% data_allo_wl_ret_by_learn_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allo_wl_ret_by_learn_1_init.mat']);
% data_allo_wl_ret_by_learn_1_init = cellfun(@str2num, m); clear m;
% 
% 
% load([path, 'wp10_plsc_ego_wl_1_by_1.mat']);
% data_ego_wl_1_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ego_wl_2_by_1.mat']);
% data_ego_wl_2_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ego_wl_2_by_2.mat']);
% data_ego_wl_2_by_2 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ego_wl_ret_by_1.mat']);
% data_ego_wl_ret_by_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ego_wl_ret_by_learn_1.mat']);
% data_ego_wl_ret_by_learn_1 = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_ego_wl_ret_by_learn_1_init.mat']);
% data_ego_wl_ret_by_learn_1_init = cellfun(@str2num, m); clear m;

% data_cell = {data_allo_2_by_1 data_ego_2_by_1 data_allo_1_by_1 data_ego_1_by_1 data_allo_2_by_1 data_ego_2_by_1};
% data_names = {'allo_2_by_1' 'ego_2_by_1' 'allo_1_by_1' 'ego_1_by_1' 'allo_2_by_2' 'ego_2_by_2'}; 


%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% CONFIGURATION
%--------------------------------------------------------------------------
cfg.pls = [];
cfg.pls.method   = 3; % regular behavior PLS
% cfg.pls.method   = 5; % non-rotated behavior PLS (with contrasts) 
cfg.pls.num_perm = 500; % 5000; % number of permutations
cfg.pls.num_boot = 500; % 5000; % number of bootstrap tests
cfg.pls.clim     = 95; % confidence interval level
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% PROCESS DATA
%--------------------------------------------------------------------------
for i=1:numel(data_cell)
    % set data 
    data = data_cell{i};
    file_name = data_names{i};
    %--------------------------------------------------------------------------
    % CREATE THE BASIC MATRCIES / VECTORS
    %--------------------------------------------------------------------------
    % NOTE: all data needs to be sorted subjects X data
    % behavioral output
%     plsinput.y = data(:,3);
    plsinput.y = data(:,4);
     
    % explanatory behavioral data
%     plsinput.X = data(:,4:size(data,2));
    plsinput.X = data(:,5:size(data,2));
    
    % z-standardization
    plsinput.y = zscore(plsinput.y,0,1);
    plsinput.X = zscore(plsinput.X,0,1);
    %--------------------------------------------------------------------------
    %--------------------------------------------------------------------------
    % RUN PLS
    %--------------------------------------------------------------------------
    % add the 'behavioral data'
    cfg.pls.stacked_behavdata = plsinput.y;
    
    % input arguments: data, number of subjects, number of conditions, specific settings
    %     n_subj = size(plsinput.y,1); % none 
    %     n_subj = histc(data(:,2),unique(data(:,2))); % group
    %     n_subj = size(plsinput.y,1)/2; % condition
    n_subj = histc(data(:,2),unique(data(:,2))) / 2; % condition, group
    
    %     datamat1_allgroups = plsinput.X;
    datamat1_group1 = plsinput.X(1:n_subj(1),:);
    datamat1_group2 = plsinput.X(n_subj(1)+1:n_subj(1)+n_subj(2),:);
    datamat1_group3 = plsinput.X(n_subj(1)+n_subj(2)+1:end,:);
    
    %     n_con = 1; % none
    n_con = 2; % condition
    %     cfg.pls.stacked_designdata=[1; -1];
    %     cfg.pls.stacked_designdata=[1; -1; 1; -1; 1; -1]; 
    %     cfg.pls.stacked_designdata=[ 1 0 0 1; -1 0 0 -1; 1 1 0 0; -1 -1 0 0; 1 0 1 0; -1 0 -1 0];
    
    % run plsc 
    %     plsres = pls_analysis({ datamat1_allgroups }, n_subj, n_con, cfg.pls);
    plsres = pls_analysis({ datamat1_group1,datamat1_group2,datamat1_group3 }, n_subj, n_con, cfg.pls);
    
    %--------------------------------------------------------------------------
    %--------------------------------------------------------------------------
    % SAVE OUTPUT
    % -------------------------------------------------------------------------
    % Latent variable significance (should be < than 0.05)
    % and correlation with behavioral data 
    LV = [plsres.perm_result.sprob; plsres.lvcorrs];
    
    % Latent variable weights / Bootstrap ratios (BSR) (should be < -1.96 or > +1.96)
    % and correlations with behavioral data 
    LV_n = 1;
    cor = plsres.datamatcorrs_lst{1,1};
    
%     latency = [plsres.boot_result.compare_u(1,LV_n); cor(1)];
%     excess_path = [plsres.boot_result.compare_u(2,LV_n); cor(2)];
%     excess_distance = [plsres.boot_result.compare_u(3,LV_n); cor(3)];
%     rotation_velocity = [plsres.boot_result.compare_u(4,LV_n); cor(4)];
%     layout = [plsres.boot_result.compare_u(5,LV_n); cor(5)];
%     landmark = [plsres.boot_result.compare_u(6,LV_n); cor(6)];
%     position = [plsres.boot_result.compare_u(7,LV_n); cor(7)];
%     PLSC_LV = table(LV, latency, excess_path, excess_distance, rotation_velocity, layout, landmark, position);
% 
%     writetable(PLSC_LV,[path, 'PLSC_LV_', file_name, '.txt'])
    
%         figure; subplot(1,2,1);
%         bar(plsres.boot_result.compare_u(:,LV_n),'k'); hold on;
%         set(gca,'xticklabels',{'latency','path','distance','init.vel','layout', 'landmark', 'position'}, 'fontsize', 12);
%         box off; grid on;
%         lh = line([0,size(plsres.boot_result.compare_u,1)+1],[2,2]);
%         set(lh, 'color','r','linestyle','--');
%         lh = line([0,size(plsres.boot_result.compare_u,1)+1],[-2,-2]);
%         set(lh, 'color','r','linestyle','--');
%         ylim([-12 12]);
%         title('LV profile');
%         hold off;
%         
%         subplot(1,2,2);
%         n_dim = size(plsres.boot_result.orig_corr,1); 
%         bar(1:n_dim, plsres.boot_result.orig_corr(:,LV_n),'k'); hold on; 
%         if n_dim==1
%             set(gca,'xticklabels','');  
%         elseif n_dim==3
%             set(gca,'xticklabels',{'6-8yo','9-11yo','adults'}, 'fontsize', 12);
%         end
%         box off; grid on; grid minor;
%         for p = 1:n_dim
%             lh1 = line([p,p],[plsres.boot_result.llcorr_adj(p,LV_n),plsres.boot_result.ulcorr_adj(p,LV_n)]); 
%             set(lh1, 'color','r');
%         end
%         xlim([0,n_dim+1]); ylim([-1,1]);
%         title('LV correlation with memory score');
%         hold off; 
     
    % Latent profile scores
    % combine data and write table
    id = data(:,1);
    group = data(:,2);
    memory_score = data(:,3);
    latent_profile_score = plsres.usc(:,LV_n);
    PLSC_LP = table(id, group, memory_score, latent_profile_score);
    
    writetable(PLSC_LP, [path, 'PLSC_LP_', file_name, '.txt']); 
    
%         figure;
%         gscatter(plsres.usc(:,LV_n), plsinput.y, group);
%         set(gca,'fontsize', 12);
%         xlabel(upper('LV profile score'),'fontweight','bold');
%         ylabel(upper('memory score'),'fontweight','bold');
%         [R,P]=corrcoef(plsres.usc(:,LV_n), plsinput.y, 'rows', 'complete');
%         title(strcat('r=',num2str(R(2,1)),', p=',num2str(P(2,1))));   
       
    save([path, 'PLSC_full_results_', file_name, '.mat'],'plsres');
    %--------------------------------------------------------------------------
end

clear all; 