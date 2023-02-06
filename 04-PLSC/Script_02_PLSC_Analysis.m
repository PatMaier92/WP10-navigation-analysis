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

load([path, 'wp10_plsc_allo_2_by_1.mat']);
data_allo_2_by_1 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_allo_2_by_2.mat']);
data_allo_2_by_2 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_allo_1_by_1.mat']);
data_allo_1_by_1 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_ego_2_by_1.mat']);
data_ego_2_by_1 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_ego_2_by_2.mat']);
data_ego_2_by_2 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_ego_1_by_1.mat']);
data_ego_1_by_1 = cellfun(@str2num, m); clear m;

data_cell = {data_allo_2_by_1 data_ego_2_by_1 data_allo_1_by_1 data_ego_1_by_1 data_allo_2_by_1 data_ego_2_by_1};
data_names = {'allo_2_by_1' 'ego_2_by_1' 'allo_1_by_1' 'ego_1_by_1' 'allo_2_by_2' 'ego_2_by_2'}; 
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% CONFIGURATION
%--------------------------------------------------------------------------
cfg.pls = [];
cfg.pls.method   = 3; % regular behavior PLS
cfg.pls.num_perm = 5000; % number of permutations
cfg.pls.num_boot = 5000; % number of bootstrap tests
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
    plsinput.y = data(:,3);
    
    % explanatory behavioral data
    plsinput.X = data(:,4:10);
    
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
    n_subj = size(plsinput.y,1);
    plsres = pls_analysis({ plsinput.X }, n_subj, 1, cfg.pls);
    %--------------------------------------------------------------------------
    %--------------------------------------------------------------------------
    % SAVE OUTPUT
    % -------------------------------------------------------------------------
    % Bootstrap ratios for included variables
    % get significance of extracted latent variable (should be < than 0.05)
    LV_sig = plsres.perm_result.sprob;
    
    % Latent variable weights (should be < -1.96 or > +1.96)
    % combine data and write table
    LV = 1;
    latency = plsres.boot_result.compare_u(1,LV);
    excess_path = plsres.boot_result.compare_u(2,LV);
    excess_distance = plsres.boot_result.compare_u(3,LV);
    rotation_velocity = plsres.boot_result.compare_u(4,LV);
    layout = plsres.boot_result.compare_u(5,LV);
    landmark = plsres.boot_result.compare_u(6,LV);
    position = plsres.boot_result.compare_u(7,LV);
    PLSC_LV = table(LV_sig, latency, excess_path, excess_distance, rotation_velocity, layout, landmark, position);
    
    writetable(PLSC_LV,[path, 'PLSC_LV_', file_name, '.txt'])
    
        % figure; subplot(1,2,1);
        % bar(plsres.boot_result.compare_u(:,sig_LV),'k'); hold on;
        % set(gca,'xticklabels',{'latency','path','distance','init.vel','layout', 'landmark', 'position'}, 'fontsize', 12);
        % box off; grid on;
        % lh = line([0,numel(plsres.boot_result.compare_u)+1],[2,2]);
        % set(lh, 'color','r','linestyle',"--');
        % lh = line([0,numel(plsres.boot_result.compare_u)+1],[-2,-2]);
        % set(lh, 'color','r','linestyle','--');
        % title('LV profile');
        % ylim([-12 6]);
        %
        % subplot(1,2,2);
        % bar(plsres.boot_result.orig_corr(:,1),'k'); hold on;
        % set(gca,'xticklabels',{''});
        % box off; grid on; grid minor; ylim([-1,1]); xlim([0,2]);
        % lh1 = line([1,1],[plsres.boot_result.llcorr_adj(1,1),plsres.boot_result.ulcorr_adj(1,1)]);
        % set(lh1, 'color','r');
        % title('LV correlation with memory score');
    
    % Latent profile scores
    % combine data and write table
    id = data(:,1);
    group = data(:,2);
    memory_score = data(:,3);
    latent_profile_score = plsres.usc;
    PLSC_LP = table(id, group, memory_score, latent_profile_score);
    
    writetable(PLSC_LP, [path, 'PLSC_LP_', file_name, '.txt']); 
    
        % figure;
        % gscatter(plsres.usc, plsinput.y, group);
        % set(gca,'fontsize', 12);
        % xlabel(upper('LV profile score'),'fontweight','bold');
        % ylabel(upper('memory score'),'fontweight','bold');
        % [R,P]=corrcoef(plsres.usc, plsinput.y, 'rows', 'complete');
        % title(strcat('r=',num2str(R(2,1)),', p=',num2str(P(2,1))));
    %--------------------------------------------------------------------------
end

clear all; 