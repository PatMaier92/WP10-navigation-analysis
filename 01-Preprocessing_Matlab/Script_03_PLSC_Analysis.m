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

load([path, 'wp10_plsc_total.mat']);
data = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_retention_by_1.mat']);
data = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_total_2_by_1.mat']);
data = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_allo.mat']);
data_allo = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_allo_s1.mat']);
data_allo_1 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_allo_s2.mat']);
data_allo_2 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_allo_2_by_1.mat']);
data_allo_2_by_1 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_allo_retention_by_1.mat']);
data_allo_r_by_1 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_ego.mat']);
data_ego = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_ego_s1.mat']);
data_ego_1 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_ego_s2.mat']);
data_ego_2 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_ego_2_by_1.mat']);
data_ego_2_by_1 = cellfun(@str2num, m); clear m;

load([path, 'wp10_plsc_ego_retention_by_1.mat']);
data_ego_r_by_1 = cellfun(@str2num, m); clear m;
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% CONFIG
%--------------------------------------------------------------------------
cfg.pls = [];
cfg.pls.method   = 3; % regular behavior PLS
cfg.pls.num_perm = 5000; % number of permutations
cfg.pls.num_boot = 5000; % number of bootstrap tests
cfg.pls.clim     = 95; % confidence interval level
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% CREATE THE BASIC MATRCIES / VECTORS
%--------------------------------------------------------------------------
% NOTE: all data needs to be sorted subjects X data
% the behavioral output
plsinput.y = data(:,3);  % the behavioral output we want to explain
% plsinput.y = data(:,11);

% the explanatory behavioral data - remember the order of it!
plsinput.X = data(:,4:7);
% plsinput.X = data(:,8:10);
% plsinput.X = data(:,4:10);

% optional: exclude all subjects with missings

% z-standardization of variables
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
% PREPARE OUTPUT 
% -------------------------------------------------------------------------
% Bootstrap ratios for included variables
% get significance of extracted latent variable (should be < than 0.05) 
plsres.perm_result.sprob 
sig_LV = 1; % define the latent variable (here it is only one extracted latent variable) 

% Latent variable
% combine data and write table 
time = plsres.boot_result.compare_u(1,sig_LV);
excess_path = plsres.boot_result.compare_u(2,sig_LV);
excess_distance = plsres.boot_result.compare_u(3,sig_LV);
inital_velocity = plsres.boot_result.compare_u(4,sig_LV);
PLSC_LV = table(time, excess_path, excess_distance, inital_velocity);
% layout = plsres.boot_result.compare_u(5,sig_LV);
% landmark = plsres.boot_result.compare_u(6,sig_LV);
% position = plsres.boot_result.compare_u(7,sig_LV);

figure; subplot(1,2,1);
bar(plsres.boot_result.compare_u(:,sig_LV),'k'); hold on;
set(gca,'xticklabels',{'time','path','distance','init.vel','layout', 'landmark', 'position'}, 'fontsize', 12);
% set(gca,'xticklabels',{'layout', 'landmark', 'position'}, 'fontsize', 12);
box off; grid on;
lh = line([0,numel(plsres.boot_result.compare_u)+1],[2,2]);
set(lh, 'color','r','linestyle','--');
lh = line([0,numel(plsres.boot_result.compare_u)+1],[-2,-2]);
set(lh, 'color','r','linestyle','--');
title('LV profile');

subplot(1,2,2);
bar(plsres.boot_result.orig_corr(:,1),'k'); hold on;
set(gca,'xticklabels',{''});
box off; grid on; grid minor; ylim([-1,1]); xlim([0,2]);
lh1 = line([1,1],[plsres.boot_result.llcorr_adj(1,1),plsres.boot_result.ulcorr_adj(1,1)]);
set(lh1, 'color','r');
title('LV correlation with memory score');

% Latent profile scores
% combine data and write table 
id = data(:,1); % subjects_use (excl,:) = []; 
group = data(:,2); 
ms = plsinput.y;
lp = plsres.usc;
PLSC_LP = table(id, group, lp, ms);

% scatter plot 
figure; 
gscatter(plsres.usc, plsinput.y, group); 
set(gca,'fontsize', 12);
xlabel(upper('LV profile score'),'fontweight','bold');
ylabel(upper('memory score'),'fontweight','bold');
[R,P]=corrcoef(plsres.usc, plsinput.y, 'rows', 'complete');
title(strcat('r=',num2str(R(2,1)),', p=',num2str(P(2,1)))); 
%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
% SAVE OUTPUT 
% -------------------------------------------------------------------------
condition='allo'; 
condition='ego'; 
session='0'; 
session='1';
session='2'; 

writetable(PLSC_LV,[path, 'PLSC_LV_', condition, '_', session, '.txt'])
writetable(PLSC_LP,[path, 'PLSC_LP_', condition,  '_', session, '.txt'])
% -------------------------------------------------------------------------

clear all; 