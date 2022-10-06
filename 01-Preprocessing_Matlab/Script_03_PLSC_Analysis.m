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
% CONFIG
%--------------------------------------------------------------------------
cfg.pls = [];
cfg.pls.method   = 3; % regular behavior PLS
cfg.pls.num_perm = 5000; % number of permutations
cfg.pls.num_boot = 5000; % number of bootstrap tests
cfg.pls.clim     = 95; % confidence interval level

%--------------------------------------------------------------------------
% LOAD DATA 
%--------------------------------------------------------------------------
path = '../WP10_data/WP10_results/';
load([path, 'wp10_plsc_allo_s2.mat']);
data = cellfun(@str2num, m);
clear m;

%--------------------------------------------------------------------------
% CREATE THE BASIC MATRCIES / VECTORS
%--------------------------------------------------------------------------
% NOTE: all data needs to be sorted subjects X data
% the behavioral output
plsinput.y = data(:,3);  % the behavioral output we want to explain

% the explanatory behavioral data - remember the order of it!
% % with groups 
% plsinput.X = { data(data(:,2)==1, 4:end), ...
%     data(data(:,2)==2, 4:end), data(data(:,2)==3, 4:end) };
% without groups 
plsinput.X = data(:, 4:end);

% % exclude all subjects with missings
% A = find(isnan(plsinput.y)==1);  % find missing age values
% B = find(any(isnan(plsinput.X),2)==1); % find missing brain data
% C = vertcat(A,B); 
% excl = unique(C); % define all subjects with missings on one variable
% 
% plsinput.y(excl) = []; % exclude subjects
% plsinput.X(excl,:) = []; % exclude subjects

% z-standardization of variables
plsinput.y = zscore(plsinput.y,0,1);
plsinput.X = zscore(plsinput.X,0,1);
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
% RUN PLS
%--------------------------------------------------------------------------
% add the 'behavioral data'
cfg.pls.stacked_behavdata = plsinput.y;

% add the 'design matrix' with conditions 
% cfg.pls.stacked_designdata = [data(:,3)]; 

% input arguments: data, number of subjects, number of conditions, specific settings
% plsres = pls_analysis({plsinput.X},size(plsinput.y,1),1,cfg.pls);
% % with groups
% n_subj = [ size(plsinput.X{1},1) size(plsinput.X{2},1) size(plsinput.X{3},1) ];
% plsres = pls_analysis(plsinput.X, n_subj, 1, cfg.pls);
% without groups 
n_subj = size(plsinput.y,1); 
plsres = pls_analysis({ plsinput.X }, n_subj, 1, cfg.pls);

%--------------------------------------------------------------------------
% PREPARE OUTPUT 
% -------------------------------------------------------------------------
% Bootstrap ratios for included variables
% get significance of extracted latent variable (should be < than 0.05) 
plsres.perm_result.sprob 
sig_LV = 1; % define the latent variable (here it is only one extracted latent variable) 

% write output table
time = plsres.boot_result.compare_u(1,sig_LV);
excess_path = plsres.boot_result.compare_u(2,sig_LV);
presence = plsres.boot_result.compare_u(3,sig_LV);
initial_rotation = plsres.boot_result.compare_u(4,sig_LV);
rotation = plsres.boot_result.compare_u(5,sig_LV);
PLSC_LV = table(time, excess_path,presence,initial_rotation, rotation);

subplot(1,2,1);
bar(plsres.boot_result.compare_u(:,sig_LV),'k'); hold on;
set(gca,'xticklabels',{'time','path','presence','init. rot', 'rot'}, 'fontsize', 12);
box off; grid on;
lh = line([0,6],[2,2]);
set(lh, 'color','r','linestyle','--');
lh = line([0,6],[-2,-2]);
set(lh, 'color','r','linestyle','--');
title('LV profile');

subplot(1,2,2);
bar(plsres.boot_result.orig_corr(:,1),'k'); hold on;
set(gca,'xticklabels',{''});
box off; grid on; grid minor; ylim([-1,1]); xlim([0,2]);
lh1 = line([1,1],[plsres.boot_result.llcorr_adj(1,1),plsres.boot_result.ulcorr_adj(1,1)]);
set(lh1, 'color','r');
title('LV correlation with memory score');

%% Latent profile scores

% combine data and write table 
subjects_use = data(:,1); % subjects_use (excl,:) = []; 
group = data(:,2); 
lp = plsres.usc;
PLSC_LP = table(subjects_use, group, lp);

% scatter plot 
figure; 
gscatter(plsres.usc, plsinput.y, group); 
set(gca,'fontsize', 12);
xlabel(upper('LV profile score'),'fontweight','bold');
ylabel(upper('memory score'),'fontweight','bold');
[R,P]=corrcoef(plsres.usc, plsinput.y, 'rows', 'complete');
title(strcat('r=',num2str(R(2,1)),', p=',num2str(P(2,1)))); 

clear all; 