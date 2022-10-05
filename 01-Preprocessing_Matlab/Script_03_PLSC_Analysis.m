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
load([path, 'wp10_plsc_data.mat']);
% load([path, 'wp10_plsc_data_cond.mat']);
data = cellfun(@str2num, m);
clear m;

%--------------------------------------------------------------------------
% CREATE THE BASIC MATRCIES / VECTORS
%--------------------------------------------------------------------------
% NOTE: all data needs to be sorted subjects X data
% the behavioral output
plsinput.y = data(:,3);  % the behavioral output we want to explain
% plsinput.y = data(:,4);  % the behavioral output we want to explain

% the explanatory behavioral data - remember the order of it!
% with groups 
plsinput.X = { data(data(:,2)==1, 4:end), ...
    data(data(:,2)==2, 4:end), data(data(:,2)==3, 4:end) };
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
% cfg.pls.stacked_designdata = [data(:,3) data(:,4)]; 
% cfg.pls.stacked_designdata = [data(:,3)]; 

% input arguments: data, number of subjects, number of conditions, specific settings
% plsres = pls_analysis({plsinput.X},size(plsinput.y,1),1,cfg.pls);
% with groups
n_subj = [ size(plsinput.X{1},1) size(plsinput.X{2},1) size(plsinput.X{3},1) ];
plsres = pls_analysis(plsinput.X, n_subj, 1, cfg.pls);
% without groups 
n_subj = size(plsinput.y,1); 
plsres = pls_analysis({ plsinput.X }, n_subj, 1, cfg.pls);

%--------------------------------------------------------------------------
% PREPARE OUTPUT 
% -------------------------------------------------------------------------
% Bootstrap ratios for included variables
% get significance of extracted latent variable (should be < than 0.05) 
plsres.perm_result.sprob 
bar(plsres.perm_result.sprob)
sig_LV = 1; % define the latent variable (here it is only one extracted latent variable) 

% correlation of behavior with brain scores (???)
bar(plsres.lvcorrs(:,1)); % unsure if row-wise or column-wise

% write output table
time = plsres.boot_result.compare_u(1,sig_LV);
excess_path = plsres.boot_result.compare_u(2,sig_LV);
presence= plsres.boot_result.compare_u(3,sig_LV);
initial_rotation = plsres.boot_result.compare_u(4,sig_LV);
rotation = plsres.boot_result.compare_u(5,sig_LV);

PLSC_LV = table(time, excess_path,presence,initial_rotation, rotation); 
bar(plsres.boot_result.compare_u(:,sig_LV));

%% Latent profile scores

% subjects included in analysis 
subjects_use = data(:,1); 
% subjects_use (excl,:) = []; 

% combine data & write output table
lp = plsres.usc;
PLSC_LP = table(subjects_use, lp);

clear all; 