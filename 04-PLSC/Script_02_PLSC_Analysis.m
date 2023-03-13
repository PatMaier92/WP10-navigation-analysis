clear; close all; clc; format long;
addpath(genpath(pwd)) % add subfolder functions to path

%% Partial Least Square Correlation (PLSC) Analysis 
% @ date 2022-09-05 @ author Patrizia Maier & tracked by git 

% This script applies Partial Least Squares Correlation (PLSC; Krishnan et al., 2011) 
% to extract a latent variable (LV) capturing age-related differences in
% navigation behavior and correlating this LV with memory. 

% This script requires the plscmd toolbox (https://www.rotman-baycrest.on.ca).

%%  Load data and run PLSC 
%--------------------------------------------------------------------------
% LOAD DATA 
%--------------------------------------------------------------------------
% load raw data 
path = '../WP10_data/WP10_results/';

% age by session 1 learning trials
data_age_by_NlS1PT = readtable([path, 'wp10_plsc_age_by_NlS1PT.txt']); 

% age by session 1 probe trials (supplemental analysis) 
data_age_by_NpS1PT = readtable([path, 'wp10_plsc_age_by_NpS1PT.txt']); 

% memory data 
memory_table = readtable([path, 'wp10_plsc_memory.txt']); 

% analysis settings 
data_cell = { data_age_by_NlS1PT data_age_by_NpS1PT data_age_by_NpS1PT };
data_names = { 'age_by_NlS1PT' 'age_by_NpS1PT' 'age_by_NpS1PT' }; 
analysis_method = { 3 3 5 }; % 3=regular behavior PLS; 5=non-rotated behavior PLS
n_conditions = { 1 2 2 }; % number of conditions 
by_group = { 0 0 0 }; % 0=no; 1=yes; note: not required if age is outcome variable 
design_contrasts = { [] [] [1 -1 ]' }; % rows=conditions x groups, sorted as condition in group; colums = desired contrasts

for i=1:numel(data_cell)
    %--------------------------------------------------------------------------
    % CONFIGURATION
    %-------------------------------------------------------------------------- 
    data = data_cell{i};
    file_name = data_names{i};
    
    % behavioral output data
    plsinput.y = data.age;
    
    % behavioral explanatory data
    plsinput.X = table2array(data(:,5:size(data,2)));

    % z-standardization
    plsinput.y = zscore(plsinput.y,0,1);
    plsinput.X = zscore(plsinput.X,0,1);

    % cfg settings
    cfg.pls = [];
    cfg.pls.num_perm = 5000; % number of permutations
    cfg.pls.num_boot = 5000; % number of bootstrap tests
    cfg.pls.clim     = 95; % confidence interval level

    % analysis method: 3=regular behavior PLS; 5=non-rotated behavior PLS
    cfg.pls.method   = analysis_method{i}; 
    
    % add behavioral output data
    cfg.pls.stacked_behavdata = plsinput.y;
    
    % number of conditions 
    n_con = n_conditions{i}; 
    if cfg.pls.method==5
        cfg.pls.stacked_designdata=design_contrasts{i};
    end 

    % number of subjects and data preparation
    if by_group{i}==0
        n_subj = size(plsinput.y,1) / n_con;
        datamat1_allgroups = plsinput.X;
    elseif by_group{i}==1
        n_subj = histc(data(:,2),unique(data(:,2))) / n_con;
        datamat1_group1 = plsinput.X(1:n_subj(1),:);
        datamat1_group2 = plsinput.X(n_subj(1)+1:n_subj(1)+n_subj(2),:);
        datamat1_group3 = plsinput.X(n_subj(1)+n_subj(2)+1:end,:);
    end   
    %--------------------------------------------------------------------------
    %--------------------------------------------------------------------------
    % RUN PLS AND SAVE OUTPUT
    %--------------------------------------------------------------------------
    % input arguments: data, number of subjects, number of conditions, specific settings
    if by_group{i}==0
        plsres = pls_analysis({ datamat1_allgroups }, n_subj, n_con, cfg.pls);
    elseif by_group{i}==1
        plsres = pls_analysis({ datamat1_group1, datamat1_group2, datamat1_group3 }, n_subj, n_con, cfg.pls);
    end
    
    % add relevant input data to pls output 
    plsres.data.group = data.group; 
    plsres.data.age = data.age; 
    plsres.data.memoryAvg = memory_table.memoryAvg; 
    plsres.data.memoryEgo1 = memory_table.memoryEgo1; 
    plsres.data.memoryEgo2 = memory_table.memoryEgo2; 
    plsres.data.memoryAllo1 = memory_table.memoryAllo1; 
    plsres.data.memoryAllo2 = memory_table.memoryAllo2; 

    % calculate two variants of latent profile scores (usc)
    plsres.usc_nav = datamat1_allgroups(:,1:4) * plsres.u(1:4); 
    plsres.usc_post = datamat1_allgroups(:,5:end) * plsres.u(5:end);
       
    % save data file 
    save([path, '/PLSC_', file_name, '/results_m', int2str(cfg.pls.method), '_g', int2str(size(n_subj,1)), '.mat'],'plsres');
    
    clear n_con cfg datamat*; 
%     %--------------------------------------------------------------------------
%     %--------------------------------------------------------------------------
%     % OPTIONAL DATA VIZUALIZATION
%     % -------------------------------------------------------------------------
%     for LV_n=1:numel(plsres.perm_result.sprob)
%         
%         % Latent variable (LV) significance (should be < than 0.05)
%         p=plsres.perm_result.sprob(LV_n);
%         
%         % Latent variable weights (LV weights)
%         % Bootstrap ratios (BSR) (should be < -1.96 or > +1.96)
%         % calculated as correlation/standard error (for method 3) 
%         % or salience u/se (for method 5)
%         BSR = plsres.boot_result.compare_u(:,LV_n);
%                
%         % Plot 
%         % BSR with threshold and LV correlations with 95%-CI
%         if p < 0.05
%             
%             fig = figure('visible','off'); subplot(1,2,1);
%             bar(BSR(:),'k'); hold on;
%             set(gca,'xticklabels',data.Properties.VariableNames(5:end)); 
%             box off; grid on;
%             lh = line([0,size(BSR,1)+1],[1.96,1.96]);
%             set(lh, 'color','r','linestyle','--');
%             lh = line([0,size(BSR,1)+1],[-1.96,-1.96]);
%             set(lh, 'color','r','linestyle','--');
%             % ylim([-12 12]);
%             title(['BSR for LV ', num2str(LV_n), ' with p-value=', num2str(round(p,3))]);
%             hold off;
%             
%             subplot(1,2,2);
%             n_dim = size(plsres.boot_result.orig_corr,1);
%             bar(1:n_dim, plsres.boot_result.orig_corr(:,LV_n),'k'); hold on;
%             box off; grid on; grid minor;
%             for nd = 1:n_dim
%                 lh1 = line([nd,nd],[plsres.boot_result.llcorr_adj(nd,LV_n),plsres.boot_result.ulcorr_adj(nd,LV_n)]);
%                 set(lh1, 'color','r');
%             end
%             xlim([0,n_dim+1]); ylim([-1,1]);
%             title(['Correlation [95%-CI] with memory for LV ', num2str(LV_n)]);
%             hold off;
%             clear n_dim nd lh*;
%             
%             saveas(fig,[path, '/PLSC_', file_name, '/Plot_LV_m', int2str(plsres.method), '_g', int2str(size(n_subj,1)), '_lv', int2str(LV_n), '.png']);
%         end 
%
%         % LV weights correlations with CI based on standard error
%         if plsres.method==3 && p < 0.05
%             
%             cor = plsres.datamatcorrs_lst{1,1}'; % note: datamatcorrs_lst changes shape for some settings
%             se = plsres.boot_result.u_se(:,LV_n);
%             
%             fig = figure('visible','off');
%             n_dim = numel(cor);
%             bar(cor(:),'k'); hold on;
%             set(gca,'xticklabels',data.Properties.VariableNames(5:end));
%             box off; grid on;
%             for nd = 1:n_dim
%                 lh1 = line([nd,nd],[cor(nd)+1.96*se(nd),cor(nd)-1.96*se(nd)]);
%                 set(lh1, 'color','r');
%             end
%             ylim([-1 1]);
%             title('Mean Correlation +- 1.96*SE');
%             hold off;
%             clear n_dim lh*;
%             
%             saveas(fig,[path, '/PLSC_', file_name, '/Plot_LV_CI_m', int2str(plsres.method), '_g', int2str(size(n_subj,1)), '_lv', int2str(LV_n),'.png']);
%             
%         end
% 
%     clear BSR cor se; 
%     end 
%     
%     % Latent profile scores (LPS)
%     % indicates an individual's expression of the profile
%     if plsres.method==3 && numel(plsres.perm_result.sprob)==1 && p < 0.05
%         
%         % Plot
%         % LPS scatter (grouped) and general correlation
%         fig = figure('visible','off');
%         gscatter(data.age, plsres.usc(:,LV_n), data.group);
%         xlabel(upper('Age'),'fontweight','bold');
%         ylabel(upper('LV profile score'),'fontweight','bold');
%         [R,P]=corrcoef(plsres.usc(:,LV_n), data.age, 'rows', 'complete');
%         title(strcat('r=',num2str(R(2,1)),', p=',num2str(P(2,1))));
%         clear R P;
%         saveas(fig,[path, '/PLSC_', file_name, '/Plot_LP_AGE_m', int2str(plsres.method), '_g', int2str(size(n_subj,1)), '.png']);
%         
%         fig = figure('visible','off');
%         y_var = {'memoryAvg' 'memoryEgo1' 'memoryEgo2' 'memoryAllo1' 'memoryAllo2'};
%         for mp=1:5
%             subplot(2,3,mp);
%             gscatter(memory_table.(y_var{mp}), plsres.usc(:,LV_n), data.group);
%             xlabel(upper(y_var{mp}),'fontweight','bold');
%             ylabel(upper('LV profile score'),'fontweight','bold');
%             [R,P]=corrcoef(plsres.usc(:,LV_n), memory_table.(y_var{mp}), 'rows', 'complete');
%             title(strcat('r=',num2str(R(2,1)),', p=',num2str(P(2,1))));
%             xlim([0 1]);
%             clear R P;
%         end
%         saveas(fig,[path, '/PLSC_', file_name, '/Plot_LP_MEM_m', int2str(plsres.method), '_g', int2str(size(n_subj,1)), '.png']);
%     end
%     %--------------------------------------------------------------------------
    clear p n_subj data file_name plsinput plsres;
end

clearvars; 