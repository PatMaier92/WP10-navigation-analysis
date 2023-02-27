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
% load raw data 
path = '../WP10_data/WP10_results/';


% % data based on ego/allo navigation trials 
% load([path, 'wp10_plsc_all_by_NeaS1PT.mat']);
% data_all_by_NeaS1PT = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allSC_by_NeaS1PT.mat']);
% data_allSC_by_NeaS1PT = cellfun(@str2num, m); clear m;
% 
% % reshape data for analysis 
% % memory by session & condition combined by navigation 1 ego/allo 
% data_allSC_combined_by_NeaS1PT=data_allSC_by_NeaS1PT;
% data_allSC_combined_by_NeaS1PT(:,3)=sscanf(sprintf('%d%d,',[data_allSC_by_NeaS1PT(:,3).';data_allSC_by_NeaS1PT(:,4).']),'%d,'); 
% data_allSC_combined_by_NeaS1PT(:,4)=[]; 
% clear data_allSC_by_NeaS1PT;
% 
% data_cell = { data_all_by_NeaS1PT data_all_by_NeaS1PT data_all_by_NeaS1PT ...
%     data_allSC_combined_by_NeaS1PT data_allSC_combined_by_NeaS1PT data_allSC_combined_by_NeaS1PT data_allSC_combined_by_NeaS1PT };
% data_names = { 'all_by_NeaS1PT' 'all_by_NeaS1PT' 'all_by_NeaS1PT' ...
%     'allSC_combined_by_NeaS1PT' 'allSC_combined_by_NeaS1PT' 'allSC_combined_by_NeaS1PT' 'allSC_combined_by_NeaS1PT' }; 
% analysis_method = { 3 3 5 3 3 5 5 }; 
% n_conditions = { 1 1 1 4 4 4 4 }; 
% by_group = { 0 1 1 0 1 0 1 }; 
% design_contrasts = { [] [] ... % n rows = n conditions x n groups, sorted as condition in group; n colums =  n desired contrasts
%     [1 -1 0; 1 0 -1; 0 1 -1]' ... 
%     [] [] ...
%     [1 -1 0 0 ; 0 0 1 -1; 1 0 -1 0; 0 1 0 -1]' ...
%     [1 -1 0 0 1 -1 0 0 1 -1 0 0; 0 0 1 -1  0 0 1 -1 0 0 1 -1; 1 0 -1 0 1 0 -1 0 1 0 -1 0; 0 1 0 -1 0 1 0 -1 0 1 0 -1]' }; 


% % data based on learning navigation trials 
% load([path, 'wp10_plsc_all_by_NlS1PT.mat']);
% data_all_by_NlS1PT = cellfun(@str2num, m); clear m;
% 
% load([path, 'wp10_plsc_allSC_by_NlS1PT.mat']);
% data_allSC_by_NlS1PT = cellfun(@str2num, m); clear m;
% 
% % reshape data for analysis 
% % memory by session & condition combined by navigation 1 learn
% data_allSC_combined_by_NlS1PT=data_allSC_by_NlS1PT;
% data_allSC_combined_by_NlS1PT(:,3)=sscanf(sprintf('%d%d,',[data_allSC_by_NlS1PT(:,3).';data_allSC_by_NlS1PT(:,4).']),'%d,'); 
% data_allSC_combined_by_NlS1PT(:,4)=[]; 
% clear data_allSC_by_NlS1PT;
%  
% data_cell = { data_all_by_NlS1PT data_all_by_NlS1PT data_all_by_NlS1PT ...
%     data_allSC_combined_by_NlS1PT data_allSC_combined_by_NlS1PT data_allSC_combined_by_NlS1PT data_allSC_combined_by_NlS1PT };
% data_names = { 'all_by_NlS1PT' 'all_by_NlS1PT' 'all_by_NlS1PT' ...
%     'allSC_combined_by_NlS1PT' 'allSC_combined_by_NlS1PT' 'allSC_combined_by_NlS1PT' 'allSC_combined_by_NlS1PT' }; 
% analysis_method = { 3 3 5 3 3 5 5 }; 
% n_conditions = { 1 1 1 4 4 4 4 }; 
% by_group = { 0 1 1 0 1 0 1 }; 
% design_contrasts = { [] [] ... % n rows = n conditions x n groups, sorted as condition in group; n colums =  n desired contrasts
%     [1 -1 0; 1 0 -1; 0 1 -1]' ... 
%     [] [] ...
%     [1 -1 0 0 ; 0 0 1 -1; 1 0 -1 0; 0 1 0 -1]' ...
%     [1 -1 0 0 1 -1 0 0 1 -1 0 0; 0 0 1 -1  0 0 1 -1 0 0 1 -1; 1 0 -1 0 1 0 -1 0 1 0 -1 0; 0 1 0 -1 0 1 0 -1 0 1 0 -1]' }; 


for i=1:numel(data_cell)
    %--------------------------------------------------------------------------
    % CONFIGURATION
    %-------------------------------------------------------------------------- 
    data = data_cell{i};
    file_name = data_names{i};
    
    % behavioral output data
    plsinput.y = data(:,4);
    
    % behavioral explanatory data
    plsinput.X = data(:,5:size(data,2));

    % z-standardization
    plsinput.y = zscore(plsinput.y,0,1);
    plsinput.X = zscore(plsinput.X,0,1);

    % cfg settings
    cfg.pls = [];
    cfg.pls.num_perm = 500; % 5000; % number of permutations
    cfg.pls.num_boot = 500; % 5000; % number of bootstrap tests
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

    save([path, '/PLSC_', file_name, '/full_results_m', int2str(cfg.pls.method), '_g', int2str(size(n_subj,1)), '.mat'],'plsres');
    
    clear n_con cfg datamat*; 
    %--------------------------------------------------------------------------
    %--------------------------------------------------------------------------
    % DATA VIZUALIZATION
    % -------------------------------------------------------------------------
    for LV_n=1:numel(plsres.perm_result.sprob)
        
        % Latent variable significance (should be < than 0.05)
        p=plsres.perm_result.sprob(LV_n);
        
        % Latent variable weights
        % Bootstrap ratios (BSR) (should be < -1.96 or > +1.96)
        % calculated as correlation/standard error (for method 3) 
        % or salience u/se (for method 5)
        BSR = plsres.boot_result.compare_u(:,LV_n);
               
        % Plot 
        % BSR with threshold and LV correlations with 95%-CI
        if p < 0.05
            
            fig = figure('visible','off'); subplot(1,2,1);
            bar(BSR(:),'k'); hold on;
            set(gca,'xticklabels',{'latency','path','distance','init.rot','layout','landmark','position'});
            box off; grid on;
            lh = line([0,size(BSR,1)+1],[1.96,1.96]);
            set(lh, 'color','r','linestyle','--');
            lh = line([0,size(BSR,1)+1],[-1.96,-1.96]);
            set(lh, 'color','r','linestyle','--');
            ylim([-12 12]);
            title(['BSR for LV ', num2str(LV_n), ' with p-value=', num2str(round(p,3))]);
            hold off;
            
            subplot(1,2,2);
            n_dim = size(plsres.boot_result.orig_corr,1);
            bar(1:n_dim, plsres.boot_result.orig_corr(:,LV_n),'k'); hold on;
            box off; grid on; grid minor;
            for nd = 1:n_dim
                lh1 = line([nd,nd],[plsres.boot_result.llcorr_adj(nd,LV_n),plsres.boot_result.ulcorr_adj(nd,LV_n)]);
                set(lh1, 'color','r');
            end
            xlim([0,n_dim+1]); ylim([-1,1]);
            title(['Correlation [95%-CI] with memory for LV ', num2str(LV_n)]);
            hold off;
            clear n_dim nd lh*;
            
            saveas(fig,[path, '/PLSC_', file_name, '/Plot_LV_m', int2str(plsres.method), '_g', int2str(size(n_subj,1)), '_lv', int2str(LV_n),'.png']);
        
%             if plsres.method==3
%                 
%                 % Plot
%                 % LV weights correlations with CI based on standard error
%                 cor = plsres.datamatcorrs_lst{1,1}'; % TBD datamatcorrs change change shape based on settings
%                 se = plsres.boot_result.u_se(:,LV_n);
%                 
%                 fig = figure('visible','off');
%                 n_dim = numel(cor);
%                 bar(cor(:),'k'); hold on;
%                 set(gca,'xticklabels',{'latency','path','distance','init.rot','layout', 'landmark', 'position'});
%                 box off; grid on;
%                 for nd = 1:n_dim
%                     lh1 = line([nd,nd],[cor(nd)+1.96*se(nd),cor(nd)-1.96*se(nd)]);
%                     set(lh1, 'color','r');
%                 end
%                 ylim([-1 1]);
%                 title('Mean Correlation +- 1.96*SE');
%                 hold off;
%                 clear n_dim lh*;
%                 
%             end
            
        end
        
        clear BSR cor se;
    end 
    
    % Latent profile scores (LPS)
    % indicates an individual's expression of the profile
    if plsres.method==3 && numel(plsres.perm_result.sprob)==1 && p < 0.05
              
        % Plot 
        % LPS scatter (grouped) and general correlation
        group = data(:,2);
        fig=figure('visible','off');
        gscatter(plsres.usc(:,LV_n), plsinput.y, group);
        xlabel(upper('LV profile score'),'fontweight','bold');
        ylabel(upper('memory score'),'fontweight','bold');
        [R,P]=corrcoef(plsres.usc(:,LV_n), plsinput.y, 'rows', 'complete');
        title(strcat('r=',num2str(R(2,1)),', p=',num2str(P(2,1))));
        clear R P group;
        saveas(fig,[path, '/PLSC_', file_name, '/Plot_LP_m', int2str(plsres.method), '_g', int2str(size(n_subj,1)), '.png']);
        
    end

    clear p n_subj data file_name plsinput plsres; 
    %--------------------------------------------------------------------------
end

clear all; 