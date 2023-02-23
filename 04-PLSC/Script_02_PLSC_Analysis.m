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
% load([path, 'wp10_plsc_allSC_by_NeaS1PT.mat']);
% data_allSC_by_NeaS1PT = cellfun(@str2num, m); clear m;
% 
% % load([path, 'wp10_plsc_allS_by_NeaS1PT.mat']);
% % data_allS_by_NeaS1PT = cellfun(@str2num, m); clear m;
% 
% % reshape data for analysis 
% % memory by session & condition combined by navigation 1 ego/allo 
% data_allSC_combined_by_NeaS1PT=data_allSC_by_NeaS1PT;
% data_allSC_combined_by_NeaS1PT(:,3)=sscanf(sprintf('%d%d,',[data_allSC_by_NeaS1PT(:,3).';data_allSC_by_NeaS1PT(:,4).']),'%d,'); 
% data_allSC_combined_by_NeaS1PT(:,4)=[]; 
% 
% % memory for session 2 by condition combined by navigation 1 ego/allo (pre memory as indicator)
% T1 = array2table(data_allSC_by_NeaS1PT);
% T2 = unstack(T1, 5, 3, 'VariableNamingRule', 'preserve'); % reshape session long to wide
% order=[1:3 12 4:11]; 
% data_S2allC_by_NeaS1PT=table2array(T2(:,order));
% clear T1 T2 order; 
% clear data_allSC_by_Nea1PT;
% 
% % ego memory for session 2 by navigation 1 ego/allo (pre memory as indicator)
% data_egoS2_by_NeS1PT=data_S2allC_by_NeaS1PT(data_S2allC_by_NeaS1PT(:,3)==5,:); 
% 
% % allo memory for session 2 by navigation 1 ego/allo (pre memory as indicator)
% data_alloS2_by_NaS1PT=data_S2allC_by_NeaS1PT(data_S2allC_by_NeaS1PT(:,3)==6,:); 
% 
% data_cell = { data_egoS2_by_NeS1PT data_alloS2_by_NaS1PT data_egoS2_by_NeS1PT data_alloS2_by_NaS1PT data_S2allC_by_NeaS1PT data_allSC_combined_by_NeaS1PT};
% data_names = { 'egoS2_by_NeS1PT' 'alloS2_by_NaS1PT' 'egoS2_by_NeS1PT' 'alloS2_by_NaS1PT' 'S2allC_by_NeaS1PT' 'allSC_combined_by_NeaS1PT' }; 
% analysis_method = { 3 3 5 5 5 5 }; 
% n_conditions = { 1 1 1 1 2 4 }; 
% by_group = { 0 0 1 1 1 0 }; 
% design_contrasts = { [] [] ... % n rows = n conditions x n groups, sorted as condition in group; n colums =  n desired contrasts
%     [1 -1 0; 1 0 -1; 0 1 -1]' [1 -1 0; 1 0 -1; 0 1 -1]' ...
%     [1 -1 1 -1 1 -1; 1 -1 0 0 0 0; 0 0 1 -1 0 0; 0 0 0 0 1 -1]' ... 
%     [1 -1 0 0 ; 0 0 1 -1; 1 0 -1 0; 0 1 0 -1]' }; 


% data based on learning navigation trials 
load([path, 'wp10_plsc_allSC_by_NlS1PT.mat']);
data_allSC_by_NlS1PT = cellfun(@str2num, m); clear m;

% load([path, 'wp10_plsc_allS_by_NlS1PT.mat']);
% data_allS_by_NlS1PT = cellfun(@str2num, m); clear m;

% reshape data for analysis 
% memory by session & condition combined by navigation 1 learn
data_allSC_combined_by_NlS1PT=data_allSC_by_NlS1PT;
data_allSC_combined_by_NlS1PT(:,3)=sscanf(sprintf('%d%d,',[data_allSC_by_NlS1PT(:,3).';data_allSC_by_NlS1PT(:,4).']),'%d,'); 
data_allSC_combined_by_NlS1PT(:,4)=[]; 

% memory for session 2 by condition combined by navigation 1 learn (pre memory as indicator)
T1 = array2table(data_allSC_by_NlS1PT);
T2 = unstack(T1, 5, 3, 'VariableNamingRule', 'preserve'); % reshape session long to wide
order=[1:3 12 4:11]; 
data_S2allC_by_NlS1PT=table2array(T2(:,order));
clear T1 T2 order; 
clear data_allSC_by_Nl1PT;

% ego memory for session 2 by navigation 1 learn (pre memory as indicator)
data_egoS2_by_NlS1PT=data_S2allC_by_NlS1PT(data_S2allC_by_NlS1PT(:,3)==5,:); 

% allo memory for session 2 by navigation 1 learn (pre memory as indicator)
data_alloS2_by_NlS1PT=data_S2allC_by_NlS1PT(data_S2allC_by_NlS1PT(:,3)==6,:); 

data_cell = { data_egoS2_by_NlS1PT data_alloS2_by_NlS1PT data_egoS2_by_NlS1PT data_alloS2_by_NlS1PT data_S2allC_by_NlS1PT data_allSC_combined_by_NlS1PT};
data_names = { 'egoS2_by_NlS1PT' 'alloS2_by_NlS1PT' 'egoS2_by_NlS1PT' 'alloS2_by_NlS1PT' 'S2allC_by_NlS1PT' 'allSC_combined_by_NlS1PT' }; 
analysis_method = { 3 3 5 5 5 5 }; 
n_conditions = { 1 1 1 1 2 4 }; 
by_group = { 0 0 1 1 1 0 }; 
design_contrasts = { [] [] ... % n rows = n conditions x n groups, sorted as condition in group; n colums =  n desired contrasts
    [1 -1 0; 1 0 -1; 0 1 -1]' [1 -1 0; 1 0 -1; 0 1 -1]' ...
    [1 -1 1 -1 1 -1; 1 -1 0 0 0 0; 0 0 1 -1 0 0; 0 0 0 0 1 -1]' ... 
    [1 -1 0 0 ; 0 0 1 -1; 1 0 -1 0; 0 1 0 -1]' }; 


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
    % RUN PLS
    %--------------------------------------------------------------------------
    % input arguments: data, number of subjects, number of conditions, specific settings
    if by_group{i}==0
        plsres = pls_analysis({ datamat1_allgroups }, n_subj, n_con, cfg.pls);
    elseif by_group{i}==1
        plsres = pls_analysis({ datamat1_group1, datamat1_group2, datamat1_group3 }, n_subj, n_con, cfg.pls);
    end
    % plsres.perm_result.sprob
    
    save([path, 'PLSC_full_results_', file_name, '_m', int2str(cfg.pls.method),'.mat'],'plsres');
    
    clear n_subj n_con cfg datamat*; 
    %--------------------------------------------------------------------------
    %--------------------------------------------------------------------------
    % SAVE OUTPUT
    % -------------------------------------------------------------------------
    PLSC_LV = []; 
    for LV_n=1:numel(plsres.perm_result.sprob)
        p=plsres.perm_result.sprob(LV_n); 
        
        % Latent variable significance (should be < than 0.05)
        LV = repmat(LV_n,2,1);
        LV_p = repmat(round(p,3),2,1);
        
        % Latent variable weights
        % Bootstrap ratios (BSR) (should be < -1.96 or > +1.96)
        % correlations with behavioral data and standard error 
        BSR = plsres.boot_result.compare_u(:,LV_n);
        if size(plsres.datamatcorrs_lst{1,1},1)==1 % TBD CHECK for different methods/conditions
            cor = plsres.datamatcorrs_lst{1,1}'; 
        else
            cor = plsres.datamatcorrs_lst{1,1}(LV_n,:)'; 
        end
        se = plsres.boot_result.u_se(:,LV_n); % u = plsres.u(:,LV_n); 
        
        % BSR table 
        latency = [BSR(1); cor(1)];
        excess_path = [BSR(2); cor(2)];
        excess_distance = [BSR(3); cor(3)];
        rotation_velocity = [BSR(4); cor(4)];
        layout = [BSR(5); cor(5)];
        landmark = [BSR(6); cor(6)];
        position = [BSR(7); cor(7)];
        if size(BSR,1)==8
            pre_memory = [BSR(8); cor(8)];
            plsc_lv = table(LV, LV_p, latency, excess_path, excess_distance, rotation_velocity, layout, landmark, position, pre_memory);
        else
            plsc_lv = table(LV, LV_p, latency, excess_path, excess_distance, rotation_velocity, layout, landmark, position);
        end
        PLSC_LV=[PLSC_LV; plsc_lv]; 
        clear latency excess_path excess_distance rotation_velocity layout landmark position pre_memory plsc_lv;
        
        % BSR plots 
        if plsres.method==3
            
            figure; subplot(1,2,1);
            n_dim=numel(cor);
            bar(cor(:)./se,'k'); hold on;
            if size(cor,1)==8
                set(gca,'xticklabels',{'latency','path','distance','init.vel','layout', 'landmark', 'position', 'pre.memory'});
            else
                set(gca,'xticklabels',{'latency','path','distance','init.vel','layout', 'landmark', 'position'});
            end
            box off; grid on;
            lh = line([0,size(BSR,1)+1],[2,2]);
            set(lh, 'color','r','linestyle','--');
            lh = line([0,size(BSR,1)+1],[-2,-2]);
            set(lh, 'color','r','linestyle','--');
            ylim([-12 12]);
            title(['BSR for LV with p-value=', num2str(round(p,3))]);
            hold off;
            
            subplot(1,2,2);
            bar(cor(:),'k'); hold on;
            if size(cor,1)==8
                set(gca,'xticklabels',{'latency','path','distance','init.vel','layout', 'landmark', 'position', 'pre.memory'});
            else
                set(gca,'xticklabels',{'latency','path','distance','init.vel','layout', 'landmark', 'position'});
            end
            box off; grid on;
            for nd = 1:n_dim
                lh1 = line([nd,nd],[cor(nd)+1.96*se(nd),cor(nd)-1.96*se(nd)]);
                set(lh1, 'color','r');
            end
            ylim([-1 1]);
            title('Mean Correlation +- 1.96*SE');
            hold off;
            clear n_dim lh*; 

        elseif plsres.method==5 
       
            figure; subplot(1,2,1);
            bar(BSR(:),'k'); hold on;
            if size(BSR,1)==8
                set(gca,'xticklabels',{'latency','path','distance','init.vel','layout', 'landmark', 'position', 'pre.memory'});
            else
                set(gca,'xticklabels',{'latency','path','distance','init.vel','layout', 'landmark', 'position'});
            end
            box off; grid on;
            lh = line([0,size(BSR,1)+1],[2,2]);
            set(lh, 'color','r','linestyle','--');
            lh = line([0,size(BSR,1)+1],[-2,-2]);
            set(lh, 'color','r','linestyle','--');
            ylim([-12 12]);
            title(['BSR for LV with p-value=', num2str(round(p,3))]);
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
            title('Correlation LV with memory');
            hold off;
            clear n_dim nd lh*; 
            
        end
        % saveas(p,'plot.jpeg')
        
    end 
    
    writetable(PLSC_LV,[path, 'PLSC_LV_', file_name, '_m', int2str(plsres.method), '.txt']);
    clear PLSC_LV p LV_p BSR cor u se; 
    
    % Latent profile scores
    % indicates an individual's expression of the profile
    if plsres.method==3
        
        LV = repmat(LV_n,size(data,1),1);
        id = data(:,1);
        group = data(:,2);
        memory_score = data(:,3);
        latent_profile_score = plsres.usc(:,LV_n);
        PLSC_LP = table(LV, id, group, memory_score, latent_profile_score);
        
        writetable(PLSC_LP, [path, 'PLSC_LP_', file_name, '_m', int2str(plsres.method), '.txt']);
        clear PLSC_LP LV id memory_score latent_profile_score;
        
        figure;
        gscatter(plsres.usc(:,LV_n), plsinput.y, group);
        xlabel(upper('LV profile score'),'fontweight','bold');
        ylabel(upper('memory score'),'fontweight','bold');
        [R,P]=corrcoef(plsres.usc(:,LV_n), plsinput.y, 'rows', 'complete');
        title(strcat('r=',num2str(R(2,1)),', p=',num2str(P(2,1))));
        clear R P group;
        % saveas(p,'plot.jpeg')
        
    end

    clear data file_name plsinput plsres; 
    %--------------------------------------------------------------------------
end

clear all; 