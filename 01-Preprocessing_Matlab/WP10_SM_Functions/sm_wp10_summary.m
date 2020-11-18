% CURRENTLY UNUSED %

function sm_wp10_summary(new_file)
% SM_WP10_SUMMARY Write summary file for Starmaze WP10.
%
% Input:
% Folder path, incl. file name for saving the file. 
%
% Returns: Writes a summary file.

% % select variables 
% i_var_path={'time_abs','time_accuracy','time_accuracy_ego','velocity_abs','velocity_accuracy','velocity_accuracy_ego',...
%     'path_abs','path_accuracy','path_accuracy_ego',...
%     'final_distance_to_goal_abs', 'final_distance_to_ego_abs','av_distance_to_goal_abs','av_distance_to_ego_abs',...
%     'distance_accuracy','distance_accuracy_ego','path_score', 'direct_path'};
% i_var_expl={'exploration','success','success_ego','correct_final_alley','final_deviation',...
%     'head_rotation_abs', 'full_head_rotation','head_turn_abs', 'head_turn_accuracy','head_turn_left', 'head_turn_right',...
%     'body_rotation_abs','body_rotation_accuracy', 'body_turn_abs','body_turn_accuracy', 'body_turn_left', 'body_turn_right',...
%     'search_strategy_no','direct_run', 'reoriented','serial','central_focus','random_search','unclassified','failed_strategy',...
%     'allocentric','egocentric'};
% path=readtable(new_file,'Sheet','path','Range','A:AA');
% expl=readtable(new_file,'Sheet','exploration','Range','A:AK');
% 
% d_a=path(path.trial_condition==1 | path.trial_condition==3,:);
% d_e=path(path.trial_condition==2 ,:);
% d_t=path(path.trial_condition==0,:);
% 
% stat_1 = grpstats(path,{'id','group','session'},{'sum','mean'},'DataVars',i_var_path);
% stat_1_d_a = grpstats(d_a,{'id','group','session'},{'sum','mean'},'DataVars',i_var_path);
% stat_1_d_e = grpstats(d_e,{'id','group','session'},{'sum','mean'},'DataVars',i_var_path);
% stat_1_d_t = grpstats(d_t,{'id','group','session'},{'sum','mean'},'DataVars',i_var_path);
% 
% d_a=expl(expl.trial_condition==1 | expl.trial_condition==3,:);
% d_e=expl(expl.trial_condition==2 ,:);
% d_t=expl(expl.trial_condition==0,:);
% 
% stat_2 = grpstats(expl,{'id','group','session'},{'sum','mean'},'DataVars',i_var_expl);
% stat_2_d_a = grpstats(d_a,{'id','group','session'},{'sum','mean'},'DataVars',i_var_expl);
% stat_2_d_e = grpstats(d_e,{'id','group','session'},{'sum','mean'},'DataVars',i_var_expl);
% stat_2_d_t = grpstats(d_t,{'id','group','session'},{'sum','mean'},'DataVars',i_var_expl);
% 
% writetable(stat_1,new_file,'Sheet','summary_path');
% writetable(stat_1_d_a,new_file,'Sheet','summary_path_allo');
% writetable(stat_1_d_e,new_file,'Sheet','summary_path_ego');
% writetable(stat_1_d_t,new_file,'Sheet','summary_path_training');
% 
% writetable(stat_2,new_file,'Sheet','summary_exploration');
% writetable(stat_2_d_a,new_file,'Sheet','summary_exploration_allo');
% writetable(stat_2_d_e,new_file,'Sheet','summary_exploration_ego');
% writetable(stat_2_d_t,new_file,'Sheet','summary_exploration_training');

end