% function A=computeDeviationToIdealValue(i_total,i_ideal)
% % computeDeviationToIdealValue: Calculates deviation of actual compared to ideal value.
% %
% % Input: 
% % i_total is actual value (e.g. time, path length).
% % i_ideal is ideal value (e.g. time, path length).
% %
% % Returns: A is accuracy/deviation.
% 
% A=((i_total-i_ideal)/i_ideal)*100;
% if A <=0
%     A=A*(-1); % return absolute value
% end