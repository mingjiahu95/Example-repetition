
function [WSSE,predicted_result] = calc_WSSE(observed_result,all_vars,n_sim,weights)
% all_vars is a vector with each parameter in order
% n_sim is the number of times examplar model is run before averaging

n_dim = 6;
param = struct();
param.criterion = zeros(3,2);
param.between = all_vars(1); param.within = all_vars(2); param.sensitivity = all_vars(3);
param.noise = all_vars(4); param.response_scaling = all_vars(5); 
param.criterion(2,1) = all_vars(6);param.criterion(2,2) = all_vars(7); 
param.criterion(3,1) = all_vars(8); param.criterion(3,2) = all_vars(9);

% all data points are given equal weights if weight matrix not given
if ~exist('weights','var') || isempty(weights)
    w = ones(size(observed_result));
else
    w = weights;
end

% random number generator is seeded to make sure the same set of patterns
% were used for each iteration of parameter search

rng(243437); %this one gives SSE of .0079

%predicted_result = zeros([size(observed_result),n_sim]); % dims:[cond,item type,expt,i_sim] 
predicted_result = zeros([2,5,2,n_sim]);
for isim = 1:n_sim   
    predicted_result(:,:,:,isim) = examplar_model(n_dim,param); % concatenate results from each simulation of the experimental process
end
predicted_result_mean = mean(predicted_result,4);% average along the fourth dimension

SE = (observed_result - predicted_result_mean).^2.*w; %compute squared errors
WSSE = sum(SE(:)); %compute (weighted) sum of squares
end