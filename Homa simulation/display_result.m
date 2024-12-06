clear;
clc;
n_sim = 10000;
n_dim = 6;
param = struct();
param.criterion = zeros(3,2);
param.between = 4.000; param.within = 0.226; param.sensitivity = 0.368;
param.noise = 0; param.response_scaling_class = 1.000; param.response_scaling_rec = 1.922;
param.criterion(2,1) = 200;param.criterion(2,2) = 100; 

predicted_result = zeros([2,5,2,n_sim]);
parfor isim = 1:n_sim   
    predicted_result(:,:,:,isim) = examplar_model(n_dim,param); % concatenate results from each simulation of the experimental process
end
predicted_result_mean = mean(predicted_result,4);% average along the fourth dimension
disp(predicted_result_mean);