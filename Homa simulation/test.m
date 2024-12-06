observed_result(:,:,1) = [0.980  0.980  0.920  0.840;
    0.990  0.970  0.960  0.890];
observed_result(:,1:3,2) = [0.900  0.600  0.200;
    0.780  0.800  0.240];
observed_result(:,1:3,3) = [0.860  0.580  0.870;
    0.780  0.780  0.920];

best_params = [3.135,0.201,0.771,0.000,1.539,23.505,12.024,29.818,13.809];
%best_params = [3.135,0.201,0.771,0.000,1.539,23.505,12.024,23.505,12.024];

% param = struct();
% param.criterion = zeros(3,2);
% param.between = best_params(1); param.within = best_params(2); param.sensitivity = best_params(3);
% param.noise = best_params(4); param.response_scaling = best_params(5); 
% param.criterion(2,1) = best_params(6);param.criterion(2,2) = best_params(7); 
% param.criterion(3,1) = best_params(8); param.criterion(3,2) = best_params(9);
% 

[WSSE,predicted_results] = calc_WSSE(observed_result,best_params,10000);
pred_result_mean = mean(predicted_results,4);

results(2,:,:,:) = permute(pred_result_mean,[2,1,3]);
results(1,:,:,:) = permute(observed_result,[2,1,3]);
disp(results) %format: [obs/pred, item type, cond, expt_num]
