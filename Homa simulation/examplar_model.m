function [pred_result,similarity_per_item] = examplar_model(n_dim , param)

% Input: parameters as a structure array, number of dimensions to represent dot patterns (default to 6); 
% Output: predicted accuracy as an array [expt,cond,itemtype]
% similarity_per_item stores similarity between training items and 3 test
% items in NREP condition, Expt2 as a matrix  [test itemtype, training item]
% crit[i,j] is for experiment i, condition j

% define parameter values
between = param.between; within = param.within; c = param.sensitivity;back = param.noise; 
gam_class = param.response_scaling_class; gam_rec = param.response_scaling_rec; crit = param.criterion;

if ~exist('n_dim','var') || isempty(n_dim)
    n_dim = 6; % number of dimensions to represent dot patterns
end

% specify distortion levels for different itemtypes
low = within*1.2; med = within*2.8; high = within*4.6;

pred_result = zeros(2,5,2);
train = zeros(2,3,100,n_dim);
ntrain = [5,75]; % number of unique training patterns presented
nrep = [15,1]; % number of times the same training pattern was repeated
n_expt = 3; 
n_cond = 2;
n_cat = 3; 
n_type = [5,4];


 
% construct the various test patterns to be
% used in this simulation, one pattern per test item type
prototype = between*rand(n_cat,n_dim); % same 3 prototypes across experiments
foil = between*rand(1,n_dim) + med*randn(1,n_dim);% to be used in expt 2
for icond = 1:n_cond
    for icat = 1:n_cat
        for ipat = 1:ntrain(icond)
              % the training patterns for each condition and category; same
              % across experiments
              train(icond,icat,ipat,:) = prototype(icat,:)+ med*randn(1,n_dim);
        end
    end
end

%% predict classification transfer (expt 1)
% construct test patterns
testpat(2,:) = prototype(1,:); % proto
testpat(3,:) = prototype(1,:) + low*randn(1,n_dim); %low
testpat(4,:) = prototype(1,:) + med*randn(1,n_dim); %med
testpat(5,:) = prototype(1,:) + high*randn(1,n_dim); %high
% simulate process
class = zeros(n_cond,n_type(1));% summed similarity for each itemtype in each condition
for icond = 1:n_cond
    testpat(1,:) = train(icond,1,1,:); %old
    for itype = 1:n_type(1)
        val = zeros(1,n_cat); 
        for icat = 1:n_cat
            sim = zeros(1,ntrain(icond)); % store the similarity of the test pattern to each training pattern
                                          % for each category
            for ipat = 1:ntrain(icond)
                train_item = reshape(train(icond,icat,ipat,:),[1 n_dim]); % coord of a training pattern as a vector
                test_item = testpat(itype,:); % coord of a test pattern as a vector               
                sim(ipat) = exp(-c * sum((train_item - test_item).^2));% similarity measure between the two
            end
            % summed similarity for each category + background noise + response scaling 
            sim_sum = sum(sim); % summed similarity for each category
            val(icat) = ((nrep(icond).*sim_sum)+ back).^gam_class;
        end
        % compute correct classification probability
        class(icond,itype) = val(1)/sum(val);
    end
end

%% predict recognition transfer (expt 2)
%construct test patterns
%set testpat1 equal to the first training example of the first category for condition icond
%testpat1 to be constructed later for two conditions seperately
testpat(2,:) = prototype(1,:) + med*randn(1,n_dim); %novel medium distortion for expt 2
testpat(3,:) = prototype(1,:); % proto
testpat(4,:) = foil; % foil
%simulate process
rec1 = zeros(n_cond,n_type(2));

% store similarity measures for each training item
% sim_store_expt2 = zeros(n_type(2),n_cat*ntrain(2),n_cond);

for icond = 1:n_cond
    testpat(1,:) = train(icond,1,1,:); % construct testpat 1
    for itype = 1:n_type(2)
        sim = zeros(1,n_cat*ntrain(icond)); % store the similarity of the test pattern to each training pattern
        count = 0; % count total number of training patterns across category
        for icat = 1:n_cat
            for ipat = 1:ntrain(icond)
                count = count + 1; % count total number of training patterns across category
                train_item = reshape(train(icond,icat,ipat,:),[1 n_dim]); % coord of a training pattern as a vector
                test_item = reshape(testpat(itype,:),[1 n_dim]); % coord of a test pattern as a vector               
                sim(count) = exp(-c * sum((train_item - test_item).^2));% similarity measure between the two
                
                % store similarity for each training item by item type and cond
                % sim_store_expt2(itype,count,icond) = sim(count);
            end
        end
        % summed similarity over all training patterns + background noise + response scaling 
        sim_sum = sum(sim);
        val = ((nrep(icond)*sim_sum)+ back)^gam_rec;
            
        % compute old response probability
        rec1(icond,itype) = val/(val + crit(2,icond));
    end

end

% %% predict recognition transfer (expt 3)
% %construct test patterns
% %testpat(1) and testpat(2) stay the same as in expt 2 but now testpat3 is the Category-1 prototype
% testpat(3,:) = prototype(1,:); %proto
% 
% %simulate process
% rec2 = zeros(n_cond,3);
% 
% % store similarity measures for each training item
% % sim_store_expt3 = zeros(n_type(3),n_cat*ntrain(2),n_cond);
% 
% for icond = 1:n_cond
%     testpat(1,:) = train(icond,1,1,:); % extract testpat 1
%     for itype = 1:size(testpat,1)
%         sim = zeros(1,n_cat*ntrain(icond)); % store the similarity of the test pattern to each training pattern
%         count = 0; % count total number of patterns across category
%         for icat = 1:n_cat
%             for ipat = 1:ntrain(icond)
%                 count = count + 1;
%                 train_item = reshape(train(icond,icat,ipat,:),[1 n_dim]); % coord of a training pattern as a vector
%                 test_item = reshape(testpat(itype,:),[1 n_dim]); % coord of a test pattern as a vector               
%                 sim(count) = exp(-c * sum((train_item - test_item).^2));% similarity measure between the two
%                 
%                 % store similarity for each training item by item type and cond
%                 % sim_store_expt3(itype,count,icond) = sim(count);               
%             end
%         end
%         % summed similarity for all training patterns + background noise + response scaling 
%         sim_sum = sum(sim);
%         val = ((nrep(icond)*sim_sum)+ back)^gam;
%         
%         % compute old response probability
%         rec2(icond,itype) = val/(val + crit(3,icond));
%     end 
% % testpat_expt3(:,:,icond) = testpat;
% end

% concatenate results from 3 experiments into an array
pred_result(:,:,1) = class;
pred_result(:,1:4,2) = rec1;
%pred_result(:,1:3,3) = rec2;

% summed similarity for 3 types of test item (100 training item for each category) in expt 2 cond 2
% similarity_per_item  = {sim_store_expt2,sim_store_expt3};
end

    

