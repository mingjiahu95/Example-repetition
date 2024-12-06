bdd = [.001, 20.000;...  % between,
       .001, 2.000;...  % within,
       .001, 5.000;...  % sensitivity,
       .000, 20.000;...  % noise,
       .001, 5.000;...  % response_scaling,
       .001, 30.000;...  % crit2_rep,
       .001, 30.000;...  % crit2_nrep,
       .001, 30.000;...  % crit3_rep,
       .001, 30.000];  % crit3_nrep,


observed_accuracy(1,:,:) = [0.964  0.958  0.920  0.828;
    0.971  0.965  0.936  0.852];
observed_accuracy(2,:,1:3) = [0.888  0.626  0.155;
    0.806  0.789  0.221];
observed_accuracy(3,:,1:3) = [0.863  0.583  0.862;
    0.786  0.768  0.938];

start_params = [3.135,0.201,0.771,0.000,1.539,23.505,12.024,29.818,13.809];


problem = createOptimProblem('fmincon','x0',init_guess(bdd),'objective',@(x) calc_WSSE(observed_accuracy,x,1000),...
    'lb',bdd(:,1),'ub',bdd(:,2));
gs = GlobalSearch('MaxTime',60*30,'StartPointsToRun','bounds','Display','iter');
[x,fval] = run(gs,problem);