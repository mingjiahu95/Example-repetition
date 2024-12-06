function guess = init_guess(bound)
tmp = size(bound);
ndim = tmp(1);
for i = 1:ndim
    tmp = bound(i,:);
    guess(i) = rand*diff(tmp) + min(tmp);
end