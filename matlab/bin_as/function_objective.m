% This sets up a wrapper for function_growth in script.m.
% 2025/04/15

function out = function_objective(b, data, p)

W_obv = data

W_sim2 = function_growth(b, data, p)

out = abs(W_obv(:, 1) - (W_obv(:, 2) + W_sim2));

end