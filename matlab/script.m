% This modifies an earlier script for stand growth.
% 2025/04/15

clear all; close all; clc;

rng(0112358)

b0 = [100, 2, 7000, 0.50]

obs = 100

% Set up fake data.

% First Column is current biomass
% Second column is previous biomass
% Third column is the years between measurement
% Biomass data is reported as cubic feet per acre

data = zeros(obs, 4)
data(:, 1) = normrnd(100, 25, obs, 1) % Current Biomass
data(:, 2) = data(:, 1) - normrnd(10, 2.5, obs, 1) % Previous Biomass
data(:, 3) = 10 % Periods
data(:, 4) = 3 % Site Class Code

% Set up parameters.

p.t0 = 1
p.T = 10
p.obvs = obs
p.Ndraws = 100

% Set up noise.

p.D = zeros(p.Ndraws, p.T - 1) % Does pre-allocating help? Probably not.

p.D = net(haltonset(p.T - 1), p.Ndraws)

p.U = norminv(p.D)

% Optimize

options = optimoptions('lsqnonlin','Display','iter');
options.MaxFunEvals=1500;
options.MaxIter=500;
options.TolX=1.0e-14;
options.TolFun=1.000e-14;

[bhat,resnorm,residual,exitflag] = lsqnonlin(@(b)function_objective(b,data,p),b0,[0,0,0,0],[],options);

bhat

% Generate predictions.

data_simulated = data
data_simulated(:, 5) = function_growth(bhat, data, p) % Simulate growth.
data_simulated(:, 6) = data_simulated(:, 2) + data_simulated(:, 5) % Get final biomass.

% Visualize.

hold on
scatter(data_simulated(:, 2), data_simulated(:, 1))
scatter(data_simulated(:, 2), data_simulated(:, 6))
xlabel('Biomass (0)')
ylabel('Biomass (1)')
hold off

% hold on
% scatter(data_simulated(:, 2), data_simulated(:, 1) - data_simulated(:, 2))
% scatter(data_simulated(:, 2), data_simulated(:, 5))
% xlabel('Biomass (0)')
% ylabel('Biomass (Change)')
% hold off
