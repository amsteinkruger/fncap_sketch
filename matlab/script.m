% This modifies an earlier script for stand growth.
% 2025/04/08

clear all; close all; clc;

rng(1949)

b0 = [100, 2, 7000, 0.50]

% Set up fake data.

% First Column is current biomass
% Second column is preious biomass
% Third column is the years between measurement
% Biomass data is reported as cubic feet per acre

data = zeros(10, 4)
data(:, 1) = normrnd(100, 50, 10, 1) % Current Biomass
data(:, 2) = data(:, 1) - normrnd(10, 5, 10, 1) % Previous Biomass
data(:, 3) = 10 % Periods
data(:, 4) = 3 % Site Class Code

% Set up parameters.

p.t0 = 1
p.T = 10
p.obvs = length(data)
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
%options.Algorithm='levenberg-marquardt';
options.TolFun=1.000e-14;

[bhat,resnorm,residual,exitflag] = lsqnonlin(@(b)function_growth(b,data,p),b0,[0,0,0,0],[],options);
bhat

%%

% Generate predictions.

siteclass=3;
count=0;

for i=1:length(data(:,2))
    if data(i,4)==siteclass
        count=count+1;
    end
end

W_obv=zeros(count,3);
W_gen=zeros(count,1);

count=0;
for i=1:length(data(:,2))
    if data(i,3)==siteclass;
        count=count+1;
        W_obv(count,1)=data(i,1);
        W_obv(count,2)=data(i,2);
        W_obv(count,3)=data(i,3);
    end
end

W_sim = [bhat(1)*ones(p.Ndraws,1),zeros(p.Ndraws,p.T-1)];
for i=2:p.T
    w_t = W_sim(:,i-1);
    u_t = p.U(:,i-1);
    W_sim(:,i) = w_t.*(bhat(2)./(1+((bhat(2)-1)./bhat(3)).*w_t)).*exp(bhat(4).*u_t - .5*(bhat(4).^2));
end

W_sim2=zeros(p.T,1);

for i=1:p.T;
    W_sim2(i,1)=mean(W_sim(:,i));
end

%%

% Visualize.

figure
x=linspace(0,90,length(W_sim2));
bmass=zeros(90,1);
scatter(W_obv(:,1),W_obv(:,2),'filled')
hold on
plot(x,W_sim2(:,1))
xlabel('Stand Age')
ylabel('Cu.ft. Biomass')
hold off

%Generate Growth Table
GTable=zeros(18,1);
temp=1;
for i=1:18
    GTable(i,1)=W_sim2(temp,1);
    temp=temp+5;
end
