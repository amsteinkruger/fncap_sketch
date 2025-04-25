%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GrowthFitSC2025
%
%
% Fits growth function for single or set of
% Site classes
%
% Created 4.11.17
% Updated 4/17/25
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Housekeeping

clear all; close all; clc;

% location = 'C:\Users\sloggym\desktop\sk\GrowthFit';
% location = 'C:\Users\David\Dropbox\23 AFRI private\fncap_matlab';
location = 'C:\Users\amste\OneDrive\Documents\GitHub\fncap_sketch\matlab'
addpath(location)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Read in growth data
% From the "README":
% Column one reports the stand age. 
% Column two reports the biomass in cubic feet of growing stock. 
% Column three reports the site class. 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% filename='NewData/Growth_Data_SC_clean.csv';

data=readmatrix('Growth_Data_SC_clean.csv');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Choose site class and calculate maximum stand age and stand age
% average volume
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p.siteclass = 4; % Pick site class
r_t = find(data(:,3)==p.siteclass); % Filter to site class (vector of row positions)
p.T=max(data(r_t,1)); % Get maximum stand age (scalar)
dataAgeClass = data(r_t,:); % data subsetted to row positions in r_t
p.N = length(dataAgeClass); % Number of observations in an age class (scalar, also should be site class?)

% compute age class-specific means
dataVolumes = dataAgeClass(:,2); % i.e. biomasses
[ii,jj] = ndgrid( dataAgeClass(:,1), 1:size( dataVolumes, 2 )) ;
iijj = [ii(:), jj(:)] ; % ???
VolumeSums = accumarray( iijj, dataVolumes(:) ) ;
VolumeCnts = accumarray( iijj, ones( numel( dataVolumes ), 1 )) ;
VolumeByAgeMeans = VolumeSums./VolumeCnts ;
p.SAVolumeAverage = zeros(p.N,1);
for i=1:p.N
	SA = dataAgeClass(i,1);
	p.SAVolumeAverage(i)=VolumeByAgeMeans(SA);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Halton draws
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p.obvs = length(data);
p.Ndraws = 10000;

HaltonTemp = haltonset(p.T,'skip',9);
HaltonTemp = scramble(HaltonTemp,'RR2');
p.U = norminv(net(HaltonTemp,p.Ndraws));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Set up solver and run minimization problem
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

options = optimoptions('lsqnonlin','Display','iter');
options.MaxFunEvals=5000;
options.MaxIter=5000;
options.TolX=1.0e-14;
options.TolFun=1.000e-14;
options.Algorithm='levenberg-marquardt';

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Generate comparison runs
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Loss function choice. 
% LossChoice: 
% 1 = conventional nonlinear least squares; 
% 2 = sum of squared relative errors
% 3 = sum of squared errors relative to age class average
% 4 = weight by 1/(stand age)

% Growth curve estimation
% GrowthCurveCalc:
% 1 = average
% 2 = median
p.GrowthCurveCalc = 1;

% initial guess
% bGuess = [28.9,1.4,7494,0.7]; 
% bGuess = [40,2,5000,0.1]; 
% bGuess = [10,1,1000,1];
bGuess =  [28.9,1.4,7494,0.7];
bGuess2 = [40,2,4000,1];

p.LossChoice = 1;
b0 = bGuess;
[bhatLoss1,resnorm,residual,exitflag] = lsqnonlin(@(b)GrowthFitResiduals(b,dataAgeClass,p),b0,[],[],options);

% p.LossChoice = 3;
% b0 = bGuess2; % initial guess

% [bhatLoss3,resnorm,residual,exitflag] = lsqnonlin(@(b)GrowthFitResiduals(b,dataAgeClass,p),b0,[1,1.1,0,0],[],options);

% p.LossChoice = 2;
% b0 = bhatLoss3; % initial guess

% [bhatLoss2,resnorm,residual,exitflag] = lsqnonlin(@(b)GrowthFitResiduals(b,dataAgeClass,p),b0,[1,1.1,0,0],[],options);

% p.LossChoice = 4;
% b0 = bhatLoss1; % initial guess

% [bhatLoss4,resnorm,residual,exitflag] = lsqnonlin(@(b)GrowthFitResiduals(b,dataAgeClass,p),b0,[],[],options);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Visualizing growth function
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

GrowthCurveLoss1 = GrowthFit(bhatLoss1,dataAgeClass,p);
% GrowthCurveLoss2 = GrowthFit(bhatLoss2,dataAgeClass,p);
% GrowthCurveLoss3 = GrowthFit(bhatLoss3,dataAgeClass,p);
% GrowthCurveLoss4 = GrowthFit(bhatLoss4,dataAgeClass,p);

figure
tt=linspace(1,p.T,p.T);
scatter(dataAgeClass(:,1),dataAgeClass(:,2),'filled')
hold on
  plot(tt,GrowthCurveLoss1,'LineWidth',1.5)
  % plot(tt,GrowthCurveLoss2,'--','color',[.5 .5 .5],'LineWidth',1.5)
  % plot(tt,GrowthCurveLoss3,'-.','color','m','LineWidth',1.5)
  % plot(tt,GrowthCurveLoss4,':','color','green','LineWidth',1.5)
xlabel('Stand Age')
ylabel('Cu.ft. Biomass')
legend({'Data';'NL Least Squares'},'location','eastoutside','FontSize',24);
hold off

% ;'Relative Error';'Relative to SA mean';'NLLS Weighted by Age'
