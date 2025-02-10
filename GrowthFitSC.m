% Fits growth function for single or set of site classes
%
% Created 4.11.17 (MS)
% Modified 20250126 (AS)

clear all; close all; clc;

% location = 'C:\Users\sloggym\desktop\sk\GrowthFit';

location = 'C:\Users\amste\OneDrive\Documents\GitHub\fncap_sketch'

rng(1949)

b0=[116.2194142, 1.8768584, 7233.071142, 0.51759921];
 
% initial parameter guesses 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Read in CSV of Growth Data
% First Column is current biomass
% Second column is preious biomass
% Third column is the years between measurement
% Biomass data is reported as cubic feet per acre
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% filename='NewData/Growth_Data_SC_clean.csv';

% data=csvread(filename);

data = readmatrix('data/dat_fake.csv')

p.t0=1; p.T=110;

p.obvs = length(data);
p.Ndraws = 2000;

% Primes for Halton draws
prim = [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 ...
59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 ...
131 137 139 149 151 157 163 167 173 179 181 191 193 ... 
197 199 211 223 227 229 233 239 241 251 257 263 269 ...
271 277 281 283 293 307 311 313 317 331 337 347 349 ...
353 359 367 373 379 383 389 397 401 409 419 421 431 ...
433 439 443 449 457 461 463 467 479 487 491 499 503 ...
509 521 523 541 547 557 563 569 571 577 587 593 599 ...
601 607 613 617 619 631 641 643 647 653 659 661 673 ...
677 683 691 701 709 719 727 733 739 743 751 757 761 ...
769 773 787 797 809 811 821 823 827 829 839 853 857 ...
859 863 877 881 883 887 907 911 919 929 937 941 947 ...
953 967 971 977 983 991 997];

% Faure and Lemieux (FL sequence) factors

FL = [1,1,3,3,4,9,7,5,9,18,18,8,13,31,9,19,36,33,21,44,43,61,60,56,26,71,...
32,77,26,95,92,47,29,61,57,69,115,63,92,31,104,126,50,80,55,152,114,80,...
83,97,95,150,148,55,80,192,71,76,82,109,105,173,58,143,56,177,203,239,...
196,143,278,227,87,274,264,84,226,163,231,177,95,116,165,131,156,105,...
188,142,105,125,269,292,215,182,294,152,148,144,382,194,346,323,220,...
174,133,324,215,246,159,337,254,423,484,239,440,362,464,376,398,174,...
149,418,306,282,434,196,458,313,512,450,161,315,441,549,555,431,295,...
557,172,343,472,604,297,524,251,514,385,531,663,674,255,519,324,391,...
394,533,253,717,651,399,596,676,425,261,404,691,604,274,627,777,269,...
217,599,447,581,640,666,595,669,686,305,460,599,335,258,649,771,619,...
666,669,707,737,854,925,818,424,493,463,535,782,476,451,520,886,340,...
793,390,381,274,500,581,345,363,1024,514,773,932,556,954,793,294,863,...
393,827,527,1007,622,549,613,799,408,856,601,1072,938,322,1142,873,629,...
1071,1063,1205,596,973,984,875,918,1133,1223,933,1110,1228,1017,701,...
480,678,1172,689,1138,1022,682,613,635,984,526,1311,459,1348,477,716,...
1075,682,1245,401,774,1026,499,1314,743,693,1282,1003,1181,1079,765,...
815,1350,1144,1449,718,805,1203,1173,737,562,579,701,1104,1105,1379,...
827,1256,759,540,1284,1188,776,853,1140,445,1265,802,932,632,1504,856,...
1229,1619,774,1229,1300,1563,1551,1265,905,1333,493,913,1397,1250,612,...
1251,1765,1303,595,981,671,1403,820,1404,1661,973,1340,1015,1649,855,...
1834,1621,1704,893,1033,721,1737,1507,1851,1006,994,923,872,1860];

% Try out haltonset().

p.D = zeros(p.Ndraws, p.T-1);
p.D = net(haltonset(p.T-1, 'Skip', 1000, 'Leap', 1000), p.Ndraws)
p.U = (norminv(p.D))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Set up solver and run minimization problem
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

options = optimoptions('lsqnonlin','Display','iter');
options.MaxFunEvals=1500;
options.MaxIter=500;
options.TolX=1.0e-14;
%options.Algorithm='levenberg-marquardt';
options.TolFun=1.000e-14;
[bhat,resnorm,residual,exitflag] = lsqnonlin(@(b)formon_growthls_FIA_SC(b,data,p),b0,[0,0,0,0],[],options);
bhat

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Generating observations from function file
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% filename='NewData/Growth_Data_SC_clean.csv';
% data=csvread(filename);

siteclass=4;
count=0;
for i=1:length(data(:,2))
if data(i,3)==siteclass
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Visualizing growth function
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
