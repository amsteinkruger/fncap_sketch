% Growth Function for Site Class
% Generates Residual
% Based on formon_growthls_FIA_SC

function [out] = GrowthFitResiduals(b,data,p)

W_gen=zeros(p.N,1);

W_sim = [b(1)*ones(p.Ndraws,1),zeros(p.Ndraws,p.T-1)];
for i=2:p.T
	w_t = W_sim(:,i-1);
	u_t = p.U(:,i-1);
	W_sim(:,i) = w_t.*(b(2)./(1+((b(2)-1)./b(3)).*w_t)).*exp(b(4).*u_t - .5*(b(4).^2));
end

W_sim2=zeros(p.T,1);

% Select mean or median

if p.GrowthCurveCalc ==1
	for i=1:p.T;
		W_sim2(i)=mean(W_sim(:,i));
	end
else
	for i=1:p.T;
		W_sim2(i)=median(W_sim(:,i));
	end
end
  
for i=1:p.N
	SA=data(i,1);
	W_gen(i)=W_sim2(SA);
end

if p.LossChoice == 1
	
	% Conventional nonlinear least squares
	out = data(:,2) - W_gen;
	
elseif p.LossChoice == 2

	% Sum of squared relative errors
	out = (data(:,2) - W_gen)./data(:,2);

elseif p.LossChoice == 3
	
	% Sum of squared errors relative to age class average
	out = (data(:,2) - W_gen)./p.SAVolumeAverage;

else

	% Weight by age
	out = (data(:,2) - W_gen)./data(:,1);
	
end

end