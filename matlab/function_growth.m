% This sets up a growth function for script.m.
% 2025/04/15

function out = function_growth(b,data,p)

W_obv = data;

W_gen = zeros(length(data), 1);

W_sim = [b(1) * ones(p.Ndraws, 1), zeros(p.Ndraws, p.T - 1)];

for i=2:p.T
	w_t = W_sim(:,i-1);
	u_t = p.U(:,i-1);
	W_sim(:,i) = w_t.*(b(2)./(1+((b(2)-1)./b(3)).*w_t)).*exp(b(4).*u_t - .5*(b(4).^2));
end

out = sum(W_sim, 2);

% This is where the code breaks without modification. Indexing by SA
% doesn't work or make immediate sense, so instead (in the preceding line) 
% I sum the means calculated in the previous step, add them to initial biomass, 
% and get the sum of differences between initial and final biomass. The resulting
% vector is the thing to minimize. But that wastes a lot of information 
% from the data so it must be wrong; I'm just not seeing the alternative.

% for i=1:count
%     SA = W_obv(i,1);
%     W_gen(i,1) = W_sim2(SA, 1);
% end
% 
% out = W_obv(:, 2) - W_gen

end