% This sets up a function for script.m.
% 2025/04/08.

function out = function_growth(b,data,p)

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
if data(i,4)==siteclass
    count=count+1;
    W_obv(count,1)=data(i,1);
    W_obv(count,2)=data(i,2);
    W_obv(count,3)=data(i,3);
end
end

W_sim = [b(1)*ones(p.Ndraws,1),zeros(p.Ndraws,p.T-1)];
for i=2:p.T
	w_t = W_sim(:,i-1);
	u_t = p.U(:,i-1);
	W_sim(:,i) = w_t.*(b(2)./(1+((b(2)-1)./b(3)).*w_t)).*exp(b(4).*u_t - .5*(b(4).^2));
end

W_sim2=zeros(p.T,1);

for i=1:p.T
    W_sim2(i,1)=mean(W_sim(:,i));
end

% This is where the code breaks without modification. Indexing by SA
% doesn't work (or make immediate sense), so instead I sum the means
% calculated in the previous step, add them to initial biomass, and get the
% sum of differences between initial and final biomass. The resulting
% vector, or better still (for Julia) the sum of absolute values of
% elements of the result vector, is the thing to minimize. But that wastes
% a lot of information from the data so it must be wrong, I'm just not seeing the alternative in this set-up.

% for i=1:count
%     SA = W_obv(i,1);
%     W_gen(i,1) = W_sim2(SA, 1);
% end

% out = W_obv(:, 2) - W_gen

W_sim3 = sum(W_sim2);

out = abs(W_obv(:, 1) - W_obv(:, 2) + W_sim3);

end