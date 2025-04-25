%Growth Function for Site Class
%Generates Residual

function [out] = formon_growthls_FIA_SC(b,data,p)



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

W_sim = [b(1)*ones(p.Ndraws,1),zeros(p.Ndraws,p.T-1)];
for i=2:p.T
	w_t = W_sim(:,i-1);
	u_t = p.U(:,i-1);
	W_sim(:,i) = w_t.*(b(2)./(1+((b(2)-1)./b(3)).*w_t)).*exp(b(4).*u_t - .5*(b(4).^2));
end

  W_sim2=zeros(p.T,1);

  for i=1:p.T;
      W_sim2(i,1)=mean(W_sim(:,i));
  end
  
  for i=1:count
      SA=W_obv(i,1);
      W_gen(i,1)=W_sim2(SA,1);
  end

   out=W_obv(:,2)-W_gen;

end