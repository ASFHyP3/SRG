%  test pinv function
clear all;  close all;

load 'Tm.out';

Tminv=pinv(Tm);

[V,D]=eig(Tm*Tm');

for i=1:size(Tm,1)
    ev(i)=D(i,i);
end

lambda=eig(Tm*Tm','nobalance');

[U,S,V]=svd(Tm);

Tminv=V*pinv(S)*U';


