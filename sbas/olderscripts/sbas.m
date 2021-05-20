clear all;clc;
% extract linear background motion plus a offset(SSE) of each pixels
% load data
load dat_looks
load tsx_info
% % SBAS linear inversion
r_ref=100;
az_ref=200;
np=naz_s*nr_s;
dphase=zeros(np*N,1);
count=0;
for jj=1:naz_s
    disp(jj);
    for ii=1:nr_s
        count=count+1;
        d=phase_s(ii,jj,:)-phase_s(r_ref,az_ref,:);
        d=d(:);
        dphase(((count-1)*N+1):count*N)=d;
    end
end
% check and flag 1 for data that across the SSE
t_event=2012+6/12.+4/360. % decyear(2012,06,08);
% t_event=2012+5/12.+15/360. % decyear(2012,06,08);
offsetflag=zeros(N,1);
for ii=1:N
    if(deltime(ii,3)<t_event&&deltime(ii,4)>t_event)
       offsetflag(ii)=1; 
    end
    if(deltime(ii,3)>t_event&&deltime(ii,4)<t_event)
       offsetflag(ii)=-1; 
    end
end
% poly coeffients
P=zeros(n-1,1);
P(:,1) = 1;
BM=Tm*P; 
Gi=[BM offsetflag];
G=kron(speye(np),Gi);
I2=kron(speye(np),[0 1]);
e=ones(nr_s,1);
Di=spdiags([e -1*e], 0:1, nr_s, nr_s);
Dr=kron(speye(naz_s),Di); 
e=ones(naz_s,1);
Dj=spdiags([e -1*e], 0:1, naz_s, naz_s);
Daz=kron(Dj,speye(nr_s));
D=[Dr*I2;Daz*I2];
alpha=0; % 10^5.1;
m=(G'*G+alpha*(D'*D))\(G'*dphase);
velocity=zeros(nr_s,naz_s);
offset=zeros(nr_s,naz_s);
count=0;
for jj=1:naz_s
    for ii=1:nr_s
        count=count+1;
        velocity(ii,jj)=m((count-1)*2+1);%radians/day
        offset(ii,jj)=m(count*2);
    end
end
days=365;
output=[amp_s;velocity.*mask_s.*days];
fid=fopen(sprintf('Abg_%d.un',days),'w');
fwrite(fid,output,'float');
fid=fopen(sprintf('Aoffset.un'),'w');
fwrite(fid,[amp_s;offset.*mask_s],'float');
