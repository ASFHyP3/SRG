% test sentinel rc azimuth spectrum
clear all; close all;

%  read a burst
fp=fopen('unfolded','r');
data=fread(fp,[7301*2 inf],'float32');
cpx=data(1:2:end,:)+j*data(2:2:end,:);
clear data;
cpx(1:9,:)=complex(0.,0.);
subplot(4,1,1);imagesc(abs(cpx)'); colorbar; colormap gray;
ave=mean(mean(abs(cpx)));
caxis([0 ave*3]);

% take some looks
n=10;
mask=ones(n);
q=conv2(abs(cpx),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(4,1,2);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);

%  centroid in these data
prf=1685*4
dopest=cpx(:,1:end-1).*conj(cpx(:,2:end));
n=10;
mask=ones(n);
q=conv2(dopest,mask,'same');
looks=q(1:n:end,1:n:end);
dopcent=sum(q,1);
dopcent(end+1)=dopcent(end); %  add a point to make ends match
dopcent=dopcent./abs(dopcent);
% unwrap estimates
fd=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();
% fit line to these
p=polyfit([21:size(cpx,2)-20],fd(21:size(cpx,2)-20),1); % p(1) is rate in cycles per line
ks=p(1)*prf*prf; % ks is rate in Hz/s

% flatten
phase=pi()*p(1)*linspace(1,size(cpx,2),size(cpx,2)).^2+2*pi*p(2)*linspace(1,size(cpx,2),size(cpx,2));
cpxdop=complex(cos(phase),sin(phase));

flatcpx=repmat((cpxdop),size(cpx,1),1).*cpx;

dopest=flatcpx(:,1:end-1).*conj(flatcpx(:,2:end));
n=10;
mask=ones(n);
q=conv2(dopest,mask,'same');
looks=q(1:n:end,1:n:end);
dopcent=sum(q,1);
dopcent(end+1)=dopcent(end); %  add a point to make ends match
dopcent=dopcent./abs(dopcent);
% unwrap estimates
fdcheck=unwrap(atan2(imag(dopcent),real(dopcent)))/2./pi();

% figure(2);plot(fd,'r');hold on;plot(fdcheck,'b');
% 
% unfocussed proc
mask=ones(1,256);
unf=conv2(flatcpx,mask,'same');
subplot(4,1,3);
imagesc(abs(unf)'); colorbar; colormap(gray);
ave=mean(mean(abs(unf)));
caxis([0 ave*3]);

% looks
n=32;
mask=ones(n);
q=conv2(abs(unf),mask,'same');
looks=q(1:n:end,1:n:end);
subplot(4,1,4);imagesc(looks'); colorbar; colormap gray;
ave=mean(mean(abs(looks)));
caxis([0 ave*3]);

