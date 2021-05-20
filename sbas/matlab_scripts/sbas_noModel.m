clear all; close all; clc;
% least squares solution for pixel time series

iplot=0;
imovie=1;

% load data
load dat_looks
load tsx_info
dtk=load('timedeltas.out');
% SBAS least squares
% at Each pixel, we solve for velocity at (n-1) time inverval
r_ref=218;   %  reference pixel location
az_ref=622;
velocity=zeros(nr_s,naz_s,n-1);
count=0;
for jj=1:naz_s
    disp(jj);
    for ii=1:nr_s
        count=count+1;
        d=phase_s(ii,jj,:)-phase_s(r_ref,az_ref,:);
        d=d(:);
        velocity(ii,jj,:)=Tm\d;
    end
end
save velocity_solution velocity
% To see the time series at pixel (ix,iy):
% Define vector dtk = [dt1 dt2 ... dt(n-1)], (n-1) time invervals between
% the nearest two InSAR acquisition time
% ix = 50;
% iy = 50;
% v = velocity(ix,iy,:);
% v = v(:);
% phi=zeros(n,1);
% for kk=2:n
%     phi(kk)=phi(kk-1)+dtk(kk-1).*v(kk-1);
% end
% plot(phi);

if iplot == 1
% plot images and look up pixels
stack=sum(phase_s,3);
figure(2);
subplot(1,2,1);
imagesc(stack');
phasemin=min(min(min(phase_s)));
phasemax=max(max(max(phase_s)));
axis image;
subplot(1,2,2);
imagesc(amp_s');
axis image;
for i=1:1000
    figure(2);
    [x,y]=ginput(1);
    v=velocity(round(x),round(y),:);
    v=v(:);
    phi=zeros(n,1);
    for kk=2:n
        phi(kk)=phi(kk-1)+dtk(kk-1).*v(kk-1);
    end
    figure(3);
    xx=linspace(0,sum(dtk),n);
    xx(1)=0;
    xx(2:n)=cumsum(dtk);
    scatter(xx,phi);
    axis([0 sum(dtk) phasemin phasemax]);
    title(['Location: ' num2str(round(x)) ' ' num2str(round(y))]);
end

end

if imovie == 1
%  make a movie
tmin=0;
tmax=sum(dtk);
nframes=120;
xx(1)=0;
xx(2:n)=cumsum(dtk);
figure(10);

% create a color table
colormap default;
map=colormap;

% integrate the velocities first
phi=zeros(nr_s,naz_s,n);
for kk=2:n
    phi(:,:,kk)=phi(:,:,kk-1)+dtk(kk-1)*velocity(:,:,kk-1);
end

amp_s=max(amp_s,3000);
amp_s=min(amp_s,8000);
phimin=-4;
phimax=10;

scale=((amp_s-3000))/(5000);
fid=fopen('225690730_272660730.c','r');
temp=fread(fid,[nr_s*2 naz_s],'float32');
fclose(fid);
corr=temp(nr_s+1:nr_s*2,:).^0.35;
thresh=0.6;
corr(find(corr<thresh))=0;
%corr=ones(nr_s,naz_s);
% delete 'kilauea.avi';
% aviobj = avifile('kilauea.avi');
clear M;
kframe=0;
for i=1:nframes
    t=i/nframes*tmax
    for j=1:length(xx)
        if t >= xx(j)
            f1=j;
        end
    end
    f2=min(f1+1,length(xx));
    phi1=phi(:,:,f1);
    phi2=phi(:,:,f2);
    if f1 == f2
        frac=0;
    else
        frac=(t-xx(f1))/(xx(f2)-xx(f1));
    end
    phiframe=phi1*(1-frac)+phi2*frac;
    phiframe=max(phiframe,phimin);
    phiframe=min(phiframe,phimax);
    colorframe=round((phiframe-phimin)/(phimax-phimin)*64);
    colorframe=max(colorframe,1);
    colorframe=min(colorframe,64);
    for k=1:nr_s
        for kk=1:naz_s
            red(k,kk)=map(colorframe(k,kk),1);
            green(k,kk)=map(colorframe(k,kk),2);
            blue(k,kk)=map(colorframe(k,kk),3);
        end
    end
    pic(:,:,1)=red.*corr.*scale;
    pic(:,:,2)=green.*corr.*scale;
    pic(:,:,3)=blue.*corr.*scale;
    for m=1:naz_s
        picpic(:,m,:)=pic(:,naz_s+1-m,:);
    end
    %colorframe=colorframe.*amp_s./ampmax;
    figure(10);
    %image(imrotate(pic,-90));
    image(picpic);
    %caxis([-30 110]);
    axis image;
    M(i)=getframe;
%     aviobj=addframe(aviobj,M(i));
    if mod(i,nframes/12) == 0
        figure(11);
        kframe=kframe+1;
        subplot(3,4,kframe);
        %image(imrotate(pic,-90));
        image(picpic);
        axis image;
        axis off;
    end
        
end
% aviobj=close(aviobj);
mpgwrite(M, map, 'kilauea.mpg');
figure(11);
print -depsc 'kilauea_time_series.eps'

end


