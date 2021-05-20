% triplet time series viewing
clear all; close all;

width=2200;
lines=1487;

iplot=0;
imovie=1-iplot;
nframes=120;

phasemin=-pi()/4;   %   for movie scaling aesthetics
phasemax=pi()/4;

% load data with limits desired
lim=[601 1100 301 800];
amp=zeros(lim(2)-lim(1)+1,lim(4)-lim(3)+1);

fd=fopen('unwlist','r');
for i=1:100000
    line=fgetl(fd);
    if length(line)>1
        tripname(:,i)=line(:);
    else
        break;
    end
end
fclose(fd);
N=size(tripname,2);

% create a reference image
figure(1);
fd=fopen(tripname(:,1)','r');
a=fread(fd,[width*2 lines],'float32');
fclose(fd);
ref=a(1:width,:).^0.2;
imagesc(ref');
ampmed=median(median(ref));
caxis([0 ampmed*2]);

nn=1;
mask=ones(nn)/nn/nn;
for i=1:N
    fd=fopen(tripname(:,i)','r');
    a=fread(fd,[width*2 lines],'float32');
    fclose(fd);
    tempph=mod(a(width+lim(1):width+lim(2),lim(3):lim(4))+pi(),2*pi())-pi();
    smooth=conv2(tempph,mask,'same');
%     temp2=smooth(1:2:end,1:2:end);
    tripph(:,:,i)=smooth;
    amp=amp+a(lim(1):lim(2),lim(3):lim(4)).^0.2/N;
    clear a;
end
disp('Data loaded');
width=size(tempph,1);
lines=size(tempph,2);

%  amplitude image
% figure(1);
% imagesc(amp');
% colormap gray;
ampmed=median(median(amp));
% caxis([0 ampmed*2]);
 
%  create figure 3 not behind figure 2
figure(3);
pos=get(gcf,'Position');
pos(1)=1;
pos(2)=1;
set(gcf,'Position',pos);
figure(2);
% pos=get(gcf,'Position');
% pos(3)=pos(3)*2;
% pos(4)=pos(4)*2;
% set(gcf,'Position',pos);
% plot images and look up pixels
stack=(sum(tripph,3))/N;
stack=tripph(:,:,24);
subplot(1,2,1);
imagesc(stack');
caxis([-pi()/8 pi()/8]);
axis image;
subplot(1,2,2);
imagesc(amp');
caxis([0 ampmed*2]);
axis image;
%     print -depsc 'location_map.eps'

if iplot == 1
    for i=1:100
        fprintf('Getting position\n');
        set(0,'CurrentFigure',2);
%         figure(2);
        [x,y]=ginput(1);
        fprintf('Cursor: %d %d\n',x,y);
        if x < 1 || y < 1 || x > width || y > lines
            return
        end
        v=tripph(round(x),round(y),:);
        v=v(:);
%         figure(3);
        set(0,'CurrentFigure',3);
        xx=linspace(0,N-1,N);
        scatter(xx,v);
        ax=axis;
        ax(3)=-pi();
        ax(4)=pi();
        axis(ax);
        title(['Location: ' num2str(round(x)) ' ' num2str(round(y))]);
        set(0,'CurrentFigure',2);
        fprintf('Getting another input...\n');
 end
    
end

if imovie == 1
    figure(10);
    
    % create a color table
    colormap default;
    map=colormap;
    
    phimin=phasemin;
    phimax=phasemax;
        
    clear M;
    kframe=0;
    for i=1:N %nframes
%         t=i/nframes*tmax
%         for j=1:length(xx)
%             if t >= xx(j)
%                 f1=j;
%             end
%         end
%         f2=min(f1+1,length(xx));
%         phi1=phi(:,:,f1);
%         phi2=phi(:,:,f2);
%         if f1 == f2
%             frac=0;
%         else
%             frac=(t-xx(f1))/(xx(f2)-xx(f1));
%         end
%         phiframe=phi1*(1-frac)+phi2*frac;
        phiframe=tripph(:,:,i);
        phiframe=max(phiframe,phimin);
        phiframe=min(phiframe,phimax);
        colorframe=round((phiframe-phimin)/(phimax-phimin)*64);
        colorframe=max(colorframe,1);
        colorframe=min(colorframe,64);
        red=reshape(map(colorframe,1),width,lines);
       green=reshape(map(colorframe,2),width,lines);
       blue=reshape(map(colorframe,3),width,lines);
        scale=1;
        pic(:,:,1)=red'.*scale;
        pic(:,:,2)=green'.*scale;
        pic(:,:,3)=blue'.*scale;

        M(i)=im2frame(pic); %getframe;
        %     aviobj=addframe(aviobj,M(i));
%         if mod(i,nframes/12) == 0
            figure(11);
            kframe=kframe+1;
            subplot(6,8,kframe);
            %image(flipdim(imrotate(picpic,-90),1));
            image(pic);
            axis image;
            axis off;
            title(tripname(1:end-4,kframe)');
%         end
        
    end
    % aviobj=close(aviobj);
%     mpgwrite(M, map, 'sbas.mpg');
%     figure(11);
%     print -depsc 'sbas_time_series.eps'
%     figure(10);
%     print -depsc 'cumulative_deformation.eps'
    
end

figure;
for i=1:46
 image(frame2im(M(i)));
 pause(0.5);
end


