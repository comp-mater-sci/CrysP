

close all, clear all; %iii=1;
cryst='BCC';  dg='0.01'; Yextr=0.03;

Ngr=50;
Ntsts=12;
Nmod=3;
oscLISTall=zeros(Nmod,Ngr*Ntsts); %init

%%

tst=1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values

model='FCTaylor'; mod=1; file='r0Fbcc_3'; ReadLST;
model='ALAMEL';   mod=mod+1; file='r0Abcc_3'; ReadLST;
model='MASAL2';   mod=mod+1; file='r0Mbcc_3'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='r0Fbcc_4'; ReadLST;
model='ALAMEL';   mod=mod+1; file='r0Abcc_4'; ReadLST;
model='MASAL2';   mod=mod+1; file='r0Mbcc_4'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='r9Fbcc_3'; ReadLST;
model='ALAMEL';   mod=mod+1; file='r9Abcc_3'; ReadLST;
model='MASAL2';   mod=mod+1; file='r9Mbcc_3'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='r9Fbcc_4'; ReadLST;
model='ALAMEL';   mod=mod+1; file='r9Abcc_4'; ReadLST;
model='MASAL2';   mod=mod+1; file='r9Mbcc_4'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='s0Fbcc_3'; ReadLST;
model='ALAMEL';   mod=mod+1; file='s0Abcc_3'; ReadLST;
model='MASAL2';   mod=mod+1; file='s0Mbcc_3'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='s0Fbcc_4'; ReadLST;
model='ALAMEL';   mod=mod+1; file='s0Abcc_4'; ReadLST;
model='MASAL2';   mod=mod+1; file='s0Mbcc_4'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='s9Fbcc_3'; ReadLST;
model='ALAMEL';   mod=mod+1; file='s9Abcc_3'; ReadLST;
model='MASAL2';   mod=mod+1; file='s9Mbcc_3'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='s9Fbcc_4'; ReadLST;
model='ALAMEL';   mod=mod+1; file='s9Abcc_4'; ReadLST;
model='MASAL2';   mod=mod+1; file='s9Mbcc_4'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='t0Fbcc_3'; ReadLST;
model='ALAMEL';   mod=mod+1; file='t0Abcc_3'; ReadLST;
model='MASAL2';   mod=mod+1; file='t0Mbcc_3'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='t0Fbcc_4'; ReadLST;
model='ALAMEL';   mod=mod+1; file='t0Abcc_4'; ReadLST;
model='MASAL2';   mod=mod+1; file='t0Mbcc_4'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='t9Fbcc_3'; ReadLST;
model='ALAMEL';   mod=mod+1; file='t9Abcc_3'; ReadLST;
model='MASAL2';   mod=mod+1; file='t9Mbcc_3'; ReadLST;

tst=tst+1;
pos_ini=(tst-1)*Ngr+1; pos_end=pos_ini-1+Ngr; %ini en end positions in oscLISTall to put oscLOG-values
model='FCTaylor'; mod=1; file='t9Fbcc_4'; ReadLST;
model='ALAMEL';   mod=mod+1; file='t9Abcc_4'; ReadLST;
model='MASAL2';   mod=mod+1; file='t9Mbcc_4'; ReadLST;


%% Check max/min
max(oscLISTall,[],2)
min(oscLISTall,[],2)

%x=-25.:0.5:-10;
%x=-14:0.4:-6;
x=-13:0.4:-5;
%
close all
h=figure; hold on
set(gca,'YTick',[5 15 50 100])
set(gca,'XTick',-12:2:-6) %-14:2:-6) %-25:5:-10)
set(gca,'FontSize',16)%,'FontWeight','bold')

set(gca,'XTickLabel',{'1.e-12','1.e-10','1.e-8','1.e-6'})%{'1.e-14','1.e-12','1.e-10','1.e-8','1.e-6'})
set(gca,'YTickLabel',{'5%','15%','50%','100%'})

nrmFAC=100/(Ngr*Ntsts); % to express in percentage
%{
plot( x,nrmFAC*histc(oscLISTall(1,:),x),  '--k','LineWidth',2);
plot( x,nrmFAC*histc(oscLISTall(2,:),x),  '-k','LineWidth',2);
plot( x,nrmFAC*histc(oscLISTall(3,:),x),  ':k','LineWidth',2);

plot( x,nrmFAC*cumsum(histc(oscLISTall(1,:),x)),  '--k','LineWidth',2);
plot( x,nrmFAC*cumsum(histc(oscLISTall(2,:),x)),  '-k','LineWidth',2);
plot( x,nrmFAC*cumsum(histc(oscLISTall(3,:),x)),  ':k','LineWidth',2);
%}
plot( x,100-nrmFAC*cumsum(histc(oscLISTall(1,:),x)),  '--k','LineWidth',4);
plot( x,100-nrmFAC*cumsum(histc(oscLISTall(2,:),x)),  '-k','LineWidth',4);
plot( x,100-nrmFAC*cumsum(histc(oscLISTall(3,:),x)),  ':k','LineWidth',4);

xlabel('\delta','FontSize',22)
%xlabel('log_1_0(\sigma*)','FontSize',16)
%ylabel('Cumulative histogram (%)','FontSize',16)

xlim([-13 -5]) %([-14 -6]) %([-25 -10])
ylim([0 100])

%%title(tit{iii});

legend(['FC Taylor        ';'ALAMEL           ';'MAS-AL(version 2)'],'Location','NorthEast'); %%%%%%  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

hold off
% Automatic save of figure
%
outName=['oscLISTall_cumul_osc_new'];
saveas(h,outName,'fig')
saveas(h,outName,'jpg')
close all
clear outName h
%}
