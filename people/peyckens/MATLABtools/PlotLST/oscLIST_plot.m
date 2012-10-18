
iii=1,

cryst='BCC';  dg='0.01';

model='FCTaylor';file='BTC1F'; ReadLST; 
model='ALAMEL';file='BTC1A'; ReadLST; 
model='MASAL1';file='BTC1M'; ReadLST; 
model='MASAL2';file='BTC1-M'; ReadLST;
LOGosc_Bz= oscLIST; 

%{
model='FCTaylor';file='BzF'; ReadLST; 
model='ALAMEL';file='BzA'; ReadLST; 
model='MASAL1';file='BzM'; ReadLST; 
model='MASAL2';file='Bz-M'; ReadLST;

LOGosc_Bz= oscLIST;  
%}

% the files were:
%{
file='r0M1p13'; ReadLST; 
file='r0M1p14'; ReadLST; 
file='r9M1p13'; ReadLST; 
file='r9M1p14'; ReadLST; 

file='s0M1p13'; ReadLST; 
file='s0M1p14'; ReadLST; 
file='s9M1p13'; ReadLST; 
file='s9M1p14'; ReadLST; 

file='t0M1p13'; ReadLST; 
file='t0M1p14'; ReadLST; 
file='t9M1p13'; ReadLST; 
file='t9M1p14'; ReadLST; 
for ii=1:6 %join the o3.smt and o4.smt textures
    LOGoscM1p1{ii}= [oscLIST{2*ii-1} oscLIST{2*ii}];
end
%}

%{
file='r0-M1p13'; ReadLST; 
file='r0-M1p14'; ReadLST; 
file='r9-M1p13'; ReadLST; 
file='r9-M1p14'; ReadLST; 

file='s0-M1p13'; ReadLST; 
file='s0-M1p14'; ReadLST; 
file='s9-M1p13'; ReadLST; 
file='s9-M1p14'; ReadLST; 

file='t0-M1p13'; ReadLST; 
file='t0-M1p14'; ReadLST; 
file='t9-M1p13'; ReadLST; 
file='t9-M1p14'; ReadLST; 
for ii=1:6 %join the o3.smt and o4.smt textures
    LOGoscminM1p1{ii}= [oscLIST{2*ii-1} oscLIST{2*ii}];  
end
%{
file='r0-Mbcc3'; ReadLST; 
file='r0-Mbcc4'; ReadLST; 
file='r9-Mbcc3'; ReadLST; 
file='r9-Mbcc4'; ReadLST; 

file='s0-Mbcc3'; ReadLST; 
file='s0-Mbcc4'; ReadLST; 
file='s9-Mbcc3'; ReadLST; 
file='s9-Mbcc4'; ReadLST; 

file='t0-Mbcc3'; ReadLST; 
file='t0-Mbcc4'; ReadLST; 
file='t9-Mbcc3'; ReadLST; 
file='t9-Mbcc4'; ReadLST; 
%}

% the files were:
%{
file='r0Abcc_3'; ReadLST; file='r0Mbcc_3'; ReadLST; %
file='r0Abcc_4'; ReadLST; file='r0Mbcc_4'; ReadLST; %
file='r9Abcc_3'; ReadLST; file='r9Mbcc_3'; ReadLST; %
file='r9Abcc_4'; ReadLST; file='r9Mbcc_4'; ReadLST; %

file='s0Abcc_3'; ReadLST; file='s0Mbcc_3'; ReadLST; %
file='s0Abcc_4'; ReadLST; file='s0Mbcc_4'; ReadLST; %
file='s9Abcc_3'; ReadLST; file='s9Mbcc_3'; ReadLST; %
file='s9Abcc_4'; ReadLST; file='s9Mbcc_4'; ReadLST; %

file='t0Abcc_3'; ReadLST; file='t0Mbcc_3'; ReadLST; %
file='t0Abcc_4'; ReadLST; file='t0Mbcc_4'; ReadLST; %
file='t9Abcc_3'; ReadLST; file='t9Mbcc_3'; ReadLST; %
file='t9Abcc_4'; ReadLST; file='t9Mbcc_4'; ReadLST; %
%}
%}
%%
% AFTERWARDS RUN:
%{
for ii=1:12 %split between Alamel runs and Masal runs
    oscLOG_A{ii}=oscLIST{2*ii-1};
    oscLOG_M{ii}=oscLIST{2*ii};
end
%

for ii=1:6 %join the o3.smt and o4.smt textures
    LOGoscA{ii}= [oscLOG_A{2*ii-1} oscLOG_A{2*ii}];
    LOGoscM{ii}= [oscLOG_M{2*ii-1} oscLOG_M{2*ii}];
end
%}

%OR:
for ii=1:6 %join the o3.smt and o4.smt textures
    %LOGoscminM{ii}= [oscLIST{2*ii-1} oscLIST{2*ii}];
    %LOGoscminM1p1{ii}= [oscLIST{2*ii-1} oscLIST{2*ii}];  
    LOGoscM1p1{ii}= [oscLIST{2*ii-1} oscLIST{2*ii}];
end
%}
%% 
%%

tit{1}='r0  - rolling def. along 0° to RD      ';t{1}= 'r0-FAM12-1_1p1-34'; %12-bcc-34';
tit{2}='r90 - rolling def. along 90° to RD     ';t{2}='r90-FAM12-1_1p1-34'; %12-bcc-34';
tit{3}='s0  - simple shear def. along 0° to RD ';t{3}= 's0-FAM12-1_1p1-34'; %12-bcc-34';
tit{4}='s90 - simple shear def. along 90° to RD';t{4}='s90-FAM12-1_1p1-34'; %12-bcc-34';
tit{5}='t0  - wire drawing def. along 0° to RD ';t{5}= 't0-FAM12-1_1p1-34'; %12-bcc-34';
tit{6}='t90 - wire drawing def. along 90° to RD';t{6}='t90-FAM12-1_1p1-34'; %12-bcc-34';

%% plot Bz
tit='BTC1 - simple shear def. along RD (RD-ND: shear plane)';%'Bz - simple shear def. along RD (RD-ND: shear plane)';
t= 'BTC1-FAM12-1p0';%'Bz-FAM12-1p0';
XX=[-20:2.5:20];

h=figure; hold on %%LOG_OSC PLOT

for iii=1:4
for jjj=1:17
    LOGosc_Bz_avg2{iii}(jjj)=(LOGosc_Bz{iii}(2*jjj-1)+LOGosc_Bz{iii}(2*jjj))/2.;
end
end
%
plot(XX, LOGosc_Bz_avg2{1},  'k+','MarkerSize',14);
plot(XX, LOGosc_Bz_avg2{2},  'rx','MarkerSize',14);
plot(XX, LOGosc_Bz_avg2{3},  'g<','MarkerSize',14);
plot(XX, LOGosc_Bz_avg2{4},  'b^','MarkerSize',14);
%
xlabel('GB initial misorientation (ND= rotation axis)','FontSize',14)
ylabel('log_1_0(osc) [osc:measure of oscilatory behavior]','FontSize',14)
ylim([-23 -10])
title(tit);
%
legend(['FC TAYLOR   ';'ALAMEL      ';'MASAL-1(1.0)';'MASAL-2(1.0)'],'Location','EastOutside'); %%%%%%  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
hold off
% Automatic save of figure
outName=[t '_LOGosc'];
saveas(h,outName,'fig')
saveas(h,outName,'jpg')
close all
clear outName h

h=figure; hold on  %%%OSC-PLOt
%
for iii=1:4
for jjj=1:17
    osc_Bz_avg2{iii}(jjj)= 10.^(LOGosc_Bz_avg2{iii}(jjj));
end
end
%
plot(XX, osc_Bz_avg2{1},  'k+','MarkerSize',14);
plot(XX, osc_Bz_avg2{2},  'rx','MarkerSize',14);
plot(XX, osc_Bz_avg2{3},  'g<','MarkerSize',14);
plot(XX, osc_Bz_avg2{4},  'b^','MarkerSize',14);
%
xlabel('GB initial misorientation (ND= rotation axis)','FontSize',14)
ylabel('log_1_0(osc) [osc:measure of oscilatory behavior]','FontSize',14)
%%%%%%%%%%%ylim([-23 -10])
title(tit);
%
legend(['FC TAYLOR   ';'ALAMEL      ';'MASAL-1(1.0)';'MASAL-2(1.0)'],'Location','EastOutside'); %%%%%%  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
hold off
% Automatic save of figure
outName=[t '_osc'];
saveas(h,outName,'fig')
saveas(h,outName,'jpg')
close all
clear outName h
%% plot LOGosc
for iii=1:6

h=figure; hold on

%plot(0:length(oscilLOG),[NaN('single') oscLIST{iii}],  'kx','MarkerSize',12);
%plot(0:length(oscilLOG),[NaN('single') oscLIST{iii+1}],'k0','MarkerSize',12);
plot( LOGoscF{iii},  'k+','MarkerSize',12);
plot( LOGoscA{iii},  'rx','MarkerSize',12);
plot( LOGoscM{iii},  'g<','MarkerSize',10);
plot( LOGoscM1p1{iii},  'g>','MarkerSize',10);
plot( LOGoscminM{iii},  'b^','MarkerSize',10);
plot( LOGoscminM1p1{iii},  'bv','MarkerSize',10);

xlabel('grain number','FontSize',16)
ylabel('measure of oscilatory behavior','FontSize',16)
ylim([-23 -10])

title(tit{iii});

legend(['FC TAYLOR   ';'ALAMEL      ';'MASAL-1(1.0)';'MASAL-1(1.1)';'MASAL-2(1.0)';'MASAL-2(1.1)'],'Location','EastOutside'); %%%%%%  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

hold off
% Automatic save of figure
%
outName=[t{iii} '_osc'];
saveas(h,outName,'fig')
saveas(h,outName,'jpg')
close all
clear outName h
%}

end

%% plot cumulative distr of LOGosc

x=-25.:1:-10; % contains the limits of the log(osc)

for iii=1:6
    n_F{iii} = cumsum(histc(LOGoscF{iii},x));
    n_A{iii} = cumsum(histc(LOGoscA{iii},x));
    n_M{iii} = cumsum(histc(LOGoscM{iii},x));
    n_M1p1{iii} = cumsum(histc(LOGoscM1p1{iii},x));
    n_minM{iii} = cumsum(histc(LOGoscminM{iii},x));  
    n_minM1p1{iii} = cumsum(histc(LOGoscminM1p1{iii},x));    
end

%%
for iii=1:6

h=figure; hold on

%plot(0:length(oscilLOG),[NaN('single') oscLIST{iii}],  'kx','MarkerSize',12);
%plot(0:length(oscilLOG),[NaN('single') oscLIST{iii+1}],'k0','MarkerSize',12);
plot( x,n_F{iii},  'k:','LineWidth',2);%'MarkerSize',12);
plot( x,n_A{iii},  'r--','LineWidth',2);%'MarkerSize',12);
plot( x,n_M{iii},        'g-', 'LineWidth',3)%'MarkerSize',12);
plot( x,n_M1p1{iii},     'g--', 'LineWidth',3)%'MarkerSize',12);
plot( x,n_minM{iii},     'b-', 'LineWidth',2)%'MarkerSize',12);
plot( x,n_minM1p1{iii},  'b--', 'LineWidth',2)%'MarkerSize',12);

xlabel('log_1_0(osc)','FontSize',16)
ylabel('Cumulative histogram (%)','FontSize',16)
ylim([0 100])

title(tit{iii});

legend(['FC TAYLOR   ';'ALAMEL      ';'MASAL-1(1.0)';'MASAL-1(1.1)';'MASAL-2(1.0)';'MASAL-2(1.1)'],'Location','SouthEast'); %%%%%%  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

hold off
% Automatic save of figure
%
outName=[t{iii} '_cumul_osc'];
saveas(h,outName,'fig')
saveas(h,outName,'jpg')
close all
clear outName h
%}

end






%}