%% Data plotting of Euler angles of (a pair of) 2 grains
% Assumed to be known:
%%% "grain" : nr. of the 1st of 2 grains to be plotted
%%%            So grain is an UNeven number
grainCouple=ceil(grain/2);

%% Define legend
z{1} =['grain ' int2str(grain)   '  \phi_1'];%z is CELL 
z{2} =['grain ' int2str(grain)   '  \Phi'];
z{3} =['grain ' int2str(grain)   '  \phi_2'];
z{4} =['grain ' int2str(grain+1) '  \phi_1'];
z{5} =['grain ' int2str(grain+1) '  \Phi'];
z{6} =['grain ' int2str(grain+1) '  \phi_2'];
z{7} =['GB ' int2str(grainCouple) '  n_1 (%)'];
z{8} =['GB ' int2str(grainCouple) '  n_2 (%)'];
z{9} =['GB ' int2str(grainCouple) '  n_3 (%)'];
leg=char(z{1},z{2},z{3},z{4},z{5},z{6},z{7},z{8},z{9}); %CELL->MATRIX

clear z;
%% Define LineSpec_LIST
LineSpec_LIST{1} ='r-';
LineSpec_LIST{2} ='r:';
LineSpec_LIST{3} ='r.';
LineSpec_LIST{4} ='g-';
LineSpec_LIST{5} ='g:';
LineSpec_LIST{6} ='g.';
LineSpec_LIST{7} ='k-';
LineSpec_LIST{8} ='k:';
LineSpec_LIST{9} ='k.';

%% PLOT 
h=figure;
hold on, 

%1st grain
for ii=1:3 %ADEM: Accumulated Delta_Epsilon_VonMises
    plot(ADEM,Euler{ii,grain},LineSpec_LIST{ii},'MarkerSize',5,'LineWidth',2)
end
%2nd grain
for ii=1:3 %ADEM: Accumulated Delta_Epsilon_VonMises
    plot(ADEM,Euler{ii,grain+1},LineSpec_LIST{ii+3},'MarkerSize',5,'LineWidth',2)
end
%Grain Boundary normal in %, so: *100
%for ii=1:3 %ADEM: Accumulated Delta_Epsilon_VonMises
%    plot(ADEM,GBnormal{ii,grainCouple}*100,LineSpec_LIST{ii+6},'MarkerSize',5,'LineWidth',2)
%end

xlabel('\epsilon_v_M [/]','FontSize',16)
ylabel('Euler angle [°]','FontSize',16)

legend(leg,'location','EastOutside')

clear leg LineSpec_LIST ii
%% LIMITS of plot
xlim([0. .3])%0.15])%0.6])
ylim([-110. 180])
%% TITLE of plot

step=1; %Here, we want INITIAL Euler angles
gr1=['grain' num2str(grain) ];
gr1=[gr1 '(' num2str(Euler{1,grain}(step)) '°,' num2str(Euler{2,grain}(step)) '°,' num2str(Euler{3,grain}(step)) '°)' ];
gr2=['grain' num2str(grain+1) ];
gr2=[gr2 '(' num2str(Euler{1,grain+1}(step)) '°,' num2str(Euler{2,grain+1}(step)) '°,' num2str(Euler{3,grain+1}(step)) '°)' ];
GBeuler=['(' num2str(GBEuler{1,grainCouple}(step)) '°,' num2str(GBEuler{2,grainCouple}(step)) '°,???°)'];
%{
%1st grain = uneven grain number
gr1=['grain' num2str(grain)   '(172.5°,47.5°,52.5°)'];
%2nd grain = even grain number
gr2=['grain' num2str(grain+1) '(120.0°,42.5°,85.0°)'];

switch(grain)
    case{1,2}
        GBeuler='(88.744°,55.205°,0°)';
    case{3,4}
        GBeuler='(90°,90°,0°) [perp. to RD]';
    case{5,6}
        GBeuler='(0°,90°,0°) [perp. to TD]';
    case{7,8}
        GBeuler='(0°,0°,0°) [perp. to ND]';
    otherwise
        GBeuler='UNKNOWN';
end
%}
switch(model) 
    case{'ALAMEL'}
     title(char([file ' -- ' model ' (\Delta\gamma=' dg ')'],[gr1 '+' gr2 ],[' with GB ' GBeuler ]),'FontSize',12)
    case{'FCTaylor'}    
     title(char([file ' -- ' model ' (\Delta\gamma=' dg ')'],[gr1 '+' gr2 ],[ ]),'FontSize',12)
    otherwise
     title('UNAPPROPRIATE name given to ""model""','FontSize',12)
%    title([file ' ---  grain nr.' num2str(grain)],'FontSize',16)
end %switch (model)
clear gr1 gr2 GBeuler step;
%% Automatic save of figure
%
outName=[file 'Eul' num2str(grain)];
saveas(h,outName,'fig')
saveas(h,outName,'jpg')
clear outName
%
%%
hold off, 
close(h)
clear h grainCouple 