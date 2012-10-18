%% Data plotting of slip rates
% Assumed to be known:
%%% "grain" : nr. of the grain to be plotted
grainCouple=ceil(grain/2);

%% Defined legend of slip systems
switch(cryst)
    case{'FCC'}
        z{1} =texlabel(' (111){01-1}','literal');%z is CELL 
        z{2} =texlabel(' (111){-101}','literal');
        z{3} =texlabel(' (111){1-10}','literal');
        z{4} =texlabel('(-111){01-1}','literal');
        z{5} =texlabel('(-111){101} ','literal');
        z{6} =texlabel('(-111){110} ','literal');
        z{7} =texlabel('(1-11){011} ','literal');
        z{8} =texlabel('(1-11){-101}','literal');
        z{9} =texlabel('(1-11){110} ','literal');
        z{10}=texlabel('(11-1){011} ','literal');
        z{11}=texlabel('(11-1){101} ','literal');
        z{12}=texlabel('(11-1){1-10}','literal');
        z{13}='RLX_1';
        z{14}='RLX_2';
        leg=char(z{1},z{2},z{3},z{4},z{5},z{6},z{7},z{8},z{9},z{10},z{11},z{12},z{13},z{14}); %CELL->MATRIX
    case{'BCC'}
        z{1} =  '  (01-1)[111]  ';%z is CELL
        z{2} =  '  (-101)[111]  ';
        z{3} =  '  (1-10)[111]  ';
        z{4} =  ' (0-1-1)[-1-11]';
        z{5} =  '   (101)[-1-11]';
        z{6} =  '  (-110)[-1-11]';
        z{7} =  '  (01-1)[-111] ';
        z{8} =  '   (101)[-111] ';
        z{9} =  ' (-1-10)[-111] ';
        z{10} = ' (0-1-1)[1-11] ';
        z{11} = '  (-101)[1-11] ';
        z{12} = '   (110)[1-11] ';
        z{13} = ' (2-1-1)[111]  ';
        z{14} = ' (-12-1)[111]  ';
        z{15} = ' (-1-12)[111]  ';
        z{16} = ' (-21-1)[-1-11]';
        z{17} = ' (1-2-1)[-1-11]';
        z{18} = '   (112)[-1-11]';
        z{19} = '(-2-1-1)[-111] ';
        z{20} = '  (12-1)[-111] ';
        z{21} = '  (1-12)[-111] ';
        z{22} = '  (21-1)[1-11] ';
        z{23} = '(-1-2-1)[1-11] ';
        z{24} = '  (-112)[1-11] ';
 %       z{25}='RLX_1';
  %      z{26}='RLX_2';
        leg=char(z{1},z{2},z{3},z{4},z{5},z{6},z{7},z{8},z{9},z{10},z{11},z{12},z{13},z{14},z{15},z{16},z{17},z{18},z{19},z{20},z{21},z{22},z{23},z{24}); %CELL->MATRIX
    otherwise
        leg='UNKNOWN';
end
clear z;
%% Define LineSpec_LIST


LineSpec_LIST{1} ='-ok'; % '-or';%'r-';
LineSpec_LIST{2} ='-+k'; % '-sr';%'r:';
LineSpec_LIST{3} ='-xk'; % '-dr';%'r.';
LineSpec_LIST{4} ='-*k'; % '-^r';%'g-';
LineSpec_LIST{5} ='-^k'; % '-vr';%'g:';
LineSpec_LIST{6} ='-vk'; % '-xr';%'g.';
LineSpec_LIST{7} ='->k'; % '--og';%'b-';
LineSpec_LIST{8} ='-<k'; % '--sg';%'b:';
LineSpec_LIST{9} ='-sk'; % '--dg';%'b.';
LineSpec_LIST{10}='-dk'; % '--^g';%'k-';
LineSpec_LIST{11}='-pk'; % '--vg';%'k:';
LineSpec_LIST{12}='-hk'; % '--xg';%'k.';

LineSpec_LIST{13}='-og'; % ':ob';%'rv';
LineSpec_LIST{14}='-+g'; % ':sb';%'r^';
LineSpec_LIST{15}='-xg'; % ':db';%'rd';
LineSpec_LIST{16}='-*g'; % ':^b';%'gv';
LineSpec_LIST{17}='-^g'; % ':vb';%'g^';
LineSpec_LIST{18}='-vg'; % ':xb';%'gd';
LineSpec_LIST{19}='->g'; % '-.og';%'bv';
LineSpec_LIST{20}='-<g'; % '-.sg';%'b^';
LineSpec_LIST{21}='-sg'; % '-.dg';%'bd';
LineSpec_LIST{22}='-dg'; % '-.^g';%'kv';
LineSpec_LIST{23}='-pg'; % '-.vg';%'k^';
LineSpec_LIST{24}='-hg'; % '-.xk';%'kd';

LineSpec_LIST{25} ='k'; %horizontal line Y=0
%LineSpec_LIST_pseudo{1}='r-.';
%LineSpec_LIST_pseudo{2}='g-.';

%% PLOT all slip systems
h=figure;
hold on, 
set(gca,'YTick',YTickLIST)
set(gca,'XTick',XTickLIST)
set(gca,'FontSize',16)%,'FontWeight','bold')



%real s.s.
for ss=1:NRss %ADEM: Accumulated Delta_Epsilon_VonMises
    if (ss < 13) % ss 1 - 12
        kleur=[0. 0. 0.];%=black
    else % ss 13-24
        kleur=[0. 1. 0.];%=green
    end
    plot(ADEM,sliprate_2plot{ss,grain},LineSpec_LIST{ss},'MarkerSize',14,'LineWidth',1,'MarkerEdgeColor',kleur)
    %%%%%plot(ADEM,sliprate_NORM{ss,grain},LineSpec_LIST{ss},'MarkerSize',5,'LineWidth',2)
end
%pseudo-s.s.
%for ss=1:NRpseudoss %ADEM: Accumulated Delta_Epsilon_VonMises
%    plot(ADEM,gamRLX{ss,grainCouple},LineSpec_LIST_pseudo{ss},'MarkerSize',5,'LineWidth',1)
%end

plot(XrangePlot,[0. 0.],'k','LineWidth',0.8); %horizontal line Y=0

xlabel('\epsilon_v_M [/]','FontSize',18,'FontWeight','bold')
ylabel('slip rate [s^-^1]','FontSize',18,'FontWeight','bold')
%ylabel('norm. slip rate [s^-^1]','FontSize',16)
%legend(leg,'location','EastOutside')

clear leg LineSpec_LIST ss
%% LIMITS of plot
xlim(XrangePlot) 
ylim(YrangePlot) 
%% TITLE of plot

gr=['grain' num2str(grain) ': (\phi_1,\Phi,\phi_2)='];
step=1; %Here, we want INITIAL Euler angles
gr=[gr '(' num2str(Euler{1,grain}(step)) '°,' num2str(Euler{2,grain}(step)) '°,' num2str(Euler{3,grain}(step)) '°)' ];
%%%%%GBeuler=['(' num2str(GBEuler{1,grainCouple}(step)) '°,' num2str(GBEuler{2,grainCouple}(step)) '°,???°)'];
%{
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

title(char([ ],[ ],[ ]),'FontSize',18)%makes the plot 'wide' rather than 'high'

%title(char([file ' -- ' model ' (\Delta\gamma=' dg ')'],[gr],[ ]),'FontSize',12)

%{
switch(model) 
    case{'ALAMEL','MASAL1'}
     title(char([file ' -- ' model ' (\Delta\gamma=' dg ')'],[gr ' with GB ' GBeuler],[ ]),'FontSize',12)
    case{'FCTaylor'}    
     title([file ' -- ' model ' (\Delta\gamma=' dg ') -- ' gr],'FontSize',12)
    otherwise
     title('UNAPPROPRIATE name given to ""model""','FontSize',12)
%    title([file ' ---  grain nr.' num2str(grain)],'FontSize',16)
end %switch (model)
%}
clear gr GBeuler step;
%% Automatic save of figure
%
outName=[file '-gr' num2str(grain) ext];
saveas(h,outName,'fig')
saveas(h,outName,'tif')
clear outName
%}
%%
hold off, 
close(h)
clear h grainCouple


