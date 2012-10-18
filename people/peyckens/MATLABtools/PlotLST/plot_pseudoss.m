%% Data plotting of pseudo ss of (a pair of) 2 grains
% Assumed to be known:
%%% "grain" : nr. of the 1st of 2 grains to be plotted
%%%            So grain is an UNeven number
grainCouple=ceil(grain/2);

%% Define legend
z{1}='RLX-1'; %'Pseudo-SS 1';
z{2}='RLX-2'; %'Pseudo-SS 2';
legRLX=char(z{1},z{2}); %CELL->MATRIX

clear z;
%% Define LineSpec_LIST
LineSpec_LIST_pseudo{1}='-ok';%'-sr';
LineSpec_LIST_pseudo{2}='-og';%'-dg';

%%
h=figure;
hold on, 
set(gca,'YTick',YTickLIST)
set(gca,'XTick',XTickLIST)
set(gca,'FontSize',16)%,'FontWeight','bold')


%pseudo-s.s.
for ss=1:NRpseudoss
    if (ss == 1) 
        kleur=[0. 0. 0.];%=black
    else % ss 2
        kleur=[0. 1. 0.];%=green
    end    
    plot(ADEM,gamRLX{ss,grainCouple},LineSpec_LIST_pseudo{ss},'MarkerSize',6,'LineWidth',2.,'MarkerEdgeColor',kleur,'MarkerFaceColor',kleur)
end

xlabel('\epsilon_v_M [/]','FontSize',18,'FontWeight','bold')
ylabel('slip rate [s^-^1]','FontSize',18,'FontWeight','bold')

%xlabel('inc. number [/]','FontSize',16)
%ylabel('norm. slip rate [s^-^1]','FontSize',16)

lijn0= plot(XrangePlot,[0. 0.],'k','LineWidth',0.8); %horizontal line Y=0


%% LIMITS of plot
xlim(XrangePlot) 
ylim(YrangePlot) 
%% LEGEND / TITLE
legend(legRLX,'location','North')%EastOutside')
title(char([ ],[ ],[ ]),'FontSize',18)%makes the plot 'wide' rather than 'high'
%title(char([file],[model],[ ]),'FontSize',12)
hold off, 
%}
%% Automatic save of figure
%
outName=[file '-gr' num2str(grain) '-RLX'];
saveas(h,outName,'fig')
saveas(h,outName,'tif')
clear outName

%%
hold off, 
close(h)
clear h grainCouple 