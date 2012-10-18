%% Define LineSpec_LIST


LineSpec_LIST{1} ='-ok'; % '-or';%'r-';
LineSpec_LIST{2} ='-+k'; % '-sr';%'r:';
LineSpec_LIST{3} ='-xk'; % '-dr';%'r.';

%%
h=figure; hold on

for iii=1:length(oscLIST)
    oscilLOG=oscLIST{iii};
    plot(0:length(oscilLOG),[NaN('single') oscilLOG],LineSpec_LIST{iii},'MarkerSize',12);
end

legend(['FCtaylor';'ALAMEL  ';'MAS-ALv2'],'location','EastOutside')

xlabel('grain number','FontSize',16)
ylabel('measure of oscilatory behavior','FontSize',16)
%ylim([-22 -2])
%title([file ' -- ' model ' (\Delta\gamma=' dg ')']);
hold off
% Automatic save of figure
%
outName=[file '_osc'];
saveas(h,outName,'fig')
saveas(h,outName,'jpg')
close all
clear outName h