%% get Grain Boundary data
%Assumed to be correct and known:
%  line_Start
%  line_End

t_cur= t2(line_Start_forGB:line_End_forGB, :);

%% Get GBnormal{1:3,grainCouple}(step)
line=t_cur{1,:};
GBnormal{1,grainCouple}(step)=str2num(line(end-29:end-20));%
GBnormal{2,grainCouple}(step)=str2num(line(end-19:end-10));%
GBnormal{3,grainCouple}(step)=str2num(line(end-9 :end   ));%

%n temporary variable
n(1)=GBnormal{1,grainCouple}(step);
n(2)=GBnormal{2,grainCouple}(step);
n(3)=GBnormal{3,grainCouple}(step);

% PHI in degrees; cos(PHI) = n_3
PHId= acosd(n(3)); %PHI in [0 180.]
% phi_1 in degrees; sin(phi_1)*sin(PHI) = n_1
if PHId<1e-4
    phi1d=0.;
else
    phi1d= asind( n(1) / sind(PHId) ); %phi1d in [-90. 90.]
end

%assign
GBEuler{1,grainCouple}(step)=phi1d;
GBEuler{2,grainCouple}(step)=PHId;

clear n phi1d PHId;
%% Get gamRLX{1:2,grainCouple}(step)
line=t_cur{2,:};
gamRLX{1,grainCouple}(step)=str2num(line(end-23:end-12))* DvM(step);%in ALTAY, it is normalized by DvM
gamRLX{2,grainCouple}(step)=str2num(line(end-11:end   ))* DvM(step);%in ALTAY, it is normalized by DvM

clear t_cur line;
