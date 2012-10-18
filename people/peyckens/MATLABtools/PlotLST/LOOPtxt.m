%% ALAMEL
GrainSS=grain1rA; grain=47; file='r9ABCC3';  model='ALAMEL - plane strain deformation with non-special GB orientation'; ReadTXT;
GrainSS=grain2rA; grain=48; file='r9ABCC3';  model='ALAMEL - plane strain deformation with non-special GB orientation'; ReadTXT;

GrainSS=grain1tA; grain=35; file='t0ABCC3';  model='ALAMEL - wire drawing deformation with non-special GB orientation'; ReadTXT;
GrainSS=grain2tA; grain=36; file='t0ABCC3';  model='ALAMEL - wire drawing deformation with non-special GB orientation'; ReadTXT;

GrainSS=grain1sA; grain=1;  file='vbABCCrd'; model='ALAMEL - simple shear deformation with GB aligned with shear plane'; ReadTXT;
GrainSS=grain2sA; grain=2;  file='vbABCCrd'; model='ALAMEL - simple shear deformation with GB aligned with shear plane'; ReadTXT;
%% MAS-Al(1.0001)<1:'friction'>
GrainSS=grain1rF; grain=47; file='r9FBCC3';  model='MAS-Al(1.0001)<1:ABS> - plane strain deformation with non-special GB orientation'; ReadTXT;
GrainSS=grain2rF; grain=48; file='r9FBCC3';  model='MAS-Al(1.0001)<1:ABS> - plane strain deformation with non-special GB orientation'; ReadTXT;

GrainSS=grain1tF; grain=35; file='t0FBCC3';  model='MAS-Al(1.0001)<1:ABS> - wire drawing deformation with non-special GB orientation'; ReadTXT;
GrainSS=grain2tF; grain=36; file='t0FBCC3';  model='MAS-Al(1.0001)<1:ABS> - wire drawing deformation with non-special GB orientation'; ReadTXT;

GrainSS=grain1sF; grain=1;  file='vbFBCCrd'; model='MAS-Al(1.0001)<1:ABS> - simple shear deformation with GB aligned with shear plane'; ReadTXT;
GrainSS=grain2sF; grain=2;  file='vbFBCCrd'; model='MAS-Al(1.0001)<1:ABS> - simple shear deformation with GB aligned with shear plane'; ReadTXT;
%% MAS-Al((coeff= 1.0001))<2:noIt> 
GrainSS=grain1rM; grain=47; file='r9MBCC3';  model='MAS-Al(1.0001)<2:noIt> - plane strain deformation with non-special GB orientation'; ReadTXT;
GrainSS=grain2rM; grain=48; file='r9MBCC3';  model='MAS-Al(1.0001)<2:noIt> - plane strain deformation with non-special GB orientation'; ReadTXT;

GrainSS=grain1tM; grain=35; file='t0MBCC3';  model='MAS-Al(1.0001)<2:noIt> - wire drawing deformation with non-special GB orientation'; ReadTXT;
GrainSS=grain2tM; grain=36; file='t0MBCC3';  model='MAS-Al(1.0001)<2:noIt> - wire drawing deformation with non-special GB orientation'; ReadTXT;

GrainSS=grain1sM; grain=1;  file='vbMBCCrd'; model='MAS-Al(1.0001)<2:noIt> - simple shear deformation with GB aligned with shear plane'; ReadTXT;
GrainSS=grain2sM; grain=2;  file='vbMBCCrd'; model='MAS-Al(1.0001)<2:noIt> - simple shear deformation with GB aligned with shear plane'; ReadTXT;

%% MAS-Al (coeff= exactly 1)<2:noIt> 
GrainSS=grain1rM1; grain=47; file='r9M1BCC3';  model='MAS-Al(coeff=1)<2:noIt> - plane strain deformation with non-special GB orientation'; ReadTXT;
GrainSS=grain2rM1; grain=48; file='r9M1BCC3';  model='MAS-Al(coeff=1)<2:noIt> - plane strain deformation with non-special GB orientation'; ReadTXT;

GrainSS=grain1tM1; grain=35; file='t0M1BCC3';  model='MAS-Al(coeff=1)<2:noIt> - wire drawing deformation with non-special GB orientation'; ReadTXT;
GrainSS=grain2tM1; grain=36; file='t0M1BCC3';  model='MAS-Al(coeff=1)<2:noIt> - wire drawing deformation with non-special GB orientation'; ReadTXT;

GrainSS=grain1sM1; grain=1;  file='vbM1BCCrd'; model='MAS-Al(coeff=1)<2:noIt> - simple shear deformation with GB aligned with shear plane'; ReadTXT;
GrainSS=grain2sM1; grain=2;  file='vbM1BCCrd'; model='MAS-Al(coeff=1)<2:noIt> - simple shear deformation with GB aligned with shear plane'; ReadTXT;



