echo off
rem batch file to install python packages in windows XP or later
rem
rem **** ADD YOUR CUSTOM PYTHON LOCATION HERE ***
set CUSTOM_PYTHON=C:\
rem *********************************************

rem add Python to the path environment variable
set path=%path%;C:\python24;C:\python25;C:\python26;C:\python27;C:\python28;C:\python33

python setup.py build
python setup.py install
pause
