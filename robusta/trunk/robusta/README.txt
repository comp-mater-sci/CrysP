=======
Robusta
=======
Problems, bugs, etc to diarmuid.shore@mtm.kuleuven.be?subject=shearpost


INSTALLATION
============

General Notes
-------------

You need to install Python 2.x first. 2.7 is still the most commonly used version
of Python out there, and is recommended for this package. Running with version 3
or later hasn't been tested.

Python is free platform independent software and can be downloaded at:
<http://www.python.org/getit/>

You will also need internet access during the installation so that any missing
python libraries can be installed automatically.

If you are modifying the source files and want to run the tests provided in /test
you may need to install PyLint, otherwise everything in there uses only unittest.

Windows (XP onward)
-------------------
You may need administrator privleges to install...

* Install Python (version 2.7 recommended)
* unzip pyfhm from pyfhm.zip to a temporary location
* run winInstall.bat

Possible Problems:
(1)**'python' is not recognized as an internal or external command, operable program
    or batch file.**
    
    probable cause: installation batch file will fail if Python can't be found in the
    usual location (namely C:\PythonXY, where X and Y are the major and minor version
    numbers). You can edit the winInstall.bat batch file accordingly to add your
    custom Python installation location

(2) TBA

Linux
-----
You may need root privleges to install, as python tends to install packages in
the /usr/lib/pythonxx and /usr/local/lib/python directories...

* unzip pyfhm from pyfhm.zip to a temporary location
* open a terminal window, navigate to the above folder
* ./linuxInstall.sh

If the install script won't run, make sure that it still has execute permissions
after unzipping and that the line endings haven't been converted to the windows
convention (you can run dos2unix on the script to be sure).


Mac OSX
-------
Instructions should be as for Linux systems. This text might change soon...


COPYRIGHT NOTICE
================
"""        
COPYRIGHT NOTICE
================
This file is part of robusta, copyright (c) KU Leuven 2013.

For license details see LICENSE.txt supplied with this package
