# $Id: __init__.py 2825 2017-02-15 10:42:54Z jgawad $

'''VEF alamDMC configurators'''

from configADP import configADP
from configASR import configASR
from configEWC import configEWC
from configQRS import configQRS
from configUDSA import configUDSA
from configYLD import configYLD

CONFIGURATORS = {
    'qrs': configQRS,
    'yld': configYLD,
    'udsa': configUDSA,
    'ewc': configEWC,
    'asr': configASR,
    'adp': configADP
    }

ALL_MODULES = CONFIGURATORS.keys()
