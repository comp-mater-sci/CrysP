# $Id$

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
