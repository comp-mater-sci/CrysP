# $Id$

'''VEF alamDMC configurators'''

from configASR import configASR
from configEWC import configEWC
from configQRS import configQRS
from configUDSA import configUDSA
from configYLD import configYLD

ALL_MODULES = ['qrs', 'yld', 'udsa', 'asr', 'ewc']

CONFIGURATORS = {
    'qrs': configQRS,
    'yld': configYLD,
    'udsa': configUDSA,
    'ewc': configEWC,
    'asr': configASR
    }

