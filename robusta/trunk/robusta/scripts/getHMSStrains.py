""" example use of hms archive objects
"""

import sys

# check if the robusta package is on the system path
if not any([True for pathItem in sys.path \
             if ('robusta' in pathItem) and not ('scripts' in pathItem)]):
    sys.path.append('/home/diarmuid/LAPTOP/all/Code_and_Packages/Mine/packages/Copyright_KUL/robusta')

print sys.path
from robusta.post.HmsArchive import *


simulationArchive = HmsArchive(name='test', simRootFolder ='/home/diarmuid/SYNC/IWT_sims/ShearSims/single_el/3D/gamma_5/hms_Al_2mm_05/')
simulationArchive.ScanLocDir()
simulationArchive.GetStrainIncrementsForIP(IPnumber=1)
simulationArchive.WriteStrainIncrementForIP(IPnumber=1, fileName='strain_incs.txt')
