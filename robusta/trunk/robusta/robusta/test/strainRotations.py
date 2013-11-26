""" script to calculate the rotation which would apparently bring the strain
    increment supplied by the HMS to the micromechanical model for the case
    of a single element simple shear simulation
"""

# the HmsArchive class represents the tar files the HMS produces
from robusta.post.HmsArchive import *
simOutput = HmsArchive('test')

# get the strain data
simOutput.ScanLocDir()
simOutput.GetStrainIncrementsForIP(1)

# for each calculate the
