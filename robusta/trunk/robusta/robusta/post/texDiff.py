""" script to act as a function which calls VERSCH from the MTM-FHM package
    to calculate the difference between two textures.
    
    The main purpose at was to use this function as a residual calculator in
    the minimisation of the difference between two textures by 'rotating' one
    of the textures.
"""

import subprocess.check_call as CheckCall
import os
import sys

def TextureDifference(à