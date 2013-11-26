""" script to set up an example rolling simulation using the "robusta" package
"""
# import abaqus modules
from abaqusConstants import *
from abaqus import *

# import robusta classes
from robusta.material.SimpleSwiftVonMises import *

# ----  config  ----
modelName = 'rolling_sim'
materialDataFolder = '/home/diarmuid/LAPTOP/all/Code_and_Packages/Mine/packages/Copyright_KUL/robusta/robusta/test'

# --- end config ---


# Create a new model
mdb.Model(modelName)

# create materials and sections
sheet = SimpleSwiftVonMises('AA6016_1mm', modelName)
sheet.MakeMaterialFromFile(fileName='AA6016_1mm_0deg.txt',
                           folder=materialDataFolder)
                           


