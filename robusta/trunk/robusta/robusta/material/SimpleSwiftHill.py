# Prevent the legacy class type being used
__metaclass__ = type

from abaqusConstants import *
from abaqus import *
from robusta.material.GenericMaterial import *

import os

class SimpleSwiftHill(GenericMaterial):
        
    """
    A class for defining a material in the abaqus mdb that uses isotropic elasticity and
    the built in Hill 1948 anisotropic plasticity model. The hardening is specified with
    swift law coefficients.
    """
   
    def __init__(self, name, modelName, dimensionality='3D'):

        # ideally this should be written with the "super" command for future compatibility, ie.:
        # super(CLASSNAME, self).__init__(self) #, instead Abaqus 6.9 requires the explicit version:
        GenericMaterial.__init__(self, name, modelName, dimensionality)

        # default is to name the material the same as the object instance
        self.SetValue('materialName', name)

        # default Swift law parameters
        self.SetDefault('Swift_K',250E+06)
        self.SetDefault('Swift_n',0.2)
        self.SetDefault('Swift_eps0',0.01)

        # interpolation defaults
        self.SetDefault('maxTrueStrain',1.2)
        self.SetDefault('strainStepSize',0.005)
        self.SetDefault('maxNoPoints',300)


    def MakeMaterialFromFile(self, fileName, folder):
        """
        Creates a material using parameters from a text file and defaults where necessary
        """
        # load paramaeters
        fullPath = os.path.join(folder, fileName)
        self.LoadParameters(fullPath)
        
        # create material in the abaqus mdb and assign elasticity values
        self.MakeIsotropicElastic()

        # create a table of plastic stress/strain
        self.MakeSwiftCurveTable()
        self.SetHill48YieldParameters()


    def MakeSwiftCurveTable(self):
        """
        add tabular data for plastic true stress/ true strain based on Swift coefficients
        """
        import warnings
        topLevelModelName = self.GetValue('TopLevelModelName')
        modelHandle = self.GetGlobal(topLevelModelName)
        newMaterialName = self.GetValue('materialName')

        # get Swift law parameters
        K= self.GetValue('Swift_K')
        n = self.GetValue('Swift_n')
        eps0 = self.GetValue('Swift_eps0')
        
        # get interpolation settings
        maxStrain = self.GetValue('maxTrueStrain')
        stepSize = self.GetValue('strainStepSize')

        # calculate values
        noOfPoints = int(round(maxStrain/stepSize))
        if noOfPoints > self.GetValue('maxNoPoints'):
            warnings.warn('The number of points for the plastic stress/strain table exceeded the maximum ('+str(self.GetValue('maxNoPoints'))+').'+
                          'Maybe increase stepSize or maxNoPoints?')
        tableValues = [( (K*((eps0 + (i*stepSize))**n)),(i*stepSize),) for i in range(noOfPoints)]
        
        # if the first calculated stress value is zero, remove it
        if not(tableValues[0][0]): null = tableValues.pop(0)

        # set values
        modelHandle.materials[newMaterialName].Plastic(table=tableValues)


    def SetHill48YieldParameters(self):
        """
        specify the hill model parameters using three r values
        """
        topLevelModelName = self.GetValue('TopLevelModelName')
        modelHandle = self.GetGlobal(topLevelModelName)
        newMaterialName = self.GetValue('materialName')

        r0 = self.GetValue('r0')
        r45 = self.GetValue('r45')
        r90 = self.GetValue('r90')

        # calculate the "stress ratios" - see abaqus theory manual, note that
        # the hill coefficients described there are not exactly the same as those
        # in the classic Hill expression
        R11 = 1
        R22 = sqrt((r90*(r0 + 1)) / (r0*(r90 + 1)))
        R33 = sqrt((r90*(r0 + 1)) / (r90 + r0))
        R12 = sqrt((3*r0*(r0 + 1)) / ((2*r45 + 1)*(r0 + r90)))

        # set values
        modelHandle.materials[newMaterialName].plastic.Potential(table=((R11, R22, R33, R12, R12, R12), ))
        
"""        
COPYRIGHT NOTICE
================
This file is part of robusta, copyright (c) KU Leuven 2013.

For license details see LICENSE.txt supplied with this package
"""
