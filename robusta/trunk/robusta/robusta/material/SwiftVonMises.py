# Prevent the legacy class type being used
__metaclass__ = type

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise
    
# import robusta modules
from robusta.material.GenericMaterial import *
from robusta.config import *

# import native modules
import warnings

class SwiftVonMises(GenericMaterial):
        
    """
    A class for defining a material in the abaqus mdb that uses isotropic elasticity and
    the built in Hill 1948 anisotropic plasticity model. The hardening is specified with
    swift law coefficients.
    """
   
    def __init__(self, name, modelName, dimensionality='3D'):

        GenericMaterial.__init__(self, name, modelName, dimensionality)

        # interpolation defaults
        self.SetDefault('maxTrueStrain',1.2)
        self.SetDefault('strainStepSize',0.01)
        self.SetDefault('maxNoPoints',300)
        
        # update the required parameters list
        self.AddToRequiredMaterialParmList(['Swift_n', 'Swift_K', 'Swift_eps0'])
        

    def MakeMaterialFromFile(self, fileName, folder):
        """
        Creates a material using parameters from a text file and defaults where necessary
        """
        # load paramaeters
        self.LoadParameters(fileName, folder)
        
        # make the material
        self.BuildMaterial()


    def BuildMaterial(self):
        """ make the material based on internally stored parameter values
        """
        # create material in the abaqus mdb and assign elasticity values
        self.MakeIsotropicElastic()

        # create a table of plastic stress/strain
        self.MakeSwiftCurveTable()
        
        # create an associated homogenous section
        self.MakeHomogSection()
        
        
    def MakeSwiftCurveTable(self):
        """
        add tabular data for plastic true stress/ true strain based on Swift coefficients
        """
        
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
        materialObj = self.GetMaterialObj()
        materialObj.Plastic(table=tableValues)
        
        if verbose: print 'Added plastic details for {0}'.format(self.GetDefault('materialName'))

        
"""        
COPYRIGHT NOTICE
================
This file is part of robusta, copyright (c) KU Leuven 2013.

For license details see LICENSE.txt supplied with this package
"""
