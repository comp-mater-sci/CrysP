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
from robusta.GenericRobusta import *
from robusta.config import *


class GenericMaterial(GenericRobusta):
        
    """
    An abstract base class which will contain all template material data.
    
    A material name and abaqus mdb.model object name need to be provided to the
    constructor. example:
    
    myNewMaterial = robusta.GenericMaterial(name='steel',modelName='Model-1')
    
    
    Data can be loaded from text files by calling the LoadData method.
    """
   
    def __init__(self, name, modelName, dimensionality):

        # call base class constructor
        GenericRobusta.__init__(self, modelName)

        # store material name and create the Abaqus material object
        self.SetDefault('materialName',name)
        
        modelObj = self.GetModelHandle()
        
        if name in modelObj.materials.keys():
            errorMessage = 'Material with the name {0} already in the database.'.format(name)
            self.ErrorHandling(errorMessage)
            
        modelObj.Material(name)
        
        # store some defaults
        self.SetDefault('sectionPrefix', 'HSection_({0})')
        self.SetValue('dimensionality', dimensionality)
        
        # set up the 'requiredMaterialParameters' list with parameters that are
        # common to all materials defined here (ie relevant to metal plasticity)
        self.SetValue('requiredMaterialParameters', ['density', 'youngsModulus',
                      'poissonRatio', 'materialName'])
                      
       
    def AddToRequiredMaterialParmList(self, newRequiredParmList):
        """ add the given list of parameter names to the list of required
            parameters for this material.
        """
        # add the new parameter names
        currentList = self.ListRequiredMaterialParameters()
        for newParm in newRequiredParmList:
            if not newParm in currentList:
                currentList.append(newParm)
                
        # store the updated list
        self.SetValue('requiredMaterialParameters', currentList)
        
        
    def ListRequiredMaterialParameters(self):
        """ return a list of the required material parameters for this material
            model
        """
        return self.GetValue('requiredMaterialParameters')

        
    def MakeIsotropicElastic(self):
        """
        Create the material and set up sections in the abaqus mdb and assign properties
        """
        # get the material data (assuming it has been loaded first!)
        materialObj = self.GetMaterialObj()
        materialName = self.GetDefault('materialName')
        
        rho = self.GetValue('density')
        E = self.GetValue('youngsModulus')
        nu = self.GetValue('poissonRatio')

        # set the density
        materialObj.Density(table=((rho, ), ))
        
        # set elastic data
        materialObj.Elastic(table=((E, nu), ))
        
        if verbose: print 'Added elastic details for {0}.'.format(materialName)
   

    def MakeHomogSection(self):
        """ create a homogenous solid section associated with the material
        
            the type of section is dependent on the dimensionality (2D or 3D)
        """
        # get aliases and naming data
        modelObj = self.GetModelHandle()
        materialName = self.GetDefault('materialName')
        sectionNamePrefix = self.GetDefault('sectionPrefix')
        sectionName = sectionNamePrefix.format(materialName)

        
        if sectionName in modelObj.sections.keys():
            errorMessage = 'Section with the name {0} already in the database.'.format(sectionName)
            self.ErrorHandling(errorMessage)
        
        is3D = self.GetValue('dimensionality') == '3D'
        
        if is3D:    
            modelObj.HomogeneousSolidSection(name=sectionName, material=materialName,
                     thickness=None)
        else:
            modelObj.HomogeneousSolidSection(name=sectionName, material=materialName,
                     thickness=defaultShellThickness)
            #modelObj.PEGSection(name=sectionName, material=materialName, 
            #         thickness=defaultShellThickness, wedgeAngle1=0.0, wedgeAngle2=0.0)
                                         
        if verbose: print 'Added homogeneous section for {0}.'.format(materialName)
        
        
    def GetMaterialObj(self):
        """ return an alias to this objects material object in the mdb
        """
        materialName = self.GetDefault('materialName')
        modelObj = self.GetModelHandle()
        
        if not materialName in modelObj.materials.keys():
            errorMessage = 'No material called <{0}> created yet.'.format(materialName)
            self.ErrorHandling(errorMessage)
        
        return self.GetModelHandle().materials[materialName]    
        
"""        
COPYRIGHT NOTICE
================
This file is part of robusta, copyright (c) KU Leuven 2013.

For license details see LICENSE.txt supplied with this package
"""
