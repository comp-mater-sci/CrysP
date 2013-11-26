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
from robusta.assemble.GenericAssembly import *
from robusta.config import *


class PlaneStrainComp(GenericAssembly):
    """ assemble a basic flat rolling simulation, given three part objects
        corresponding to top roll, bottom roll, and sheet
    """
    
    def __init__(self, name, modelName, tool, sample,
                 hammerName='hammer', anvilName='anvil', sampleName='sheet',
                 configuration='default'):
        """ Constructor
        """
        # call base class constructor
        GenericAssembly.__init__(self, name, modelName)
        
        # store the instance names
        self.SetValue('hammerName', hammerName)
        self.SetValue('anvilName', anvilName)
        self.SetValue('sampleName', sampleName)
                
        # determine if symmetry is being used
        symmetric = tool.GetValue('symmetric')
        self.SetValue('symmetric', symmetric)
        
        # determine and store the dimensionality
        dimensionality = sample.GetValue('dimensionality')
        self.SetValue('dimensionality', dimensionality)
        is3D = dimensionality == '3D'

        # create surfaces on the parts (which will be replicated in the assembly)
        TopSurfSampleSetName = geometrySetPrefix + posYSurfName
        TopSurfName = surfacePrefix + posYSurfName
        BotSurfSampleSetName = geometrySetPrefix + negYSurfName
        BotSurfName = surfacePrefix + negYSurfName

        sample.CreateSurfacesFromSet(setName=TopSurfSampleSetName, surfName=TopSurfName, side=2)
        sample.CreateSurfacesFromSet(setName=BotSurfSampleSetName, surfName=BotSurfName, side=2)
        
        # the surface on the tool needs to be specified slightly differently 
        # depending on whether it is 2D or 3D; also there is a weird behaviour
        # where if you create a surface on an analytic part with the correction
        # orientation (ie the surface is on the 'outside' rather than the 'inside')
        # the orientation gets reversed if the derived instance is flipped in the
        # assembly space.
        # unfortunately the solution seems to be that the surfaces for a 2D
        # part should be created on the instances rather than the parts

        # for 3D parts surfaces can be created on the part (for 2D the surface
        # is created on the instances, see below)
        if is3D:
            tool.CreateDefaultExternalSurface(name=DefaultExtSurfName, side=1)
            
        
        # store the surface names
        self.SetValue('hammerSurfName', DefaultExtSurfName)
        self.SetValue('anvilSurfName', DefaultExtSurfName)
        self.SetValue('sampleTopSurfName', TopSurfName)
        self.SetValue('sampleBotSurfName', BotSurfName)

        # instance the parts
        GenericAssembly.MakeInstance(self, partObj=tool, instanceName=hammerName)
        GenericAssembly.MakeInstance(self, partObj=tool, instanceName=anvilName)
        GenericAssembly.MakeInstance(self, partObj=sample, instanceName=sampleName)
        self.SetMovingToolInstanceName(instanceName=hammerName)
        
        # create the tool surfaces on the instances for 2D parts which are flipped
        # or rotated in the assembly space (flipping causes problems with surface
        # orientation)
        
        # note that this surface creation has to be done before the instances are
        # moved, due to the fact that the .findAt method seems to be the only
        # reliable way of creating surfaces (ie. if you move the instances first,
        # then you are faced with trying to calculate a point on each instance's
        # surface suitable for .findAt)
        if not is3D:            
            partObj = tool.GetPartObj()
            assemblyObj = self.GetAssemblyObj()
            externalPoint = tool.GetValue('extPoint')
            
            # the surface is named to seem like it came from a part surface, in
            # order to be consistent with the routines for defining interactions

            # hammer surface
            hammerSurfName = '{0}.{1}'.format(hammerName, DefaultExtSurfName)
            self.SetValue('hammerSurfName', hammerSurfName)
            
            edges = assemblyObj.instances[hammerName].edges.findAt(([externalPoint[0], externalPoint[1], 0.],))
            assemblyObj.Surface(side1Edges=edges, name=hammerSurfName)

            # anvil surface
            anvilSurfName = '{0}.{1}'.format(anvilName, DefaultExtSurfName)
            self.SetValue('anvilSurfName', anvilSurfName)
            
            edges = assemblyObj.instances[anvilName].edges.findAt(([externalPoint[0], externalPoint[1], 0.],))
            assemblyObj.Surface(side2Edges=edges, name=anvilSurfName)

        
        # Position the instances using the Abaqus CAE position constraints feature
        if configuration=='default':
        
            # make the hammer move above the sample
            GenericAssembly.BringDatumsTogether(self, moveableInstanceName=hammerName,
                            moveableDatumName='lowerXZ', fixedInstanceName=sampleName,
                            fixedDatumName='upperXZ')
                                     
            # move the anvil below the sample                                                  
            sheetThickness = abs(sample.GetValue('yMax') - sample.GetValue('yMin'))
            toolHeight = abs(tool.GetValue('yMax') - tool.GetValue('yMin'))
            GenericAssembly.TranslateAlongPrincipDir(self, instanceName=anvilName,
                            distance=-(sheetThickness+toolHeight)*2, direction='y')
                            
            # make sure all the RPs line up along the z/extrusion direction
            GenericAssembly.AlignInstanceByRP(self, moveInstanceName=anvilName,
                            refInstanceName=sampleName, direction='z')
            GenericAssembly.AlignInstanceByRP(self, moveInstanceName=hammerName,
                            refInstanceName=sampleName, direction='z')
                            
            # slightly move the tools to prevent contact problems at symmetry planes
            if not symmetric:
                GenericAssembly.TranslateAlongPrincipDir(self, instanceName=anvilName,
                                distance=-toolAlignmentAllowance, direction='x')
                GenericAssembly.TranslateAlongPrincipDir(self, instanceName=hammerName,
                                distance=-toolAlignmentAllowance, direction='x')
                            
            # rotate the anvil
            GenericAssembly.RotateInstanceAboutRP(self, instanceName=anvilName,
                                                  axis='X', angle=180.)
            
            GenericAssembly.BringDatumsTogether(self, moveableInstanceName=anvilName,
                            moveableDatumName='lowerXZ', fixedInstanceName=sampleName,
                            fixedDatumName='lowerXZ', flip=ON)
                            
            # if the tool is not symmetric, translate the sample
            if not symmetric:
                assemblyObj = self.GetAssemblyObj()
                sampleLength = sample.GetLength(direction='x')
                assemblyObj.translate(instanceList=(sampleName, ), vector=(sampleLength/2, 0.0, 0.0))
                            
            
        else:
            errorMessage = 'Dont know how to construct the configuration {0}.'.format(configuration)
            GenericAssembly.ErrorHandling(self, errorMessage)
        
        
    def DefaultContact(self, frictionCoeff):
        """ uses the method in GenericAssembly to identify which surfaces should
            be paired with which for contact in a two tool plane strain set up
            
            Note there is something strange about the abaqus object model here:
            even though surfaces appear in a common surfaces repository in the GUI,
            they are not found in rootAssembly.surfaces but actually in
            rootAssembly.instance[instanceName].surfaces !
        """
        
        assemblyObj = self.GetAssemblyObj()
        
        # interaction between hammer and sample
        hammerName = self.GetValue('hammerName')
        hammerSurfName = self.GetValue('hammerSurfName')
        masterSurf = self.FindSurface(hammerSurfName)
        
        sampleName = self.GetValue('sampleName')
        sampleTopSurfName = self.GetValue('sampleTopSurfName')
        slaveSurf = self.FindSurface(sampleTopSurfName)
        
        self.DefaultContactBySurfs(frictionCoeff=frictionCoeff,
                                   instMastSurf=masterSurf, instSlaveSurf=slaveSurf,
                                   name='hammer_sample')
        
        # interaction between bottom anvil and sample
        anvilName = self.GetValue('anvilName')
        anvilSurfName = self.GetValue('anvilSurfName')
        masterSurf = self.FindSurface(anvilSurfName)
        
        sampleBotSurfName = self.GetValue('sampleBotSurfName')
        slaveSurf = self.FindSurface(sampleBotSurfName)
        
        self.DefaultContactBySurfs(frictionCoeff=frictionCoeff,
                                   instMastSurf=masterSurf, instSlaveSurf=slaveSurf,
                                   name='anvil_sheet')
                                   
                                   
    def PSCFixedAnvil(self):
        """ Apply boundary conditions and constraints for plane strain compression
            with a fixed anvil
        """
        # get the instance names
        hammerName = self.GetValue('hammerName')
        sampleName = self.GetValue('sampleName')
        anvilName = self.GetValue('anvilName')
        
        # BCs for the tools
        self.ApplyBC(instanceName=anvilName, kind='encastre', label='fixed_anvil')
        self.ApplyBC(instanceName=hammerName, kind='ySlottedLocked', label='hammer_vert_only')
        
        # BCs for the sheet are slightly different for 2D and 3D
        is3D = self.GetValue('dimensionality') == '3D'
        if not self.GetValue('symmetric'):
            self.ApplyBC(instanceName=sampleName, kind='xsymm', label='symm_half_model_x', setName=geometrySetPrefix+negXSurfName)
            
            if is3D:    
                self.ApplyBC(instanceName=sampleName, kind='zsymm', label='symm_half_model_z', setName=geometrySetPrefix+negZSurfName)
            
            else:
                # this restraint is required for the 2D analysis because Abaqus by
                # default assumes plane stress rather than plane strain for 2D
                pass 
                #self.ApplyBC(instanceName=sampleName, kind='zsymm', label='plane_strain', setName=geometrySetPrefix+allFacesSetName)
