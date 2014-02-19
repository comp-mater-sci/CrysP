""" Configuration file for robusta
"""
# import native modules
import os

# import third party modules
abaqusImported = False
try:
    from abaqus import *
    from abaqusConstants import *
except ImportError:
    print 'The abaqus modules must be available to configure robusta fully.'
else:
    abaqusImported = True
    
try:
    import numpy as np
except ImportError:
    print ('The following modules appear to be missing or installed in a non' +
            ' standard location:')
    print ('Numpy : see http://sourceforge.net/projects/numpy/files/')
    raise

# ---- Global [General] ----
unitsLength = 'mm'                      # usually mm
defaultIterationLimit = 100             # an number of iterations limit for miscellanous algorithms


# ---- Default File Names & Locations [General] ----
#defaultMaterialDataFolder = '/home/diarmuid/SYNC/LAPTOP_BACKUPS/Backup-laptop my docs-2013-10-25_13-00/home/diarmuid/LAPTOP/all/Code_and_Packages/Mine/packages/Copyright_KUL/robusta/robusta/test'
root = os.path.join('C:\\','Users','Administrator','Documents','all','Code_and_Packages','working_copies','robusta')
#root = '/home/diarmuid/LAPTOP'
#root = '/home/diarmuid/WORKING_COPIES/robusta'
defaultMaterialDataFolder = os.path.join(root, 'robusta','data')
defaultMaterialFile = 'AA6016_1mm_0deg.txt'

# ---- Geometry [ABAQUS]----
sketchPadDims = 200.
defaultPartSizeX = 1000.                # length, rolling direction
defaultPartSizeY = 100.                 # height, sheet normal direction
defaultPartSizeZ = 1.                   # width, tangential direction
findAtOffsetPercent = 5.                # percent of sketch object length to offset when using the findAt method


# ---- Default material properties -----
defaultPointMass = 10000.                # usually for rigid tools with > 0 degress of freedom

# ---- Assembly [ABAQUS]----
airGap = 1e-8                           # gap to attempt between instances in the assembly


# ---- Contact [ABAQUS]----
DefaultFrictionCoeff = 0.2              # default friction coeff. for penalty contact
toolAlignmentAllowance = 0.25           # how much to offset the tool positions in the assembly to prevent contact problems


# --- UMAT/VUMAT [ABAQUS]----
vumatFilePath = None

# ---- Output [ABAQUS] ----
defaultFieldVars = ('S', 'LE', 'U', 'RF', 'CSTRESS', 'SDV', 'PEEQ','ER','A','V')
defaultFieldName = 'Def_field_vars'
defaultHistoryName = 'Def_history_vars'
defaultHistoryVars = ('ALLAE', 'ALLCD', 'ALLDC', 'ALLDMD', 'ALLFD', 'ALLIE',
                      'ALLKE', 'ALLPD', 'ALLSE', 'ALLVD', 'ALLWK', 'ALLCW',
                      'ALLMW', 'ALLPW', 'ETOTAL')
defaultContactVars = ('CFNM', 'CFN1', 'CFN2', 'CFN3', 'CFSM', 'CFS1', 'CFS2',
                      'CFS3', 'CFTM', 'CFT1', 'CFT2', 'CFT3', 'CAREA', 'XN',
                      'XS', 'XT')                      

# ---- Meshing [ABAQUS] ----
defaultElementSize = 1.0
defaultMinElementSizeFactor = 0.1
defaultElementDeviationFactor = 0.1

if abaqusImported:
    defaultConstraint = FINER               # controls how to choose an element size if there is ambiguity
    defaultRigidElement = R3D4              #
    defaultDeformable3DElement = C3D8R      #
    defaultDeformable2DElement = CPE4R      # CPE4R is plane strain element, CPS4R is plane stress element
    defaultMeshAlgorithm = MEDIAL_AXIS      # choice of algorithm is limited by element choice
    defaultTechnique = SWEEP                # choice of technique is limited by algorithm


# ---- Simulation Run [ABAQUS] ----
defaultDuration = 1.0
defaultMassScaling = 1.0
simulationType = 'explicit'
defaultNoODBFrames = 50

# ---- Numerical [General] ----
defaultPrecision = np.float64           # float precision can be 16,32 or 64 bit
DefaultHDFArrayshape = (2,2)            # default is a 2D array
DefaultIntegerType = np.int64           # 
defaultNoIPThickness = 1                # should be an odd int
defaultShellThickness = 1.0

toleranceCosineZero = 1e-5              # the smallest value for a cosine not to be considered as zero

# ---- Numerical [ABAQUS] ----
if abaqusImported:
    explicitPrecision = SINGLE
    nodalOutputPrecision = SINGLE
    
# ---- Numerical [HMS] ----

# ---- Numerical [HDF5] ----
maxArrayShape = (None, None)
hdfCompressionMethod = 'gzip'           # gzip is built in, LZW and SZIP can be installed
    
# ---- Contact ----
if abaqusImported:
    defaultAllowContactSeparation = OFF
    defaultOverClosureBehaviour = LINEAR
    defaultContactStiffness = 1e10
    

# ---- Plotting [ABAQUS] ----
printResolutionDPI = 600    
useXYPlotSymbols = True
globalFontName = 'verdana'
globalFontWeight = 'medium'
viewPortWidth = 300.                    # note view port sizes affect the size
viewPortHeight = viewPortWidth * 0.625  # of exported plots 
if abaqusImported:
    defaultDisplayResolution = EXTRA_FINE


# ---- Plotting [MATPLOTLIB] ----
matplotDPI = 300
matplotLineWidth = 2.0
matplotColourList = ['b','g','r','c','m','k']
matplotMarkerList = ['D','s','o','h','*','p']
matplotAntiAliasing = False
matplotMarkerSize = 8.0

# ---- labelling [ROBUSTA] ----
pickleFileExtension = '.pik'

# ---- Labelling [HMS] ----
defaultLocdirFolderName = 'locdir'
defaultArchivePrefix = 'snap'
defaultArchiveExtension = 'tgz'
hmsIPDataFilePrefix = 'elem_'
hmsStrainDataFileName = 'defdata.dat'

# ---- Labelling [ABAQUS] ----
geometrySetPrefix = 'GeoSet_'           # prefix for naming geometry based sets
nodeSetPrefix = 'NdSet_'                # prefix for naming node sets
elementSetPrefix = 'ElSet_'             # prefix for naming element sets
surfacePrefix = 'Surf_'                 # prefix for surfaces (for interactions)
posXSurfName = 'extPosX'
negXSurfName = 'extNegX'
posYSurfName = 'extPosY'
negYSurfName = 'extNegY'
posZSurfName = 'extPosZ'
negZSurfName = 'extNegZ'
DefaultIntPropName = 'Default_Int_prop' # default interaction property name
DefaultExtSurfName = 'Default_Ext_Surf' # default name for external surfaces
defaultAmpName = 'default_amp'          # name for default amplitude object
defaultStepName = 'default_step'        # default name for deformation steps
allFacesSetName = 'all_faces'

caeFileExtension = '.cae'
odbFileExtension = '.odb'

# ---- Labelling [HDF5] ----
hdfFileExtension = '.hdf'

# ---- debugging flags ----
verbose = True                          # if True, print out debugging info
suppressErrors = False                  # if True, don't raise exceptions


# ---- exporting data ----
defaultStrainVariableName = 'LE'


# ---- version info [ABAQUS] ----
if abaqusImported:
    abaqusMajVer = int(version.split('.')[0])
    abaqusMinVer = int(version.split('.')[1].split('-')[0])
    abaqusMicVer = int(version.split('.')[1].split('-')[1])
    
numpyVer = np.version.version




if verbose: print 'Being verbose.'
"""        
COPYRIGHT NOTICE
================
This file is part of robusta, copyright (c) KU Leuven 2013.

For license details see LICENSE.txt supplied with this package
"""
