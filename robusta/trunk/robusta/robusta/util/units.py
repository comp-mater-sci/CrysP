""" functions to convert between units and identify physical quantities 
"""
# import abaqus modules
try:
    from abaqus import *
    from abaqusConstants import *
except ImportError:
    print ('This module may require the abaqus modules to be available.')

            
def GetUnitName(abaqusQuantityNameConstant=None, version='SI'):
    """ return a unit name corresponding to the given abaqus constant
    
        if no constant is given, return the list of possible constants
        
        'version' can be 'SI' or 'mf' for metal forming
    """
    
    quantities = {ACCELERATION:['m/s^2','m/s^2',1.], ACOUSTIC_INTENSITY:['W:m^2','W:m^2',1.],
                  ANGLE:['rad','rad',1.], ANGULAR_MOMENTUM:['Nms','Nms',1.],
                  ARC_LENGTH:['m','m',1.], AREA:['m^2','m^2',1.],
                  AREA_VELOCITY_SQUARED:['?','?',1.], BIMOMENT:['?''?',1.],
                  CURVATURE:['?','?',1.], CORIOLIS_LOAD:['?','?',1.],
                  DAMAGE:['?','?',1.], DAMAGE_CRITERION:['?','?',1.],
                  DENSITY:['kg/m^3','kg/m^3',1.], DENSITY_ROTATIONAL_ACCELERATION:['?','?',1.],
                  DISPLACEMENT:['m','m',1.], ECURRENT_AREA_TIME:['?','?',1.],
                  ELECTRIC_CHARGE:['C','C',1.], ELECTRIC_CURRENT:['A','A',1.],
                  ELECTRIC_CURRENT_AREA:['?','?',1.], ELECTRIC_POTENTIAL:['V','V',1.],
                  ENERGY:['J','kJ',0.001], ENERGY_DENSITY:['?','?',1.],
                  ENERGY_RELEASE_RATE:['?','?',1.], EPOTENTIAL_GRADIENT:['?','?',1.],
                  FREQUENCY:['Hz','Hz',1.], FORCE:['N','kN',0.001],
                  FORCE_VOLUME:['?','?',1.], HEAT_FLUX:['W/m^2','W/m^2',1.],
                  HEAT_FLUX_AREA:['?','?',1.], HEAT_FLUX_RATE:['?','?',1.],
                  HEAT_FLUX_VOLUME:['?','?',1.], LENGTH:['m','m',1.],
                  LINEAR_PRESSURE:['?','?',1.], LUMIN:['?','?',1.], MASS:['kg','kg',1.],
                  MASS_FLOW_AREA:['?','?',1.], MASS_FLOW_AREA_RATE:['?','?',1.],
                  MASS_FLOW_RATE:['?','?',1.], MODE_NUMBER:['?','?',1.],
                  MOMENT:['?','?',1.], NUMBER:['?','?',1.], PATH:['?','?',1.],
                  PHASE:['?','?',1.], POSITION:['?','?',1.], PRESSURE:['Pa','MPa',0.000001],
                  PRESSURE_GRADIENT:['?','?',1.], RATE:['/s','/s',1.],
                  ROTARY_INERTIA:['?','?',1.], ROTATIONAL_ACCELERATION:['?','?',1.],
                  ROTATIONAL_VELOCITY:['rad/s','rad/s',1.], STATUS:['?','?',1.],
                  STRAIN:['','',1.], STRAIN_RATE:['/s','/s',1.],
                  STRESS:['Pa','MPa',0.000001], STRESS_INTENS_FACTOR:['?','?',1.],
                  SUBSTANCE:['mol','mol',1.], TEMPERATURE:['K','K',1.],
                  THICKNESS:['m','m',1.], TIME:['s','s',1.], TIME_INCREMENT:['s','s',1.],
                  TIME_HEAT_FLUX:['?','?',1.], TIME_HEAT_FLUX_AREA:['?','?',1.],
                  TIME_VOLUME:['?','?',1.], TIME_VOLUME_FLUX:['?','?',1.], TWIST:['?','?',1.],
                  VELOCITY:['m/s','m/s',1.], VELOCITY_SQUARED:['?','?',1.],
                  VOLUME:['m^3','m^3',1.], VOLUME_FLUX:['?','?',1.],
                  VOLUME_FLUX_AREA:['?','?',1.], VOLUME_FRACTION:['?','?',1.]}
    
    if not abaqusQuantityNameConstant in quantities.keys():
        raise KeyError('no quantity with abaqus constant <{0}>'.format(abaqusQuantityNameConstant))
    
    if abaqusQuantityNameConstant is None:
        # return the list of all possible quantities defined in abaqus
        return quantities.keys()
        
    else:
        if version=='SI':
            # just return the SI unit name
            return quantities[abaqusQuantityNameConstant][0]
            
        elif version=='mf':
            # return the units used for metal forming and the multiplier
            return (quantities[abaqusQuantityNameConstant][1],
                     quantities[abaqusQuantityNameConstant][2])
        
        else:
            raise ValueError('version should be SI or mf')
