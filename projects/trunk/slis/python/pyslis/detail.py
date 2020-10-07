
# $Id$
#
# Author: Jerzy Gawad
# Email:  Jerzy.Gawad@cs.kuleuven.be
# Organization: Katholieke Universiteit Leuven (KU Leuven)
# Organization unit: Dept.Comp.Sci., TWR Group
#
# Copyright by KU Leuven. All rights reserved.
#
# $Revision$
# $Date$
#

'''Implementation of SLIS wrapper for token API.'''

from ctypes import cdll, c_bool, c_int, c_char_p
import platform
import sys
import os

def setup_functions(module, lib, meta):
    '''Setup external function `fx_name` from DLL `lib` in `module`'''
    #
    # Function name is probed from the library via getattr
    # The attributes are set via setattr.
    #
    for fx_name in meta.keys():
        fx = getattr(lib, fx_name)
        fx_meta = meta[fx_name]
        fx.argtypes = fx_meta['argtypes']
        fx.restype = fx_meta['restype']
        setattr(module, fx_name, fx)

if 'win' in sys.platform
    # Dispatcher: pick the library suitable for 32- or 64-bit Python
    __bits, __linkage = platform.architecture()
    __arch_variant = 'x64' if __bits == '64bit' else 'Win32'
    __lib_path = os.path.join(os.environ['SLIS_ROOT'],
                            'lib',
                            __arch_variant,
                            'libslis.dll')
# Dries
elif 'linux' in sys.platform:
    __lib_path = os.path.join(os.environ['SLIS_ROOT'],'lib','libslis.so')
else:
    raise RuntimeError('Platform not recognized, cannot find dynamic slis library')


try:
    lib = cdll.LoadLibrary(__lib_path)
except:
    lib = cdll.LoadLibrary(__lib_path)

# map: function name -> attributes 
meta = {'isTokenValid': {'argtypes': [c_char_p], 'restype': c_bool},
         'signFile': {'argtypes': [c_char_p, c_char_p, c_char_p], 'restype': c_int},
         'isSignatureValid': {'restype': c_bool, 'argtypes': [c_char_p, c_char_p, c_char_p]}
       }


