
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

# Dispatcher: pick the library suitable for 32- or 64-bit Python
_bits, _linkage = platform.architecture()


# TODO: get the path from VEF_ROOT


if _bits == '64bit':
    _lib_path = r'C:\Users\Jerzy\Documents\Work\TWRMTMProject\projects\branches\branch-token\slis\x64\Release-DLL'
else:
    _lib_path = r'C:\Users\Jerzy\Documents\Work\TWRMTMProject\projects\branches\branch-token\slis\Win32\Release-DLL'

lib = cdll.LoadLibrary(os.path.join(_lib_path, 'libslis.dll'))


# map: function name -> attributes 
meta = {'isTokenValid': {'argtypes': [c_char_p], 'restype': c_bool},
         'signFile': {'argtypes': [c_char_p, c_char_p, c_char_p], 'restype': c_int},
         'isSignatureValid': {'restype': c_bool, 'argtypes': [c_char_p, c_char_p, c_char_p]}
       }


