
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

'''Python wrapper for SLIS'''

__author__ = 'Jerzy Gawad'
__copyright__ = 'KU Leuven'
__status__ = 'Prototype'

import sys

try:
    import detail
    import remote
except Exception as excpt:
    raise RuntimeError('Cannot initialize SLIS, reason: {}'.format(str(excpt)))


this_module = sys.modules[__name__]

detail.setup_functions(this_module, detail.lib, detail.meta)

acquireToken = remote.acquireToken

getTokenCount = remote.getTokenCount

