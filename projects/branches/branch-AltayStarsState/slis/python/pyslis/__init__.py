
# $Id: __init__.py 3417 2020-10-07 15:04:29Z Dries.DeSamblanx $
#
# Author: Jerzy Gawad
# Email:  Jerzy.Gawad@cs.kuleuven.be
# Organization: Katholieke Universiteit Leuven (KU Leuven)
# Organization unit: Dept.Comp.Sci., TWR Group
#
# Copyright by KU Leuven. All rights reserved.
#
# $Revision: 3417 $
# $Date: 2020-10-07 17:04:29 +0200 (Wed, 07 Oct 2020) $
#

'''Python wrapper for SLIS'''

__author__ = 'Jerzy Gawad'
__copyright__ = 'KU Leuven'
__status__ = 'Prototype'

import sys

try:
    from . import detail
    from . import remote
except Exception as excpt:
    raise RuntimeError('Cannot initialize SLIS, reason: {}'.format(str(excpt)))


this_module = sys.modules[__name__]

detail.setup_functions(this_module, detail.lib, detail.meta)

acquireToken = remote.acquireToken

getTokenCount = remote.getTokenCount

