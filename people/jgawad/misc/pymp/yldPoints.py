#!/usr/bin/env python
#
# $Id$
#
# Author: Jerzy Gawad
# Email:  Jerzy.Gawad@cs.kuleuven.be
# Organization: Katholieke Universiteit Leuven (KU Levuen)
# Organization unit: Dept.Comp.Sci., TWR Group
#
# Copyright by KU Leuven. All rights reserved.
#
# $Revision$
# $Date$
#
segments = [(11,+1.,+1.), 
            (13,+1.,+1.), 
            (13,+1.,-1.), 
            (11,-1.,-1.), 
            (11,-1.,+1.),
            (13,-1.,+1.),
            (13,-1.,-1.),
            (11,+1.,-1.)]

fieldnames = ['point', 'idmodl', 'deps3dt', 'rho','workinc']

import numpy

def makeSamplingPoints(deps3dt,workinc,rho_start=0.0,rho_end=1.0,nrhos=11):
    result = []
    point = 1
    for segment in segments:
        for rho in numpy.linspace(rho_start,rho_end,nrhos):
            fieldvalues = [point, segment[0], deps3dt*segment[1], rho*segment[2], workinc]
            result.append(dict(zip(fieldnames,fieldvalues)))
            point += 1
    return result


if __name__ == "__main__":
    data = makeSamplingPoints(0.001,0.004)