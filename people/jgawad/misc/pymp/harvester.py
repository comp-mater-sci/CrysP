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

import os

class Harvester(object):
    """Base abstract class for harvesters: entities that can extract data"""
    def __init__(self):
        pass

    def harvest():
        """Returns a dictionary of items: {data_name: data}"""
        return {}

class TextFileHarvester(Harvester):
    """Harvester for plain text files"""
    def __init__(self,data_fname):
        self.data_fname = data_fname
        return super(TextFileHarvester, self).__init__()

    def harvest(self,workdir='.'):
        """Returns a dictionary of items: {data_filename: data}"""
        output = None
        if self.data_fname:
            try:
                data_path = os.path.expanduser(os.path.join(workdir,self.data_fname))
                inp = open(data_path,'r')
                res = inp.readlines()
                inp.close()
                if len(res):
                    output = res
            except:
                print 'Cannot harvest the results'    
        return {self.data_fname: output}

if __name__ == "__main__":
    harvester = TextFileHarvester('grid2.grd')

    result = harvester.harvest()
    print result