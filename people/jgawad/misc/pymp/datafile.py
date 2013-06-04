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

class datafile(object):
    """Processing data files organizad into columns with a header line"""
    def __init__(self,fname=None,sep=None):
        self.headerlist = []
        self.datarows = []
        self.separator = sep
        if (fname):
            self.read(fname)

    def read(self,fname):
        try:
            with open(fname,'r') as inp:
                self.headerlist = []
                self.datarows = []
                content = inp.readlines()
                if len(content):
                    # interpret the first row as column names
                    self.headerlist = content[0].strip().split(self.separator)
                    # The data rows are stored as lists.
                    self.datarows = [row.strip().split(self.separator) for row in content[1:]]
        except:
            pass

    def __iter__(self):
        """Iterates over the rows in data file. Returns a dictionary of pairs {column_name: row_value}"""
        for row in self.datarows:
            yield dict(zip(self.headerlist,row))


if __name__ == "__main__":
    data = datafile('yldpoints.dat')

    print data

    for row in data:
        print row 

