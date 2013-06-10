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

class Datafile(object):
    """Processing data files organizad into columns with a header line"""
    def __init__(self,fname=None,sep=None):
        self.headerlist = []
        self.datarows = []
        self.separator = sep
        self.comment = '#'
        if (fname):
            self.read(fname)
        return super(Datafile,self).__init__()

    def read(self,fname):
        import re
        try:
            with open(fname,'r') as inp:
                self.headerlist = []
                self.datarows = []
                comment_match = re.compile(('^'+self.comment))
                content = [] 
                for line in inp.readlines():
                    # drop all the comment lines
                    if not comment_match.match(line):
                        content.append(line)

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


class DataView(object):
    def __init__(self,datafile,viewspec):
        """Creates a view on selected columns of datafile
        
            \par[in] datafile -- Datafile object
            \par[in] viewspec -- specification of columns. It is an ordered sequence of tuples: ('column_name',column_type)
        """
        if not viewspec:
            raise ValueError('View specification cannot be empty')
        fieldnames = (k[0] for k in viewspec)
        if not (all(fieldname in datafile.headerlist for fieldname in fieldnames)):
            raise ValueError('Incorrect view specification: unknown field name')

        self.__ref_datafile = datafile
        self.viewspec = viewspec
    
    def __iter__(self):
        for row in self.__ref_datafile:
            result = ()
            for colname,coltype in self.viewspec:
                result += (coltype(row[colname]),)
            yield result



if __name__ == "__main__":
    data = Datafile('datafile.dat')

    print data

    for row in data:
        print row

    view = DataView(data,[('Tnorm',float),('q-value',float)])

    for point in view:
        print point

    tuples = sorted([t for t in DataView(data,[('Tnorm',float),('q-value',float)])])

    print tuples