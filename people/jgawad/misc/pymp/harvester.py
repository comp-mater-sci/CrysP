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
import sys
import shutil

class Harvester(object):
    """Base abstract class for harvesters: entities that can extract data"""
    def __init__(self):
        pass

    def harvest(self):
        """Returns a dictionary of items: {data_name: data}"""
        return {}


class FileHarvester(Harvester):
    def __init__(self,data_fname):
        self.data_fname = data_fname
        return super(FileHarvester, self).__init__()

    def processFile(self,file_path):
        return None

    def harvest(self,workdir='.'):
        """Returns a dictionary of items: {data_filename: data}"""
        output = None
        if self.data_fname:
            try:
                data_path = os.path.expanduser(os.path.join(workdir,self.data_fname))
                output = self.processFile(data_path)
            except:
                print 'Cannot harvest the results'    
        return {self.data_fname: output}




class TextFileHarvester(FileHarvester):
    """Harvester for plain text files"""

    def __init__(self,data_fname):
        return super(TextFileHarvester, self).__init__(data_fname)

    def processFile(self,file_path):
        inp = open(file_path,'r')
        res = inp.readlines()
        inp.close()
        if len(res):
            output = res
        else:
            output = None
        return output


class DataFileHarvester(FileHarvester):

    def __init__(self,data_fname):
        return super(DataFileHarvester, self).__init__(data_fname)

    def processFile(self,file_path):
        import datafile
        return datafile.Datafile(file_path)

        


class FileCopyHarvester(Harvester):
    """Harvester for copying/moving files"""
    def __init__(self,file_list = [],target_dir_path = '.',source_dir_path='.'):
        self.file_list = file_list
        self.target_dir_path = target_dir_path
        self.source_dir_path = source_dir_path
        if not (os.path.exists(target_dir_path) and os.path.isdir(target_dir_path)): 
            os.mkdir(target_dir_path)
        return super(FileCopyHarvester, self).__init__()

    def harvest(self):
        """Make copy of files included in the list from src_dir_path to target_dir_path"""
        output = {}
        for file in self.file_list:
            try:
                output[file] = False
                if os.path.isabs(file):
                    file_path = file
                else:
                    file_path = os.path.join(self.source_dir_path,file)
                shutil.copy(file_path,self.target_dir_path)
                output[file] = True
            except:
                pass
            return output

if __name__ == "__main__":
    try:
        harvester = TextFileHarvester('grid2.grd')
        result = harvester.harvest()
        print result
        #
        aFileCopyHarvester = FileCopyHarvester(['grid2.grd'],'output_dir')
        result = aFileCopyHarvester.harvest()
        print result
    except Exception as e:
        print('An exception has been raised')
        print(e)