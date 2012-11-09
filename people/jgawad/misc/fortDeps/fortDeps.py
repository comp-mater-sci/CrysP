#!/bin/env python

## 
# file 1->n  moduledef
# moduledef 1->1 file
# file 1->n moduleuse

import fileinput
import re



def findDependencies(flist,verbose=1):
    re_mod = re.compile('(^[ \t]*module[ \t]+)(\w+)[ \t]*$')
    re_use = re.compile('^([ \t]*use[ \t]+)(\w+)[ \t,;]*')
    fi = fileinput.FileInput(files=flist)
    mod2file= {}
    file_uses = {}
    
    for line in fi:
        match_mod = re_mod.match(line)
        if (match_mod):
            modname = match_mod.group(2).lower()
            if (verbose > 0): 
                print (fi.filename() + ' defines ' + modname + ' realname: ' + match_mod.group(2))
            mod2file[modname] = fi.filename()
        # 
        match_use = re_use.match(line)
        if (match_use):
            modname = match_use.group(2).lower()
            if (verbose > 0):
                print  (fi.filename() + ' uses ' + modname)
            # Initialize the value in the dictionary if needed:
            filename = fi.filename()
            if (not filename in file_uses):
                file_uses[filename] = set()
            file_uses[filename].add(modname)
            
    #print 'File2mod:', mod2file
    #print 'mod usage:', file_uses
    
    return (mod2file, file_uses)
    
def resolveDeps(mod2file,file2uses,verbose=1):
    # build dependencies per-file
    file_deps = {}.fromkeys(file2uses.keys(),set())
    for file,modlist in file2uses.items():
        for mod in modlist:
            try:
                file_deps[file].add(mod2file[mod])
            except KeyError:
                print ("External dependency is found: " + mod)
    return file_deps

def objDeps(source_deps,pattern,objext='.o'):
    obj_deps = {}
    # apply "pattern" on every source file, rename file with mapping
    for file,filedeps in source_deps.items():
        # rename key:
        
    
def renameDeps(file_deps,pattern):
    pass

def writeRule(file_deps,out):
    for ls,rs in file_deps.items():
        out.write( (ls + '\t:\t')+(' '.join(rs))+'\n' )
        
def main(argv):
    import glob
    import sys
    import argparse
    files = sys.argv[1:]
    version = 'makefortdeps ver. 0.0.1'
    try: 
        master_parser = argparse.ArgumentParser(description=version)
        master_parser.add_argument('--src2obj',help='')
        master_parser.add_argument('--mod_template',help='Template for naming module files from module names. If specified, the program will emit dependencies against module files.')
        master_parser.add_argument('--fpp',help='Command to invoke fortran pre-processor')
    except:
        print('Error in preparation of argument parsing.')
        return 1
    
if __name__ == '__main__':
    import sys
    try:
        lastpar = sys.argv.index('--')
        try:
            exitcode = main(sys.argv[lastpar+1:])
            sys.exit(exitcode)
        except:
            print('Uhnandled exception')
    except ValueError:
        print 'Error: separator of file list ("--") not found in the arguments.'
        sys.exit(1)

