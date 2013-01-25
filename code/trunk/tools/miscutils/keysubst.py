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

def substituteBracketKeys(content,mapping,lbracket='<',rbracket='>'):
    """
    Performs substitution of keywords in the template content. Mapping of keys in form of <keyname> is used
    """
    filtered = []
    for line in content:
        for mpkey,mpval in mapping.items():
            line = line.replace(lbracket+mpkey+rbracket,str(mpval))
        filtered.append(line)
    return filtered


def substituteVarKeys(content, mapping):
    """
    Performs substitution of keywords in the template content. 
    Mapping of keys in form of either $keyname or ${keyname} is used.
    """
    import string
    filtered = []
    for line in content:
        template = string.Template(line)
        filtered.append(template.safe_substitute(mapping))
    return filtered


def substitute(content, mapping,method='brackets'):
    """Performs substitution of keywords according to selected method.
    """
    if (method == 'brackets'):
        result = substituteBracketKeys(content,mapping)
    elif(method == 'vars'):
        result = substituteVarKeys(content,mapping)
    else:
        result = []
    return result


def substFromTemplate(argv):
    """Legacy interface function of the keysubst package.
    Args:
        argv[0]: input (template file)
        argv[1]: output (output file)
        argv[2:]: key-value pairs
    Returns:
        None
    """
    try:
        inputlist =  ['--method','brackets','-i',argv[0],'-o',argv[1]] + argv[2:]
        main(inputlist)
    except:
        exit(1)

def keylist2mapping(keys):
    """Translates list of key=va; pairs into a dictionary"""
    mapping = {}
    for key in keys:
        k,v = key.split('=')
        if (k != ''):
            mapping[k] = str(v)
    return mapping


def main(argv):
    import argparse
    import sys
    #
    parser = argparse.ArgumentParser(prog='keysubst',description='The program performs keyword substitution.')
    parser.add_argument('-i','--input',type=argparse.FileType('r'),default='-')
    parser.add_argument('-o','--output',type=argparse.FileType('w'),default='-')
    parser.add_argument('--method',choices=['brackets','vars'],default='vars')
    parser.add_argument('-v','--verbose',action='store_true',default=False)
    parser.add_argument('keys',nargs='+')
    #
    #if len(argv) <= 1:
    #    parser.print_usage()
    #    return    
    try:
        args = parser.parse_args(argv)
        mapping = keylist2mapping(args.keys)
        if (args.verbose):
            sys.stderr.write(str(mapping))
        #
        content = args.input.readlines()
        result = []
        if len(mapping):
            result = substitute(content,mapping,method=args.method)    
            args.output.write(''.join(result))
        else:
            args.output.write(''.join(content))
        args.output.flush()
    except Exception as e:
        raise e

if __name__ == "__main__":
    try:
        import sys
        #testargv = '--input template.tmpl key1=value1 key2=value2 key3=value3'
        #main(testargv.split())
        #substFromTemplate(['templateb.tmpl','outb.txt','key1=value1', 'key2=value2', 'key3=value3'])
        main(sys.argv[1:])
    except:
        try:
            sys.exit(1)
        except:
            pass # to suppress SystemExit error message frot the Python runtime
