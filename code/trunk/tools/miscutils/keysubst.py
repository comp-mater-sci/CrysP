#!/usr/bin/python
# $Id$

def substitute(content,mapping):
    filtered = []
    for line in content:
        for mpkey,mpval in mapping.items():
            line = line.replace('<'+mpkey+'>',str(mpval))
        #print(line)
        filtered.append(line)
    return filtered

if __name__ == "__main__":
    import os
    import sys
    # arguments: input, output, "KEY1=VAL1" "KEY2=VAL2" ... 
    
    try:
        inp_fname = sys.argv[1]
        out_fname = sys.argv[2]
    except IndexError:
        print('\nThe script substitutes keywords by associated values')
        print('Arguments: input_template_file output_file ["KEY1=VAL1" "KEY2=VAL2" ...]')
        print('Keywords in the input_template are in form <keyname>')
        exit()
    try:
        inp = open(inp_fname,'r')
        content = inp.readlines()
        inp.close()
        #    
        out = open(out_fname,'w')
    except:
        print('Cannot process file')
        exit(1)
    # 
    mapping = {}
    # process arguments, create pairs
    for arg in sys.argv[3:]:
        key,val = arg.split('=')
        if (key != ''):
            mapping[key] = str(val)
    print(mapping)
    filtered = substitute(content,mapping)
    out.writelines(filtered)
    out.close()


