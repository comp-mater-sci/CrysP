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

def substFromTemplate(argv):    
    """ 
    Arguments in argv: input, output, "KEY1=VAL1" "KEY2=VAL2" ... 
    """
    import os
    import sys
    
    try:
        inp_fname = argv[0]
        out_fname = argv[1]
    except IndexError:
        print('The script substitutes keywords by associated values.\n')
        print('Arguments: input_template_file output_file ["KEY1=VAL1" "KEY2=VAL2" ...]')
        print('Keywords in the input_template should be embraced in angle brackets \"<>\"\ne.g. <keyname>')
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
    for arg in argv[2:]:
        key,val = arg.split('=')
        if (key != ''):
            mapping[key] = str(val)
    print(mapping)
    filtered = substitute(content,mapping)
    out.writelines(filtered)
    out.close()

def main(argv):
    substFromTemplate(argv[1:])

if __name__ == "__main__":
    import sys
    main(sys.argv)

