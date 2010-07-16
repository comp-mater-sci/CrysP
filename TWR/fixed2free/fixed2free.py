import re
"""
Converts fixed-form Fortran source into free-from

Known bugs:
 - the exclamation mark (!) is improperly recognized as Fortran90 comment even if it is a part of string.
"""
verbose=0

commentLinePattern = re.compile(r"^[Cc]{2,}\s")
commentChar = re.compile(r"[Cc]")
fixedCommentPattern = re.compile(r"^[CcDd*]")
def fixComment(cline):
    if fixedCommentPattern.match(cline):
        # This is old-style comment. Fix it.    
        m = commentLinePattern.match(cline)
        if m:
            # Substitute 
            cline =     commentChar.sub('!',cline,m.end()-1)
        else:
            cline  = fixedCommentPattern.sub('!',cline)
    return cline

def convert(fixedFname, freeFname, alignCont = 72):
    """
    Simple conversion from Fortran fixed-form source into free-form.

    The following conversions are made:
      * comments are converted into free-from style (exclamation marks)
      * continuation lines are converted into new format.
    """
    import sys
    import re
    commentPattern = re.compile(r"^[cCdD*!]")
    cntPattern = re.compile(r"^( {5}|\t)\S")
    excommentPattern = re.compile(r"!")
    # excommentPattern = re.compile(r"!.*$")
    try:
            with open(fixedFname,'r') as inp, open(freeFname,'w') as out:
                    # Skip comments in header
                    lineready = False
                  
                    for line in inp:
                            if not(commentPattern.match(line)):
                                    #print(line)
                                    lineready = True
                                    break
                            out.writelines(fixComment(line))
                    #
                    if line and lineready:        
                            prevline = line
                            verbose > 1 and print('start>>' + prevline)
                    else:
                            # nothing to do.
                            return
                    commentBlock = []
                    # process the input file. Keep the current line and the previous line
                    for line in inp:
                            if (commentPattern.match(line)):
                                    #out.writelines(line)
                                    commentBlock.append(fixComment(line))
                                    continue
                            
                            if (cntPattern.match(line)):
                                    # print(line)
                                    # clear continuation mark
                                    line = cntPattern.sub('      ',line)
                                    # analyze the previous line
                                    # Search for exclamation-comment
                                    m = excommentPattern.search(prevline)
                                    if (m):
                                            #print('in>> ' + prevline)
                                            # A bit more complex expression
                                            # Split into statement and comment (remove leading '!')
                                            splt = excommentPattern.split(prevline,1)
                                            prevline = (splt[0].ljust(alignCont) + ' & ' +  '!' + splt[1])
                                            #print('out<< ' + prevline)
                                    else:
                                            prevline =  prevline[:len(prevline)-1].ljust(alignCont)  + ' &\n'
                            # emit the previous line, there will be no further interest in it        
                            out.writelines(prevline)
                            # emit deferred comment blocks
                            if (len(commentBlock) > 0):
                                    verbose > 1 and print('Deffered comment block, # lines:',str(len(commentBlock)))
                                    for cline in commentBlock:
                                            out.writelines(cline)
                                    commentBlock = []
                            prevline = line

                    # End of main loop      
                    # emit remaining line and comment
                    out.writelines(prevline)
                    # emit deferred comment blocks
                    if (len(commentBlock) > 0):
                        verbose > 1 and print('Deffered comment block, # lines:',str(len(commentBlock)))
                        for cline in commentBlock:
                            out.writelines(cline)
                        commentBlock = []
    except IOError as e:
            print(e.args)
                

# Make it ready to act as a standalone program
if __name__ == "__main__":
    import sys
    if len(sys.argv) < 3:
        print('Syntax: ' + sys.argv[0] + ' fixed_input free_output  [alignment = 72]')
        sys.exit(0)
    alignment = 72
    if len(sys.argv) == 4:
        # interpret alignment parameter
        try:
            alignment = int(sys.argv[3])
        except ValueError:
            print('Cannot convert parameter ' + sys.argv[3] + ' into integer number')
            sys.exit(1)
    convert(sys.argv[1],sys.argv[2],alignment)
    
