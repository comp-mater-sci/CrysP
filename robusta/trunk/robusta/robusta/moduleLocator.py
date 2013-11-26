""" The following module is a work around to allow a currently executing
    script to determine the directory it has been called from
"""
import os
import sys

def Frozen():
    # All of the modules are built-in to the interpreter, e.g., by py2exe
    return hasattr(sys, "frozen")

def modulePath():
    encoding = sys.getfilesystemencoding()
    if Frozen():
        return os.path.dirname(unicode(sys.executable, encoding))
    return os.path.dirname(unicode(__file__, encoding))
