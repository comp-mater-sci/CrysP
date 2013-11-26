""" some shortcuts for testing robusta set ups
"""
import sys

def run(simType):

    if simType=='asr':
        commandString = 'import robusta.simbuild.asr'
    elif simType=='psc':
        commandString = 'import robusta.simbuild.pscupset'
    else:
        raise Exception('Unknown set up <{0}>'.format(simType))

def kill():
    sys.exit()
