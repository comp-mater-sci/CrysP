from robusta.inuit.FlatODB import *


def test():
    fileName = 'copy_test'
    folder = '/home/diarmuid/scratch_abaqus'
    entity = 'test_entity'
    run = 'test_run'

    a = FlatODB(hdfFileName=fileName, folder=folder, mode='append')
    a.SetCurrentEntity(entity)
    a.SetCurrentRun(run)
    
    return a
