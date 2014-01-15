from robusta.post.HmsArchive import *
h = HmsArchive('test')
h.ScanLocDir()
h.ScanIPFolder()
h.GetStrainIncrementsForIP(1)
h.WriteStrainIncrementForIP(1)
