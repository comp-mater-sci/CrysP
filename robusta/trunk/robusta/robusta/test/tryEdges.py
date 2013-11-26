def getEdges():

    from abaqus import *
    from abaqusConstants import *
    
    mdbFileName = 'PSC_2D_0.cae'

    m = openMdb(mdbFileName)

    p = m.models['PSC_2D_0'].parts['sample']

    els = p.elements

    edgeLists = [el.getElemEdges() for el in els]
    
    nodeLists = [[[node.label for node in edge.getNodes()] for edge in edgeList] for edgeList in edgeLists]

    return nodeLists
