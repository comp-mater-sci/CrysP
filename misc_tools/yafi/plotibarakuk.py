""" load all npy, yld and xyld files in the current directory and
    try to plot them
"""
import os, glob, sys, tempfile, shutil, subprocess
import numpy as np
import matplotlib as m
import matplotlib.pyplot as plt
from itertools import cycle
import click

# global matplotlib settings
xkcdStyle = False

if xkcdStyle:
    plt.xkcd()

m.rcParams.update({'path.sketch':(0.3,500.,20.0),
                       'font.size':20})

# global data
global LINE_TYPES
global SOURCE_COLS
global DATA_FORMAT
global LINE_WIDTH
global MARKER_SIZE
global POINT_TYPE
global MARKER_LINE_WIDTH
global NPY_STEP

NPY_STEP = 7
MARKER_LINE_WIDTH = 1
POINT_TYPE = 'o'
MARKER_SIZE = 8
LINE_WIDTH = 3
DPI = 2000

DATA_COLS = {'npy':{'val':[0,1], 'type':'xy', 'derv':{'e1e2':[2, 3], 'e1e3':[2, 4], 'e2e3':[3, 4], 'rval':[3,4], 'type':'xy'}},
             'yld':{'val':[2,0], 'derv':None},
             'xyld':{'val':[6,7], 'type':'xy', 'derv':{'e1e2':[8, 9], 'e1e3':[8, 9], 'e2e3':[8, 9], 'rval':[8,9], 'type':'xy'}},
             'nosource':{'val':[0,1], 'derv':None},
             'xqrs':{'val':[0,1], 'type':'rtheta', 'derv':{'rval':3,  'type':'rtheta'}}}
DATA_FORMAT = {'npy':{'header':None},
               'yld':{'header':2},
               'xyld':{'header':2},
               'xqrs':{'header':2}}



def Brewer_colours():
    return cycle(['#377eb8','#4daf4a','#e41a1c','#984ea3', '#ff7f00'])


def plot_kuwabara_all_data(axObj, allData, source, linetype, colours, plotType):
    
    if not allData is None:
        plotLines = []
        plotLabels = []
        for itemNum in range(len(allData[0])):
            plotLabels.append(allData[1][itemNum])
            plotLines.extend(kuwabara_plot_from_xy(axObj, allData[0][itemNum],
                             source, allData[1][itemNum], linetype,
                             colours.next(), plotType))
                             
        return plotLines, plotLabels                 


def kuwabara_plot_from_xy(axObj, data, source, label, linetype, colour, plotType):
    xCol, yCol = DATA_COLS[source]['val'] 
    dxCol, dyCol = DATA_COLS[source]['derv'][plotType]
    numLines = data.shape[0]
    
    stheta = np.zeros((numLines,))
    dtheta = np.zeros((numLines,))

    for i in range(numLines):
        x, y = data[i,xCol], data[i,yCol]
        dx, dy = data[i,dxCol], data[i,dyCol]
        stheta[i] = np.degrees(np.arctan2(y, x))
        dtheta[i] = np.degrees(np.arctan2(dy, dx))

    sortIndex = np.argsort(stheta)

    if linetype == POINT_TYPE:
        plotObj = axObj.plot(stheta[sortIndex], dtheta[sortIndex], POINT_TYPE,
                         label=label, markerfacecolor='none', mew=MARKER_LINE_WIDTH, zorder=2,
                         markersize=MARKER_SIZE, markeredgecolor=colour)
    else:
        plotObj = axObj.plot(stheta[sortIndex], dtheta[sortIndex], linetype,
                         label=label, color=colour, lw=LINE_WIDTH, zorder=1)
    axObj.set_xlabel(r'Stress direction $\varphi$ [$^{\circ}$]')
    axObj.set_ylabel(r'Projected strain rate direction $\theta$ [$^{\circ}$]')
        
    return plotObj
    

def format_kuwabara_plot(axObj, limits=[0,90,0,90]):
    axObj.set_aspect(1)
    set_axis_limits(axObj, limits)
    
    
def plot_locus_from_rtheta(axObj, data, source, label, linetype,
                           colour, rotation=2.*np.pi):
    rCol, thetaCol = DATA_COLS[source]['val']
    
    numLines = data.shape[0]
    x = np.zeros((numLines,1))
    y = np.zeros((numLines,1))
    for i in range(numLines):
        x[i] = np.cos(data[i,thetaCol]+rotation) * data[i,rCol]
        y[i] = np.sin(data[i,thetaCol]+rotation) * data[i,rCol]

    return plot_locus_from_xy(axObj, np.hstack((x,y)), 'nosource', label,
                              linetype, colour)


def plot_locus_from_xy(axObj, data, source, label, linetype, colour):
    print 'plotting {0}'.format(label)
    xCol, yCol = DATA_COLS[source]['val']
    if linetype == POINT_TYPE:
        return axObj.plot(data[:,xCol], data[:,yCol], POINT_TYPE,
                   label=label, markerfacecolor='none', mew=MARKER_LINE_WIDTH, zorder=2,
                   markersize=MARKER_SIZE, markeredgecolor=colour)
    else:    
        return axObj.plot(data[:,xCol], data[:,yCol], linetype,
                   label=label, color=colour, lw=LINE_WIDTH, zorder=1)
                   

def plot_all_data(axObj, allData, source, linetype, colours, workDir):
    if not allData is None:
        plotLines = []
        plotLabels = get_labels_for_fileNames(allData[1], workDir)
        for itemNum in range(len(allData[0])):
            sourceFileName = allData[1][itemNum]
            labelName = plotLabels.get(sourceFileName)
            print 'plotting {0}'.format(sourceFileName)
            plotLines.extend(plot_locus_from_xy(axObj, allData[0][itemNum],
                             source, labelName, linetype, colours.next()))
                             
        return plotLines, plotLabels.values()
    

def format_locus_plot(axObj, limits=[-1,1,-1,1]):
    axObj.spines['left'].set_position('center')
    axObj.spines['right'].set_color('none')
    axObj.spines['bottom'].set_position('center')
    axObj.spines['top'].set_color('none')
    axObj.xaxis.set_ticks_position('bottom')
    axObj.yaxis.set_ticks_position('left')
    axObj.set_aspect(1)
    set_axis_limits(axObj, limits)


def set_axis_limits(axObj, limits):
    limits = list(limits)
    limits[0] = - abs(limits[0])
    limits[2] = - abs(limits[2])
    axObj.set_xlim([limits[0], limits[1]])
    axObj.set_ylim([limits[2], limits[3]])


def find_filter_filelist(extension, searchKey, folder):
    return glob.glob(os.path.join(folder, '*{0}*.{1}'.format(searchKey, extension)))


def load_data_from_path(path, step=NPY_STEP):
    fileExtension = os.path.splitext(path)[1][1::]
    if fileExtension=='npy':
        return np.load(path)
    else:
        return np.genfromtxt(path, skip_header=DATA_FORMAT[fileExtension]['header'])[0:-1:step,:]

        
def load_data_from_pathlist(pathList):
    if pathList == []:
        print 'no paths found'
    else:
        allData = []
        allNames = []
        for path in np.sort(pathList):
            data = load_data_from_path(path)    
            allNames.append('{0}'.format(os.path.splitext(os.path.basename(path))[0][0:-5]))
            allData.append(data)
            
        return allData, allNames
    

def write_tiff(saveFolder, prefix):
    tempFolder =  tempfile.mkdtemp()
    try:
        tempTiff = os.path.join(tempFolder, 'temp.tiff')
        plt.savefig(tempTiff, DPI=DPI, bbox_inches='tight')
        print subprocess.check_output('convert {0} -compress lzw  {1}'.format(tempTiff,
                                      'compressed.tiff'), cwd=tempFolder, shell=True)
        shutil.copyfile(tempTiff, os.path.join(saveFolder, '{0}_{1}.tiff'.format(prefix, os.path.basename(saveFolder))))
        
    finally:
        shutil.rmtree(tempFolder)


def get_labels_for_fileNames(fileNames, workDir):
    metaPath = os.path.join(workDir, 'metadata.csv')
    if os.path.isfile(metaPath):
        data = np.genfromtxt(metaPath, delimiter=',', dtype=np.str)
        xref = dict((i[0].strip() ,i[1].strip()) for i in data)
    else:
        xref = {}

    # if there is missing data, subsitute it with the file name
    for name in fileNames:
        if not name in xref:
            xref.update({name:os.path.splitext(os.path.basename(name))[0]})
    
    return xref


@click.group()
def main():
    pass

@main.command(name='bykey')
@click.argument('folder', type=click.Path(exists=True))
@click.argument('keyword', type=str)
@click.option('--kuwalimits', default=[0,90,0,90], nargs=4, type=float,
              help='axis limits for kuwabara plot [xmin, xmax, ymin, ymax]')
@click.option('--locilimits', default=[-1.,1.,-1.,1.], nargs=4, type=float,
              help='axis limits for yield loci [xmin, xmax, ymin, ymax]')
def _wrap_s11s22_loci_kuwabara(**kwargs):
    bykey_loci_kuwabara(**kwargs)

def bykey_loci_kuwabara(folder, keyword, kuwalimits=[0,90,0,90],
                         locilimits=[-1.,1.,-1.,1.], pointColour='k'):
    workDir = os.path.realpath(folder)

    # create a figure with 2 subplots
    fig, ((yieldAx, kuwaAx)) = plt.subplots(1, 2)
    fig.set_figheight(10)
    fig.set_figwidth(18)

    # load data
    alamDMC = load_data_from_pathlist(find_filter_filelist('xyld', searchKey=keyword, folder=workDir))
    yafiData = load_data_from_pathlist(find_filter_filelist('npy', searchKey=keyword, folder=workDir))

    # plot yield locus 
    pointColours = cycle([pointColour])
    lines, labels = plot_all_data(yieldAx, allData=yafiData, source='npy',
                                  linetype='-', colours=Brewer_colours(),
                                  workDir=workDir)
    plot_all_data(yieldAx, allData=alamDMC, source='xyld', workDir=workDir,
                  linetype=POINT_TYPE, colours=pointColours)

    # plot kuwabara diagrams
    plot_kuwabara_all_data(kuwaAx, allData=yafiData, source='npy',
                           linetype='-', colours=Brewer_colours(), plotType='e1e2')
    plot_kuwabara_all_data(kuwaAx, allData=alamDMC, source='xyld',
                           linetype=POINT_TYPE, colours=pointColours, plotType='e1e2')

    # add legend and final adjustments to plot sizes
    plt.legend(lines, labels, bbox_to_anchor=(1, 1),
               bbox_transform=plt.gcf().transFigure)
    format_locus_plot(yieldAx, locilimits)
    format_kuwabara_plot(kuwaAx, kuwalimits)

    # write output
    write_tiff(workDir, prefix='key-'+keyword)
    

@main.command(name='devyield')
@click.argument('folder', type=click.Path(exists=True))
@click.option('--kuwalimits', default=[0,90,0,90], nargs=4, type=float,
              help='axis limits for kuwabara plot [xmin, xmax, ymin, ymax]')
@click.option('--locilimits', default=[-1.,1.,-1.,1.], nargs=4, type=float,
              help='axis limits for yield loci [xmin, xmax, ymin, ymax]')
def _wrap_devspace_loci_kuwabara(**kwargs):
    devspace_loci_kuwabara(**kwargs)

def devspace_loci_kuwabara(folder, kuwalimits=[0,90,0,90],
                           locilimits=[-1.,1.,-1.,1.], pointColour='k'):
    workDir = os.path.realpath(folder)

    # create a figure with 8 subplots
    fig, ((axr1c1, axr1c2, axr1c3), (axr2c1, axr2c2, axr2c3)) = plt.subplots(2, 3)
    fig.set_figheight(20)
    fig.set_figwidth(25)

    # load alamDMC data
    e1e2AlamDMC = load_data_from_pathlist(find_filter_filelist('xyld', searchKey='e1e2', folder=workDir))
    e1e3AlamDMC = load_data_from_pathlist(find_filter_filelist('xyld', searchKey='e1e3', folder=workDir))
    e2e3AlamDMC = load_data_from_pathlist(find_filter_filelist('xyld', searchKey='e2e3', folder=workDir))

    # load yafi data
    e1e2YafiData = load_data_from_pathlist(find_filter_filelist('npy', searchKey='e1e2', folder=workDir))
    e1e3YafiData = load_data_from_pathlist(find_filter_filelist('npy', searchKey='e1e3', folder=workDir))
    e2e3YafiData = load_data_from_pathlist(find_filter_filelist('npy', searchKey='e2e3', folder=workDir))

    # plot e1 e2 e3 yield locus sections
    pointColours = cycle([pointColour])
    lines, labels = plot_all_data(axr1c1, allData=e1e2YafiData, source='npy',
                                  linetype='-', colours=Brewer_colours(),  workDir=workDir)
    plot_all_data(axr1c1, allData=e1e2AlamDMC, source='xyld', workDir=workDir,
                  linetype=POINT_TYPE, colours=pointColours)

    plot_all_data(axr1c2, allData=e1e3YafiData, source='npy', workDir=workDir,
                 linetype='-', colours=Brewer_colours())
    plot_all_data(axr1c2, allData=e1e3AlamDMC, source='xyld', workDir=workDir,
                  linetype=POINT_TYPE, colours=pointColours)

    plot_all_data(axr1c3, allData=e2e3YafiData, source='npy', workDir=workDir,
                  linetype='-', colours=Brewer_colours())
    plot_all_data(axr1c3, allData=e2e3AlamDMC, source='xyld', workDir=workDir,
                  linetype=POINT_TYPE, colours=pointColours)

    # plot kuwabara diagrams
    plot_kuwabara_all_data(axr2c1, allData=e1e2YafiData, source='npy',
                           linetype='-', colours=Brewer_colours(), plotType='e1e2')
    plot_kuwabara_all_data(axr2c1, allData=e1e2AlamDMC, source='xyld',
                           linetype=POINT_TYPE, colours=pointColours, plotType='e1e2')

    plot_kuwabara_all_data(axr2c2, allData=e1e3YafiData, source='npy',
                           linetype='-', colours=Brewer_colours(), plotType='e1e3')
    plot_kuwabara_all_data(axr2c2, allData=e1e3AlamDMC, source='xyld',
                           linetype=POINT_TYPE, colours=pointColours, plotType='e1e3')

    plot_kuwabara_all_data(axr2c3, allData=e2e3YafiData, source='npy',
                           linetype='-', colours=Brewer_colours(), plotType='e2e3')
    plot_kuwabara_all_data(axr2c3, allData=e2e3AlamDMC, source='xyld',
                           linetype=POINT_TYPE, colours=pointColours, plotType='e2e3')


    # add legend
    plt.legend(lines, labels, bbox_to_anchor=(1, 1),
               bbox_transform=plt.gcf().transFigure)

    # final adjustments to plot sizes
    format_locus_plot(axr1c1, locilimits)
    format_locus_plot(axr1c2, locilimits)
    format_locus_plot(axr1c3, locilimits)

    format_kuwabara_plot(axr2c1, kuwalimits)
    format_kuwabara_plot(axr2c2, kuwalimits)
    format_kuwabara_plot(axr2c3, kuwalimits)

    # write output
    write_tiff(workDir, prefix='devspace_kuwa_loci')
        
if __name__=='__main__':
    main()   
