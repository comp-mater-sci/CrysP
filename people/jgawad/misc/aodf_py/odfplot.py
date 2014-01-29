import os
import numpy as np
import aodfimport

# ------------------ taken from the example 
# http://matplotlib.org/examples/pylab_examples/contour_label_demo.html
# Define a class that forces representation of float to look a certain way
# This remove trailing zero so '1.0' becomes '1'
class _nf(float):
        def __repr__(self):
            str = '%.1f' % (self.__float__(),)
            if str[-1]=='0':
                return '%.0f' % self.__float__()
            else:
                return '%.1f' % self.__float__()


def plotODF(odf,fname,formats=None,color=True):
    import matplotlib
    import matplotlib.pyplot as plt


    x_start, x_end = (odf.axes[1][0], odf.axes[1][-1])
    y_start, y_end = (odf.axes[0][0], odf.axes[0][-1])

    x_step = 15.
    y_step = 15.

    aspect_ratio = (y_end - y_start) / (x_end - x_start)

    base_size = 16

    fig = plt.figure(figsize=(base_size,aspect_ratio*base_size))
    
    axes = plt.gca() 
    axes.set_aspect('equal')

    axes.set_xbound(lower=x_start, upper=x_end)
    axes.set_ybound(lower=y_start, upper=y_end)

    axes.set_xticks(list(np.arange(x_start,x_end,x_step)) + [x_end])
    axes.set_yticks(list(np.arange(y_start,y_end,y_step)) + [y_end])

    axes.invert_yaxis()

    axes.grid(True)

    levels = [.70, 1.00, 1.40, 2.0, 2.80, 4.00, 5.60, 8.0, 11.0, 16.0]
    
    if color:
        colors = 2*('blue','green','red','black') + ('blue','green')
        linestyles = 'solid'
        print zip(levels,colors)
    else:
        colors = 'black'
        linestyles =  ['solid', 'dashed', 'dashdot', 'dotted']


    X,Y = np.meshgrid(odf.axes[1],odf.axes[0])
    Z = odf.odf[:,:,9]

    CS = plt.contour(X, Y, Z, levels=levels, colors=colors, linestyles=linestyles)
    #plt.show() 


    

    # Recast levels to _nf class
    CS.levels = [_nf(val) for val in CS.levels ]

    plt.clabel(CS, CS.levels, inline=True, fmt='%r', fontsize=10)

    barefname,ext = os.path.splitext(fname)
    graphics_formats = formats or (ext and [ext.strip('.')]) or []
    
    for fmt in graphics_formats:
        fig.savefig('{fname}.{ext}'.format(fname=barefname,ext=fmt),dpi=300,bbox_inches='tight')

    plt.show()
    plt.close()



if __name__ == '__main__':
    odf = aodfimport.readAODF('alum1112.aodf')
    plotODF(odf,'figure_1',formats=['pdf','png','svg'],color=True)
    # plotODF(odf,'figure_1.pdf',color=True)
    pass