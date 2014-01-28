import numpy as np
import collections


def readAODF(file_path):
    '''Read AODF file format and returns discrete ODF 
    
    The ODF is represented as a named tuple.
    '''

    with open(file_path,'r') as f:
        content = f.readlines()
        # Process the content
        # line 0 contains comment, irrelevant

        header = [ line.split()[0] for line in content[1:8] ]

        phistep = float(header[0])

        nphi1, phi1_start = (int(header[1]), float(header[2]))
        
        nphi2, phi2_start = (int(header[3]), float(header[4]))
        
        nPhi, Phi_start = (int(header[5]), float(header[6]))
           
        max_cols_per_row = 8
        nrows_per_block =  nphi1 / max_cols_per_row + (nphi1 % max_cols_per_row > 0 and 1 or 0)

        last_row = nphi1 % max_cols_per_row
        
        # process the rest of th file

        # Loop over Phi
        
        odf_grid = np.zeros([nPhi,nphi1,nphi2])

        axis_labels = ['Phi', 'phi_1', 'phi_2']

        axes = np.array([np.arange(Phi_start,nPhi*phistep,step=phistep),
                         np.arange(phi1_start,nphi1*phistep,step=phistep),
                         np.arange(phi2_start,nphi2*phistep,step=phistep)])

        line_idx = 8
        for Phi_idx in range(0,odf_grid.shape[0]):
            for phi2_idx in range(0,odf_grid.shape[2]):
                # loop over the blocks: each contains nphi1 data points, up to max_cols_per_row per line 
                phi1_line  = []
                for i in range(0,nrows_per_block):
                    phi1_line += (float(val) for val in content[line_idx].split())
                    line_idx += 1
                odf_grid[Phi_idx,:,phi2_idx] = phi1_line

       


        odf_class = collections.namedtuple('odf',['odf','axes','labels'])
        
        result = odf_class(odf=odf_grid, axes=axes, labels=axis_labels)

        return result
