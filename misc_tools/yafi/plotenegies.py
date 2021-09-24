""" plot energies stored in npy files
"""
import os, glob
import numpy as np
import matplotlib.pyplot as plt

for path in glob.glob('*energies*.npy'):
    stub = os.path.splitext(os.path.basename(path))[0]
    plt.plot(np.sort(np.load(path)), label=stub)
    
plt.legend()
plt.show()
