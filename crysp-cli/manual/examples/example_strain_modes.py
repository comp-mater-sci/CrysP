import numpy as np
import math

def deviatoric_to_tensor(dev):
    return np.stack([np.stack([math.sqrt(0.5)*dev[...,0] + dev[...,1]/math.sqrt(6),
                               math.sqrt(0.5)*dev[...,4],
                               math.sqrt(0.5)*dev[...,3]],-1),
                     np.stack([math.sqrt(0.5)*dev[...,4],
                              -math.sqrt(0.5)*dev[...,0] + dev[...,1]/math.sqrt(6),
                               math.sqrt(0.5)*dev[...,2]],-1),
                     np.stack([math.sqrt(0.5)*dev[...,3],
                               math.sqrt(0.5)*dev[...,2],
                              -math.sqrt(2/3)*dev[...,1]],-1)],-2)

rng = np.random.default_rng(4)
vectors = rng.random((100000, 5)) * 2 - 1
vectors /= np.linalg.norm(vectors,axis=-1,keepdims=True)
tensors = deviatoric_to_tensor(vectors)
for tensor in tensors:
    print(*np.round(tensor.flatten(),10))
    print(0.0)
