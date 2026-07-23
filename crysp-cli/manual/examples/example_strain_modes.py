import numpy as np
import math

def deviatoric_to_tensor(dev):
    tensor = []
    tensor.append(math.sqrt(0.5)*dev[0] + 1/math.sqrt(6)*dev[1])
    tensor.append(math.sqrt(0.5)*dev[4])
    tensor.append(math.sqrt(0.5)*dev[3])
    tensor.append(tensor[1])
    tensor.append(-math.sqrt(0.5)*dev[0] + 1/math.sqrt(6)*dev[1])
    tensor.append(math.sqrt(0.5)*dev[2])
    tensor.append(tensor[2])
    tensor.append(tensor[5])
    tensor.append(-math.sqrt(2/3)*dev[1])
    return tensor

vectors = np.random.rand(100000, 5) * 2 - 1
ts = []
for v in vectors:
    ts.append(np.round(deviatoric_to_tensor(v/np.linalg.norm(v)),10))
for t in ts:
    print(*t)
    print(0.0)
