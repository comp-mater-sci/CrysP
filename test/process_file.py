import pandas as pd
import numpy as np
import math
import re


#with open('data/out/ADP_ALAMEL_bcc24.out','r') as data:
#    txt = data.readlines()
#    del txt[1]
#    for line in txt:
#        line = re.sub(' +', ' ', line)
#    print(txt)
for mode in ['ADP', 'ASR', 'EWC', 'QRS', 'UDSA', 'YLD']:
    for algorithm in ['ALAMEL', 'FCTaylor']:
        for slip_system in ['fcc12', 'bcc24', 'bcc48']:
            data = pd.read_csv(f'data/out/{mode}_{algorithm}_{slip_system}.out',delimiter=' +').drop(0)
            pattern = re.compile("^-*0\.[0-9]+E[+\-][0-9]+$")
            for index, row in data.iterrows():
                for num in row:
                    if isinstance(num, str) and pattern.match(num):
                        formatted = num.split('E')
                        if int(formatted[1]) > -9:
                            print(formatted[0].replace('-','')[2:8])
                    #print(decompose(np.float32(num)))






