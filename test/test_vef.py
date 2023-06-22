#Test bench for VEF software
#Runs a large number of configurations using a dummy data set and compares the hashes of the output data to predetermined hash values.
#USAGE: Run 'pytest' in the 'test' directory of the project.
#       The tests are parametrized over mode, algorithm and slip system.
#REQUIRES: Python, pytest

import os
import hashlib
import shutil
import re
from pathlib import Path
import itertools
import subprocess
import math
import vef_config as conf

import pytest
import pandas as pd
import numpy as np

#File extension for the output of each execution mode. May be removed when we get rid of the file-based I/O for the simulations.
EXTENSIONS = {'ADP':'adp','ASR':'asr', 'EWC':'ewc', 'QRS':'xqrs', 'UDSA':'uds','YLD':'xyld'}
GENERATED_DATA = []
TEST_ROOT = Path.cwd()
TEST_RUN = TEST_ROOT/'run'
TEST_REF = TEST_ROOT/'reference'
TEST_INPUT = TEST_ROOT/'input'

#Configurations that can be tested
MODES = ['ADP', 'ASR', 'EWC','QRS','UDSA','YLD']
ALGORITHMS = ['ALAMEL', 'FCTaylor']
SLIP_SYSTEMS = ['fcc12','bcc24','bcc48']
HARDENING_MODELS = ['NONE', 'VOCE', 'SWIFT', 'BP', 'PEBP_SCREW', 'PEBP_LOOP'] 

#Unit tests
UNITS = [('altayAlgorithms','eigenv'),      \
         ('altayAlgorithms','normaliz'),    \
         ('altayAlgorithms','canoni'),      \
         ('altayAlgorithms','kleinKwa')]

#General setup
if not os.path.exists(TEST_RUN):
    os.mkdir(TEST_RUN)

#Execute simulations themselves. Implemented as a dedicated function to simplify test adjustments when transitioning to a different software architecture.
def generate_output(update, mode, algorithm='ALAMEL', slip_system='bcc24', hardening_model='NONE'):
    with open(TEST_RUN/'test.cfg', 'w') as conf_file:
        conf_file.write(conf.generate_config(mode, algorithm, slip_system, hardening_model))
                            
    try:
        os.remove(TEST_RUN/'out.rtdb')
        os.remove(TEST_RUN/'out.CUR')
    except: FileNotFoundError
    
    shutil.copy(TEST_INPUT/'texture.smt', TEST_RUN)
    shutil.copy(TEST_INPUT/'equiaxed.smt', TEST_RUN)
    shutil.copy(TEST_INPUT/f'{slip_system}.pre', TEST_RUN/f'{slip_system}.pre')
    if hardening_model == 'BP' or hardening_model == 'PEBP_SCREW' or hardening_model == 'PEBP_LOOP':
        with open(TEST_RUN/'DSHparaset.txt','w') as dsh_config:
            dsh_config.write(conf.DSH_CONFIG)

    if not (mode, algorithm, slip_system, hardening_model) in GENERATED_DATA:
        os.chdir(TEST_RUN)

        result = subprocess.run([TEST_ROOT/'../VEF/bin/alamDMC',mode,'test.cfg'],
                                stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
        assert result.returncode == 0
        with open('alamDMC.log','w') as f:
            f.write(result.stdout.decode())

        if mode == 'UDSA':
            with open('out.uds', 'w') as out:
                for orientation in [0,90]:
                    with open(f'out_{orientation}_000.uds','r') as out_oriented:
                        if orientation == 0:
                            out.write(out_oriented.read())
                        else:
                            out.writelines((out_oriented.read().splitlines(True))[2:])
                            

        path = TEST_RUN/f'{mode}_{algorithm}_{slip_system}_{hardening_model}'
        out_path = str(path) + '.out'
        log_path = str(path) + '.log'
        shutil.move(TEST_RUN/f'out.{EXTENSIONS[mode]}', out_path)
        shutil.move(TEST_RUN/'alamDMC.log', log_path)
        GENERATED_DATA.append((mode, algorithm, slip_system, hardening_model))
        TEST_REF.mkdir(parents=True, exist_ok=True)
        if 'log' in update: shutil.copy(log_path, TEST_REF)
        if 'out' in update: shutil.copy(out_path, TEST_REF)

def process_file(path):
    df = pd.read_csv(path,delimiter=' +', engine='python')
    pattern = re.compile(r'^-*0\.[0-9]+E[+\-][0-9]+$')
    res = []
    total = 0
    for index, row in df.iterrows():
        for num in row:
            if isinstance(num, str) and pattern.match(num):
                formatted = num.split('E')
                exp = int(formatted[1])
                if exp > -9:
                    significand = int(formatted[0].replace('-','')[2:4])
                    total = total + significand * pow(10,exp)        
                    res.append(significand)
    filtered = list(filter(lambda e: not e == 0, res))
    return (sum(filtered) / len(filtered), total)

def format_df(df, cols):
    df.columns = df.iloc[0]
    df = (df[1:]).astype(float)
    if cols != []:
        return df[cols]
    else:
        return df

tests_basic = itertools.product(MODES, ALGORITHMS, SLIP_SYSTEMS, HARDENING_MODELS[0:3])
tests_bp = itertools.product(MODES, ALGORITHMS, ['bcc24'], HARDENING_MODELS[3:6])
tests = list(tests_basic) + list(tests_bp)

#Generate and execute the different test cases.
@pytest.mark.integration
@pytest.mark.parametrize('mode,algorithm,slip_system,hardening_model', tests)
def test_vef(mode, algorithm, slip_system, hardening_model, update, margin):
    generate_output(update, mode, algorithm, slip_system, hardening_model)

    if update == 'FALSE' :
        filename =  f'{mode}_{algorithm}_{slip_system}_{hardening_model}.out'
        ref = pd.read_csv(TEST_REF/filename, delimiter=' +', engine='python')
        res = pd.read_csv(TEST_RUN/filename, delimiter=' +', engine='python')
        cols = []

        #Generate formatted dataframe from selected columns of output files based on mode
        if mode == 'ADP':
            cols = ['S_11','S_22','S_33','S_12','S_23','S_13']
        elif mode == 'ASR':
            cols = ['eps_xx', 'eps_yy', 'eps_zz', 'eps_xy', 'eps_yz', 'eps_xz']
        elif mode == 'YLD':
            cols = ['sigma_x', 'sigma_y']
        elif mode == 'UDSA':
            cols = ['S']
        elif mode == 'QRS':
            cols = ['q-value', 'r-value', 's-value']
            
        ref = format_df(ref, cols)
        res = format_df(res, cols)

        #Compare results to reference
        sensitivity = float(margin) / 100.0
        if mode == 'ADP' or mode == 'ASR':
            #None of the components should vary more from the reference than MARGIN times the max. component
            for index, row in res.iterrows():
                row_ref = ref.iloc[index-1]
                tolerance = max(abs(row_ref)) * sensitivity            
                for i in range(len(row)):
                    element = row[i]
                    element_ref = row_ref[i]
                    assert element_ref - tolerance <= element <= element_ref + tolerance
        else:
            #None of the components should vary more than MARGIN from the reference
            for index, row in res.iterrows():
                row_ref = ref.iloc[index-1]
                for i in range(len(row)):
                    element = row[i]
                    element_ref = row_ref[i]
                    if not math.isnan(element_ref):
                        assert abs(element_ref * (1 - sensitivity)) <= abs(element) <= abs(element_ref * (1 + sensitivity))
        
def get_trace_values(path, module, function):
    vals = []
    header = "TRACE " + module + ", " + function
    with open(path) as log_file:
        for line in log_file:
            if header in line:
                vals.append(line.split(':')[1])
    return np.array(vals,dtype=float)

@pytest.mark.unit
@pytest.mark.parametrize('module,function,mode,algorithm,slip_system,hardening_model', [(a,b,c,d,e,f) for ((a,b),(c,d,e,f)) in itertools.product(UNITS, tests)])
def test_unit(mode, algorithm, slip_system, hardening_model, module, function, update):
    generate_output(update, mode, algorithm, slip_system, hardening_model)
    if update == 'FALSE':
        reference = get_trace_values(TEST_REF/f'{mode}_{algorithm}_{slip_system}_{hardening_model}.log', module, function)
        data = get_trace_values(TEST_RUN/f'{mode}_{algorithm}_{slip_system}_{hardening_model}.log', module, function)
        assert np.allclose(reference,data,rtol=1e-3,atol=1e-8)

