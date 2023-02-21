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

import pytest
import pandas as pd
import numpy as np

#File extension for the output of each execution mode. May be removed when we get rid of the file-based I/O for the simulations.
EXTENSIONS = {'ADP':'adp','ASR':'asr', 'EWC':'ewc', 'QRS':'xqrs', 'UDSA':'uds','YLD':'xyld'}
GENERATED_DATA = []
TEST_ROOT = Path.cwd()
TEST_DATA = TEST_ROOT/'run'

#Configurations that can be tested
MODES = ['ADP', 'ASR', 'EWC','QRS','UDSA','YLD']
ALGORITHMS = ['ALAMEL', 'FCTaylor']
SLIP_SYSTEMS = ['fcc12','bcc24','bcc48']
HARDENING_MODEL_SETTINGS = {'NONE':'0',
                            'VOCE':'1\n12.39 15 20 0.2 0.1',
                            'SWIFT_S':'3\n12.39 1.e-3 0.24',
                            'BP':'11\nDSHparaset.txt\nFalse',
                            'PEBP_SCREW':'12\nDSHparaset.txt\nFalse',
                            'PEBP_LOOP':'13\nDSHparaset.txt\nFalse'}
HARDENING_MODELS = list(HARDENING_MODEL_SETTINGS.keys())

#Unit tests
UNITS = [('altayAlgorithms','eigenv'),      \
         ('altayAlgorithms','normaliz'),    \
         ('altayAlgorithms','canoni'),      \
         ('altayAlgorithms','kleinKwa')]

#Set up file structure for benchmark execution. May be removed when we get rid of file I/O for the simulations.
def setup_benchmark(mode, algorithm, slip_system, hardening_model):

    if not os.path.exists(TEST_DATA):
        os.mkdir(TEST_DATA)

    with open(TEST_DATA/'test.cfg', 'w') as conf_file, \
         open(TEST_ROOT/f'conf/{mode}.cfg','r') as mode_specific_conf_file:
        conf_file.write('out\n')
        conf_file.write('True\n')
        conf_file.write('2\n')
        conf_file.write('texture.smt\n')
        conf_file.write(algorithm + '\n')
        conf_file.write('True\n')
        conf_file.write(slip_system + '\n')
        conf_file.write('True\n')
        conf_file.write('False\n')
        conf_file.write(HARDENING_MODEL_SETTINGS[hardening_model] + '\n')
        conf_file.write('True\n')
        conf_file.write(mode_specific_conf_file.read())

    try:
        os.remove(TEST_DATA/'out.rtdb')
        os.remove(TEST_DATA/'out.CUR')
    except: FileNotFoundError
    if (mode == 'YLD' or mode == 'QRS'):
        shutil.copy(TEST_ROOT/'data/in/texture_yld.smt', TEST_DATA/'texture.smt')
    else:
        shutil.copy(TEST_ROOT/'data/in/sid1687f_short.smt', TEST_DATA/'texture.smt')
    shutil.copy(TEST_ROOT/'../data/equiaxed.smt', TEST_DATA/'equiaxed.smt')
    shutil.copy(TEST_ROOT/f'../data/{slip_system}.pre', TEST_DATA/f'{slip_system}.pre')
    shutil.copy(TEST_ROOT/f'data/in/DSHparaset.txt', TEST_DATA/'DSHparaset.txt')
    if (mode == 'EWC' or mode == 'ASR'):
        shutil.copyfile(TEST_ROOT/f'data/in/{mode}.rtdb', TEST_DATA/'out.rtdb')
    elif (mode == 'UDSA' or mode == 'YLD'):
        shutil.copyfile(TEST_ROOT/f'data/in/UDSA_YLD.rtdb', TEST_DATA/'out.rtdb')



#Execute simulations themselves. Implemented as a dedicated function to simplify test adjustments when transitioning to a different software architecture.
def generate_output(update, mode, algorithm='ALAMEL', slip_system='bcc24', hardening_model='NONE'):
    setup_benchmark(mode, algorithm, slip_system, hardening_model)

    if not (mode, algorithm, slip_system, hardening_model) in GENERATED_DATA:
        os.chdir(TEST_DATA)

        result = subprocess.run([TEST_ROOT/'../VEF/bin/alamDMC',mode,'test.cfg'],
                                stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
        assert result.returncode == 0
        with open('alamDMC.log','w') as f:
            f.write(result.stdout.decode())

        if mode == 'UDSA':
            with open('out.uds', 'w') as out:
                for orientation in [0,45,90]:
                    with open(f'out_{orientation}_000.uds','r') as out_oriented:
                        out.write(out_oriented.read())

        path = TEST_DATA/f'{mode}_{algorithm}_{slip_system}_{hardening_model}'
        out_path = str(path) + '.out'
        log_path = str(path) + '.log'
        shutil.move(TEST_DATA/f'out.{EXTENSIONS[mode]}', out_path)
        shutil.move(TEST_DATA/'alamDMC.log', log_path)
        GENERATED_DATA.append((mode, algorithm, slip_system, hardening_model))
        ref_path = TEST_ROOT/'data/out/'
        ref_path.mkdir(parents=True, exist_ok=True)
        if 'log' in update: shutil.copy(log_path, ref_path)
        if 'out' in update: shutil.copy(out_path, ref_path)

def process_file(path):
    df = pd.read_csv(path,delimiter=' +', engine='python')
    pattern = re.compile(r'^-*0\.[0-9]+E[+\-][0-9]+$')
    res = []
    for index, row in df.iterrows():
        for num in row:
            if isinstance(num, str) and pattern.match(num):
                formatted = num.split('E')
                if int(formatted[1]) > -9:
                    res.append(int(formatted[0].replace('-','')[2:4]))
    filtered = list(filter(lambda e: not e == 0, res))
    return sum(filtered) / len(filtered)

tests_basic = itertools.product(MODES, ALGORITHMS, SLIP_SYSTEMS, HARDENING_MODELS[0:4])
tests_bp = itertools.product(MODES, ALGORITHMS, ['bcc24'], HARDENING_MODELS[4:7])
tests = list(tests_basic) + list(tests_bp)

#Generate and execute the different test cases.
@pytest.mark.integration
@pytest.mark.parametrize('mode,algorithm,slip_system,hardening_model', tests)
def test_vef(mode, algorithm, slip_system, hardening_model, update):
    generate_output(update, mode, algorithm, slip_system, hardening_model)

    filename =  f'{mode}_{algorithm}_{slip_system}_{hardening_model}.out'
    ref = process_file(TEST_ROOT/'data/out'/filename)
    res = process_file(TEST_DATA/filename)
    assert ref > res * 0.95 and ref < res * 1.05
    ratio = round(res/ref, 2)
    print(f'Ratio {mode}, {algorithm}, {slip_system}, {hardening_model}: {ratio}')

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
    reference = get_trace_values(TEST_ROOT/f'data/out/{mode}_{algorithm}_{slip_system}_{hardening_model}.log', module, function)
    data = get_trace_values(TEST_DATA/f'{mode}_{algorithm}_{slip_system}_{hardening_model}.log', module, function)
    assert np.allclose(reference,data,rtol=1e-3,atol=1e-8)

