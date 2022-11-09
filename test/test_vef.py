#Test bench for VEF software
#Runs a large number of configurations using a dummy data set and compares the hashes of the output data to predetermined hash values.
#USAGE: Run 'pytest' in the 'test' directory of the project. The tests are parametrized over mode, algorithm and slip system. If you want to run e.g. only the tests pertaining to ADP, run "pytest -k ADP"
#REQUIRES: Python, pytest

import os
import hashlib
import shutil
import re
import pandas as pd
from pathlib import Path

import pytest

#File extension for the output of each execution mode. May be removed when we get rid of the file-based I/O for the simulations.
EXTENSIONS = {'ADP':'adp','ASR':'asr', 'EWC':'ewc', 'QRS':'xqrs', 'UDSA':'uds','YLD':'xyld'}
GENERATED_DATA = []
TEST_ROOT=Path.cwd()
TEST_DATA=TEST_ROOT/'run'

#Generate configuration file based on global settings and mode-specific ones.
@pytest.fixture
def create_conf_file(mode, algorithm, slip_system):
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
        conf_file.write('True\n')
        conf_file.write('True\n')
        conf_file.write(mode_specific_conf_file.read())

#Set up file structure for benchmark execution. May be removed when we get rid of file I/O for the simulations.
@pytest.fixture
def setup_benchmark(mode, algorithm, slip_system, create_conf_file):
    try:
        os.remove(TEST_DATA/'out.rtdb')
        os.remove(TEST_DATA/'out.CUR')
    except: FileNotFoundError 
    if (mode == 'YLD' or mode == 'QRS'):
        shutil.copy(TEST_ROOT/'data/in/texture_yld.smt', TEST_DATA/'texture.smt')
    else:
        shutil.copy(TEST_ROOT/'data/in/sid1687f_short.smt', TEST_DATA/'texture.smt')
    shutil.copy(TEST_ROOT/'../VEF/data/equiaxed.smt', TEST_DATA/'equiaxed.smt')
    shutil.copy(TEST_ROOT/f'../VEF/data/{slip_system}.pre', TEST_DATA/f'{slip_system}.pre')
    if (mode == 'EWC' or mode == 'ASR'):
        shutil.copyfile(TEST_ROOT/f'data/in/{mode}.rtdb', TEST_DATA/'out.rtdb')
    elif (mode == 'UDSA' or mode == 'YLD'):
        shutil.copyfile(TEST_ROOT/f'data/in/UDSA_YLD.rtdb', TEST_DATA/'out.rtdb')



#Execute simulations themselves. Implemented as a dedicated function to simplify test adjustments when transitioning to a different software architecture.
@pytest.fixture
def generate_output(mode, algorithm, slip_system, setup_benchmark, request):
    if not (mode, algorithm, slip_system) in GENERATED_DATA:
        os.chdir(TEST_DATA)
        os.system(TEST_ROOT/f'../VEF/release/bin/alamDMC {mode} test.cfg > alamDMC.log')

        if mode == 'UDSA':
            with open('out.uds', 'w') as out:
                for orientation in [0,45,90]:
                    with open(f'out_{orientation}_000.uds','r') as out_oriented:
                        out.write(out_oriented.read())

        path = TEST_DATA/f'{mode}_{algorithm}_{slip_system}'
        out_path = str(path) + '.out'
        log_path = str(path) + '.log'
        shutil.move(TEST_DATA/f'out.{EXTENSIONS[mode]}', out_path)
        shutil.move(TEST_DATA/'alamDMC.log', log_path)
        GENERATED_DATA.append((mode, algorithm, slip_system))
        if request.config.getoption('--update') != 'FALSE':
            ref_path = TEST_ROOT/'data/out/'
            shutil.copy(out_path, ref_path)
            shutil.copy(log_path, ref_path)
    

def process_file(path):
    df = pd.read_csv(path,delimiter=' +', engine='python')
    pattern = re.compile(r'^-*0\.[0-9]+E[+\-][0-9]+$')
    res = []
    for index, row in df.iterrows():
        for num in row:
            if isinstance(num, str) and pattern.match(num):
                formatted = num.split('E')
                if int(formatted[1]) > -9:
                    res.append(int(formatted[0].replace('-','')[2:8]))
    return res


#Generate and execute the different test cases.
@pytest.mark.integration
@pytest.mark.parametrize('mode',['ADP', 'ASR', 'EWC','QRS','UDSA','YLD'])
@pytest.mark.parametrize('algorithm',['ALAMEL', 'FCTaylor'])
@pytest.mark.parametrize('slip_system',['fcc12','bcc24','bcc48'])
def test_vef(mode, algorithm, slip_system, request, generate_output):
    reference = process_file(TEST_ROOT/f'data/out/{mode}_{algorithm}_{slip_system}.out')
    result = process_file(TEST_DATA/f'{mode}_{algorithm}_{slip_system}.out')
    assert reference == result


def get_trace_values(path, module, function):
    vals = []
    header = "TRACE " + module + ", " + function
    with open(path) as log_file:
        for line in log_file:
            if header in line:
                vals.append(re.findall(r'-?[0-9]+\.?[0-9]+ *$', line)[0].replace(' ','').replace('-','').replace('.','')[0:6])
    return vals
        
     
@pytest.mark.altayAlgorithms_eigenv
@pytest.mark.parametrize('mode',['ADP', 'ASR', 'EWC','QRS','UDSA','YLD'])
@pytest.mark.parametrize('algorithm',['ALAMEL', 'FCTaylor'])
@pytest.mark.parametrize('slip_system',['fcc12','bcc24','bcc48'])
def test_altayAlgorithms_eigenv(mode, algorithm, slip_system, request, generate_output):
    reference = get_trace_values(TEST_ROOT/f'data/out/{mode}_{algorithm}_{slip_system}.log', 'altayAlgorithms', 'eigenv')
    data = get_trace_values(TEST_DATA/f'{mode}_{algorithm}_{slip_system}.log', 'altayAlgorithms', 'eigenv')
    assert data == reference







