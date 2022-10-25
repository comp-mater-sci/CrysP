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
TEST_ROOT=Path.cwd()


#General configuration
#shutil.copy(TEST_ROOT/'../fopt/src/nlls_tr.f90', TEST_ROOT)
#shutil.copy(TEST_ROOT/'conf/nlls_tr.f90', TEST_ROOT/'../fopt/src')
#os.chdir(TEST_ROOT/'..')
#os.system('./build.sh')
#shutil.move(TEST_ROOT/'nlls_tr.f90', TEST_ROOT/'../fopt/src/nlls_tr.f90')

#Set up file structure for benchmark execution. May be removed when we get rid of file I/O for the simulations.
def setup_benchmark(mode, algorithm, slip_system, test_path):
    if (mode == 'YLD' or mode == 'QRS'):
        shutil.copy(TEST_ROOT/'data/in/texture_yld.smt', test_path/'texture.smt')
    else:
        shutil.copy(TEST_ROOT/'data/in/sid1687f_short.smt', test_path/'texture.smt')
    shutil.copy(TEST_ROOT/'../VEF/data/equiaxed.smt', test_path/'equiaxed.smt')
    shutil.copy(TEST_ROOT/f'../VEF/data/{slip_system}.pre', test_path/f'{slip_system}.pre')
    create_conf_file(mode, algorithm, slip_system, test_path)
    if (mode == 'EWC' or mode == 'ASR'):
        shutil.copyfile(TEST_ROOT/f'data/in/{mode}.rtdb', test_path/'out.rtdb')
    elif (mode == 'UDSA' or mode == 'YLD'):
        shutil.copyfile(TEST_ROOT/f'data/in/UDSA_YLD.rtdb', test_path/'out.rtdb')



#Generate configuration file based on global settings and mode-specific ones.
def create_conf_file(mode, algorithm, slip_system, test_path):
    with open(test_path/'test.cfg', 'w') as conf_file, \
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


def concat_udsa_output():
    with open('out.uds', 'w') as out:
        for orientation in [0,45,90]:
            with open(f'out_{orientation}_000.uds','r') as out_oriented:
                out.write(out_oriented.read())


#Execute simulations themselves. Implemented as a dedicated function to simplify test adjustments when transitioning to a different software architecture.
def generate_output(mode):
    os.system(TEST_ROOT/f'../VEF/release/bin/alamDMC {mode} test.cfg > alamDMC.log' )

    if mode == 'UDSA':
        concat_udsa_output()


def process_file(path):
    df = pd.read_csv(path,delimiter=' +', engine='python')
    pattern = re.compile(r'^-*0\.[0-9]+E[+\-][0-9]+$')
    res = []
    for index, row in df.iterrows():
        for num in row:
            if isinstance(num, str) and pattern.match(num):
                formatted = num.split('E')
                if int(formatted[1]) > -9:
                    val = int(formatted[0].replace('-','')[2:8])
                    if val > 0:
                        res.append(val)
    return res


def process_output(mode, algorithm, slip_system):
    reference = process_file(TEST_ROOT/f'data/out/{mode}_{algorithm}_{slip_system}.out')
    result = process_file('out.' + EXTENSIONS[mode])
    return (reference, result)


def replace_reference(tmp_path, mode, algorithm, slip_system):
    if mode == 'UDSA':
        concat_udsa_output()
    shutil.copy(tmp_path/f'out.{EXTENSIONS[mode]}', TEST_ROOT/f'data/out/{mode}_{algorithm}_{slip_system}.out')



#Generate and execute the different test cases.
@pytest.mark.parametrize('mode',['ADP', 'ASR', 'EWC','QRS','UDSA','YLD'])
@pytest.mark.parametrize('algorithm',['ALAMEL', 'FCTaylor'])
@pytest.mark.parametrize('slip_system',['fcc12','bcc24','bcc48'])
def test_vef(mode, algorithm, slip_system, tmp_path, request):
    setup_benchmark(mode, algorithm, slip_system, tmp_path)
    os.chdir(tmp_path)
    generate_output(mode)
    if request.config.getoption('--update') != 'FALSE':
        replace_reference(tmp_path, mode, algorithm, slip_system)
    else:
        (reference, result) = process_output(mode, algorithm, slip_system)
        ratio_lens = len(reference) / len(result)
        #Small deviations in length are possible because we rejecct very small numbers
        assert ratio_lens > 0.95 and ratio_lens < 1.05
        ratio_vals = (sum(reference) / len(reference)) / (sum(result) / len(result))
        assert ratio_vals > 0.93 and ratio_vals < 1.07
