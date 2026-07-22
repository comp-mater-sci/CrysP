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

GENERATED_DATA = []
TEST_ROOT = Path.cwd()
TEST_RUN = TEST_ROOT/'run'
TEST_REF = TEST_ROOT/'reference'
TEST_INPUT = TEST_ROOT/'input'

#Configurations that can be tested
MODES = ['ADP', 'ASR', 'QRS','YLD']
ALGORITHMS = ['ALAMEL', 'FCTaylor']
SLIP_SYSTEMS = ['fcc12','bcc24','bcc48']
HARDENING_MODELS = ['NONE', 'VOCE', 'HOCKETT_SHERBY', 'SWIFT', 'DSH_EDGE', 'DSH_SCREW', 'DSH_LOOP']

#Unit tests
UNITS = [('altayAlgorithms','eigenv'),      \
         ('altayAlgorithms','normaliz'),    \
         ('altayAlgorithms','canoni')]

#General setup
if not os.path.exists(TEST_RUN):
    os.mkdir(TEST_RUN)

#Execute simulations themselves. Implemented as a dedicated function to simplify test adjustments when transitioning to a different software architecture.
def generate_output(update, mode, algorithm='ALAMEL', slip_system='bcc24', hardening_model='NONE'):
    with open(TEST_RUN/'test.cfg', 'w') as conf_file:
        conf_file.write(conf.generate_config(mode, algorithm, slip_system, hardening_model))

    try:
        os.remove(TEST_RUN/'test.rtdb')
        os.remove(TEST_RUN/'test.CUR')
    except: FileNotFoundError

    shutil.copy(TEST_INPUT/'texture.smt', TEST_RUN)
    shutil.copy(TEST_INPUT/'equiaxed.smt', TEST_RUN)
    if hardening_model == 'DSH_EDGE' or hardening_model == 'DSH_SCREW' or hardening_model == 'DSH_LOOP':
        with open(TEST_RUN/'DSHparaset.txt','w') as dsh_config:
            dsh_config.write(conf.DSH_CONFIG)

    if not (mode, algorithm, slip_system, hardening_model) in GENERATED_DATA:
        path = TEST_RUN/f'{mode}_{algorithm}_{slip_system}_{hardening_model}'
        out_path = str(path) + '.out'
        log_path = str(path) + '.log'

        os.chdir(TEST_RUN)
        result = subprocess.run([TEST_ROOT/'../crysp-cli/bin/crysp',mode,'test.cfg'],
                                stdout=subprocess.PIPE,stderr=subprocess.PIPE)
        with open(log_path,'w') as f:
            f.write(result.stdout.decode())
            f.write(result.stderr.decode())
        if result.returncode != 0:
            print(result.stderr.decode())
        assert result.returncode == 0

        shutil.move(TEST_RUN/'test_out.csv', out_path)
        GENERATED_DATA.append((mode, algorithm, slip_system, hardening_model))
        TEST_REF.mkdir(parents=True, exist_ok=True)
        if 'log' in update: shutil.copy(log_path, TEST_REF)
        if 'out' in update: shutil.copy(out_path, TEST_REF)


tests_no_hardening = itertools.product(MODES, ALGORITHMS, SLIP_SYSTEMS, ['NONE'])
tests_hardening = itertools.product(MODES[0:2], ALGORITHMS, SLIP_SYSTEMS, HARDENING_MODELS[1:4])
tests_bp = itertools.product(MODES[0:2], ALGORITHMS, ['bcc24'], HARDENING_MODELS[4:7])
tests = list(tests_no_hardening) + list(tests_hardening) + list(tests_bp)

#Generate and execute the different test cases.
@pytest.mark.integration
@pytest.mark.parametrize('mode,algorithm,slip_system,hardening_model', tests)
def test_vef(mode, algorithm, slip_system, hardening_model, update, margin):

    generate_output(update, mode, algorithm, slip_system, hardening_model)

    if update == 'FALSE' :
        filename =  f'{mode}_{algorithm}_{slip_system}_{hardening_model}.out'
        ref = pd.read_csv(TEST_REF/filename, engine='python')
        res = pd.read_csv(TEST_RUN/filename, engine='python')
        cols = []

        #Generate formatted dataframe from selected columns of output files based on mode
        if mode == 'ADP':
            cols = ['S_11','S_22','S_33','S_12','S_23','S_13']
        elif mode == 'ASR':
            cols = ['A_11', 'A_22', 'A_33', 'A_23', 'A_13', 'A_12']
        elif mode == 'YLD':
            cols = ['stress']
        elif mode == 'QRS':
            cols = ['q-value', 'r-value', 's-value']

        ref = (ref.astype(float).reset_index())[cols]
        res = (res.astype(float).reset_index())[cols]

        #Compare results to reference
        sensitivity = float(margin) / 100.0
        if mode == 'ADP' or mode == 'ASR':
            #None of the components should vary more from the reference than MARGIN times the max. component
            for index, row in res.iterrows():

                row_ref = ref.iloc[index]
                tolerance = max(abs(row_ref)) * sensitivity

                for i in range(len(row)):
                    element = row[i]
                    element_ref = row_ref[i]
                    assert element_ref - tolerance <= element <= element_ref + tolerance
        else:
            #None of the components should vary more than MARGIN from the reference
            for index, row in res.iterrows():
                row_ref = ref.iloc[index]
                for i in range(len(row)):
                    element = row[i]
                    element_ref = row_ref[i]
                    if not math.isnan(element_ref):
                        assert abs(element_ref * (1 - sensitivity)) <= abs(element) <= abs(element_ref * (1 + sensitivity))

