#Test bench for VEF software
#Runs a large number of configurations using a dummy data set and compares the hashes of the output data to predetermined hash values.
#USAGE: Run 'pytest' in the 'test' directory of the project. The tests are parametrized over mode, algorithm and slip system. If you want to run e.g. only the tests pertaining to ADP, run "pytest -k ADP"
#REQUIRES: Python, pytest

import os
import hashlib
import shutil
from pathlib import Path

import pytest

#Predetermined output hashes
HASHES = {'ADP': {'ALAMEL':{'fcc12':'2e114a9f21e6c1447caa1c065da7becc',
                            'bcc24':'5ded6ebd05fb3f95b59f168a12f07f11',
                            'bcc48':'eafd5736d8046465d086fc98a09d174e'},
                  'FCTaylor':{'fcc12':'d3bd4f94f0397c830c841a6d7b0eab53',
                              'bcc24':'a869ed87b3d925193dd6fee62004e230',
                              'bcc48':'815be3efeefd9a735b1f165decb0216d'}},
          'ASR': {'ALAMEL':{'fcc12':'3a85f59feeaf9e0ac2d077a3d5e16d1d',
                            'bcc24':'4e6248723d17d10433dc08ae0062126c',
                            'bcc48':'a6bf075ced02a1f401eb6a00400cddb2'},
                 'FCTaylor':{'fcc12':'af75369c40118e26cc5188db3c6a4e9a',
                             'bcc24':'8c989e6e8cdcb208ba2557a1fd97be63',
                             'bcc48':'34a9b1c4c10e65a42f218c607fe7b9d2'}},
          'EWC': {'ALAMEL':{'fcc12':'531ce0cfb3268bb991639e4593aab114',
                            'bcc24':'f3bb187560838c54d3b805a83ed31e4b',
                            'bcc48':'a3805e4b66bc340f8c7b9a85ef892ca5'},
                  'FCTaylor':{'fcc12':'7c29f53b10f651d25b88fad8eb737fa7',
                              'bcc24':'b325a689314d2cfd6cb7872ad7af8bed',
                              'bcc48':'d618c46526608c0c532ce5fe5d86c082'}},
          'QRS': {'ALAMEL':{'fcc12':'612181898c215dac586a0f8c68e8251f',
                            'bcc24':'9b38997de6c590c32223e17525ef0ad8',
                            'bcc48':'733aac7805ac3b0bdecf39c5a27f7a84'},
                  'FCTaylor':{'fcc12':'612181898c215dac586a0f8c68e8251f',
                              'bcc24':'be10a0afe73060dc3a6f048e9e58e02f',
                              'bcc48':'fa5d02ece85d10f9eff939f6c878265c'}},
          'UDSA': {'ALAMEL':{'fcc12':'7dfe706c5d73e1e120592b14a54bb436',
                             'bcc24':'9ca59b57e8127e21e36b106d61b87e4a',
                             'bcc48':'49a91f993350e49646a4c6f81298314c'},
                   'FCTaylor':{'fcc12':'70f20cfee41a4f81d583cdcd5046da88',
                               'bcc24':'cf8bc2845fa1c3f2eba5d3e9340e5dcc',
                               'bcc48':'049185a78be4e9a5a460fd5379196182'}},
          'YLD': {'ALAMEL':{'fcc12':'8ff0fdce0e4eff616ffa9ff5b86273d4',
                            'bcc24':'c6888ba3659a06e0b98514316bc5c79b',
                            'bcc48':'6b812302a270b4c8ad7118ec95b0e5c8'},
                  'FCTaylor':{'fcc12':'64becdc0c6abb96b9d53ff23e843c2d5',
                              'bcc24':'989b2a75622ab228f92944c456ad2e91',
                              'bcc48':'2c5678b27b776f493d65d82f1610b05b'}}}
#File extension for the output of each execution mode. May be removed when we get rid of the file-based I/O for the simulations.
EXTENSIONS = {'ADP':'adp','ASR':'asr', 'EWC':'ewc', 'QRS':'xqrs', 'UDSA':'uds','YLD':'xyld'}
TEST_ROOT=Path.cwd()


#Set up file structure for benchmark execution. May be removed when we get rid of file I/O for the simulations.
def setup_benchmark(mode, algorithm, slip_system, test_path):
    if (mode == 'YLD'):
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


#Execute simulations themselves. Implemented as a dedicated function to simplify test adjustments when transitioning to a different software architecture.
def generate_output(mode):
    os.system(TEST_ROOT/f'../VEF/release/bin/alamDMC {mode} test.cfg')

    if mode == 'UDSA':
        with open('out.uds', 'w') as out:
            for orientation in [0,45,90]:
                with open(f'out_{orientation}_000.uds','r') as out_oriented:
                    out.write(out_oriented.read())


#Calculate the MD5 hash of the test output.
def calc_hash(mode):
    path = 'out.' + EXTENSIONS[mode]
    hasher = hashlib.md5()
    result = open(path,'rb').read()
    hasher.update(result)
    return hasher.hexdigest()


#Generate and execute the different test cases.
@pytest.mark.parametrize('mode',['ADP', 'ASR', 'EWC','QRS','UDSA','YLD'])
@pytest.mark.parametrize('algorithm',['ALAMEL', 'FCTaylor'])
@pytest.mark.parametrize('slip_system',['fcc12','bcc24','bcc48'])
def test_vef(mode, algorithm, slip_system, tmp_path):
    setup_benchmark(mode, algorithm, slip_system, tmp_path)
    os.chdir(tmp_path)
    generate_output(mode)
    test_hash = calc_hash(mode)
    assert test_hash == HASHES[mode][algorithm][slip_system]
