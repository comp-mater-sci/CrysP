#Test bench for VEF software
#Runs a large number of configurations using a dummy data set and compares the hashes of the output data to predetermined hash values.
#USAGE: Run 'pytest' in the 'test' directory of the project. The tests are parametrized over mode, algorithm and slip system. If you want to run e.g. only the tests pertaining to ADP, run "pytest -k ADP"
#REQUIRES: Python, pytest

import os
import hashlib
import shutil

import pytest

#Predetermined output hashes
HASHES = {'ADP': {'ALAMEL':{'fcc12':'2e114a9f21e6c1447caa1c065da7becc',
                            'bcc24':'5ded6ebd05fb3f95b59f168a12f07f11',
                            'bcc48':'eafd5736d8046465d086fc98a09d174e'},
                  'FCTaylor':{'fcc12':'d3bd4f94f0397c830c841a6d7b0eab53',
                              'bcc24':'a869ed87b3d925193dd6fee62004e230',
                              'bcc48':'815be3efeefd9a735b1f165decb0216d'}},
          'ASR': {'ALAMEL':{'fcc12':'22e163ef1b2701b3e7a94b53ab7ba6a6',
                            'bcc24':'22e163ef1b2701b3e7a94b53ab7ba6a6',
                            'bcc48':'22e163ef1b2701b3e7a94b53ab7ba6a6'},
                 'FCTaylor':{'fcc12':'22e163ef1b2701b3e7a94b53ab7ba6a6',
                             'bcc24':'22e163ef1b2701b3e7a94b53ab7ba6a6',
                             'bcc48':'2646017b760760038f3369827c47a052'}},
          'EWC': {'ALAMEL':{'fcc12':'7b2055cba222ed1a95d41bade17eef08',
                            'bcc24':'9ad4316eecf3a4d24792a71e1513fc9d',
                            'bcc48':'0ae07d5bafd59578233696d6e0ebc2ae'},
                  'FCTaylor':{'fcc12':'076ec32666143bb5765c0eb7b6fd53d0',
                              'bcc24':'8a24f588d8d7c5e814da15aad2c22483',
                              'bcc48':'a9afac2d0299529614aece7707c59b8f'}},
          'QRS': {'ALAMEL':{'fcc12':'612181898c215dac586a0f8c68e8251f',
                            'bcc24':'9b38997de6c590c32223e17525ef0ad8',
                            'bcc48':'733aac7805ac3b0bdecf39c5a27f7a84'},
                  'FCTaylor':{'fcc12':'612181898c215dac586a0f8c68e8251f',
                              'bcc24':'be10a0afe73060dc3a6f048e9e58e02f',
                              'bcc48':'fa5d02ece85d10f9eff939f6c878265c'}},
          'UDSA': {'ALAMEL':{'fcc12':'e4095d33be7f9d1095224441a061a5ba',
                             'bcc24':'e4095d33be7f9d1095224441a061a5ba',
                             'bcc48':'e4095d33be7f9d1095224441a061a5ba'},
                   'FCTaylor':{'fcc12':'e4095d33be7f9d1095224441a061a5ba',
                               'bcc24':'e4095d33be7f9d1095224441a061a5ba',
                               'bcc48':'e4095d33be7f9d1095224441a061a5ba'}},
          'YLD': {'ALAMEL':{'fcc12':'d41d8cd98f00b204e9800998ecf8427e',
                            'bcc24':'d41d8cd98f00b204e9800998ecf8427e',
                            'bcc48':'d41d8cd98f00b204e9800998ecf8427e'},
                  'FCTaylor':{'fcc12':'d41d8cd98f00b204e9800998ecf8427e',
                              'bcc24':'d41d8cd98f00b204e9800998ecf8427e',
                              'bcc48':'d41d8cd98f00b204e9800998ecf8427e'}}}
#File extension for the output of each execution mode. May be removed when we get rid of the file-based I/O for the simulations.
EXTENSIONS = {'ADP':'adp','ASR':'asr', 'EWC':'ewc', 'QRS':'xqrs', 'UDSA':'uds','YLD':'xyld'}
TEST_ROOT = os.getcwd()

#Basic configuration and cleanup
@pytest.fixture
def setup():
    os.mkdir(TEST_ROOT + '/run')
    shutil.copy(TEST_ROOT + '/data/in/sid1687f_short.smt', TEST_ROOT + '/run/texture.smt')
    os.environ["VEF_HOME"] = TEST_ROOT + '../VEF'
    yield
    shutil.rmtree(TEST_ROOT + '/run')

#Set up file structure for benchmark execution. May be removed when we get rid of file I/O for the simulations.
def setup_benchmark(mode, algorithm, slip_system):
    create_conf_file(mode, algorithm, slip_system)
    if (mode == 'EWC'):
        shutil.copyfile(TEST_ROOT + '/data/in/out.rtdb', TEST_ROOT + '/run/out.rtdb')


#Generate configuration file based on global settings and mode-specific ones.
def create_conf_file(mode, algorithm, slip_system):
    with open(TEST_ROOT + '/run/test.cfg', 'w') as conf_file, \
         open(TEST_ROOT + '/conf/' + mode + '.cfg','r') as mode_specific_conf_file:
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
    os.system('export VEF_ROOT=' + TEST_ROOT + '/../VEF && cd ' + TEST_ROOT + '/run && ../../VEF/release/bin/alamDMC ' + mode + ' test.cfg')

#Calculate the MD5 hash of the test output.
def calc_hash(mode):
    if mode == 'UDSA':
        path = TEST_ROOT + '/run/out_0_000.uds'
    else:
        path = TEST_ROOT + "/run/out." + EXTENSIONS[mode]

    hasher = hashlib.md5()
    result = open(path,'rb').read()
    hasher.update(result)
    return hasher.hexdigest()


#Generate and execute the different test cases.
@pytest.mark.parametrize('mode',['ADP', 'ASR', 'EWC','QRS','UDSA','YLD'])
@pytest.mark.parametrize('algorithm',['ALAMEL', 'FCTaylor'])
@pytest.mark.parametrize('slip_system',['fcc12','bcc24','bcc48'])
def test_vef(setup, mode, algorithm,slip_system):
    setup_benchmark(mode, algorithm, slip_system)
    generate_output(mode)
    test_hash = calc_hash(mode)
    assert test_hash == HASHES[mode][algorithm][slip_system]
